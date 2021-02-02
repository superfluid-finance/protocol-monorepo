const Web3 = require("web3");

const loadContracts = require("./utils/loadContracts");

const getConfig = require("./getConfig");
const GasMeter = require("./utils/gasMetering/gasMetering");
const { getErrorResponse } = require("./utils/error");
const { validateAddress } = require("./utils/general");
const User = require("./User");

const ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";

/**
 * @dev Superfluid Framework class
 */
module.exports = class Framework {
    /**
     * @dev Create new Superfluid framework object
     * @param {Web3.Provider} web3Provider web3 provider object
     * @param {boolean} isTruffle if the framework is used within truffle environment
     * @param {string} version protocol contract version
     * @param {string} chainId force chainId, instead relying on web3.eth.net.getId
     * @param {string} resolverAddress force resolver address
     * @param {string[]} tokens the tokens to be loaded, each element is an alias for the underlying token
     * @param {string} gasReportType (optional) output type for gas reporting. Currently HTML only
     * @return {Framework} The Framework object
     *
     * NOTE: You should call async function Framework.initialize to initialize the object.
     */
    constructor({
        web3Provider,
        isTruffle,
        version,
        chainId,
        resolverAddress,
        tokens,
        gasReportType
    }) {
        this.chainId = chainId;
        this.version = version || "test";
        this.resolverAddress = resolverAddress;
        this.web3 = isTruffle ? global.web3 : new Web3(web3Provider);
        this._tokens = tokens;

        // load contracts
        this.contracts = loadContracts({
            isTruffle,
            web3Provider
        });
        if (gasReportType) {
            if (gasReportType !== "HTML" && gasReportType !== "JSON") {
                throw new Error("Unsuported gas report type: " + gasReportType);
            }
            console.debug("Enabling gas report type:", gasReportType);
            this._gasReportType = gasReportType;
        }
    }

    /**
     * @dev Initialize the framework object
     * @return {Promise}
     */
    async initialize() {
        // NOTE: querying network type first,
        // Somehow web3.eth.net.getId may send bogus number if this was not done first
        // It could be a red-herring issue, but it makes it more stable.
        const networkType = await this.web3.eth.net.getNetworkType();
        const chainId = this.chainId || (await this.web3.eth.net.getId()); // TODO use eth.getChainId;
        console.log("networkType", networkType);
        console.log("chainId", chainId);

        const config = getConfig(chainId);

        const resolverAddress = this.resolverAddress || config.resolverAddress;
        console.debug("Resolver at", resolverAddress);
        this.resolver = await this.contracts.IResolver.at(resolverAddress);

        // load superfluid host contract
        console.debug("Resolving contracts with version", this.version);
        const superfluidAddress = await this.resolver.get.call(
            `Superfluid.${this.version}`
        );
        this.host = await this.contracts.ISuperfluid.at(superfluidAddress);
        console.debug(
            `Superfluid host contract: TruffleContract .host @${superfluidAddress}`
        );

        // load agreements
        const cfav1Type = this.web3.utils.sha3(
            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
        );
        const idav1Type = this.web3.utils.sha3(
            "org.superfluid-finance.agreements.InstantDistributionAgreement.v1"
        );
        const cfaAddress = await this.host.getAgreementClass.call(cfav1Type);
        const idaAddress = await this.host.getAgreementClass.call(idav1Type);
        this.agreements = {
            cfa: await this.contracts.IConstantFlowAgreementV1.at(cfaAddress),
            ida: await this.contracts.IInstantDistributionAgreementV1.at(
                idaAddress
            )
        };

        // load agreement helpers
        this.cfa = new (require("./ConstantFlowAgreementV1Helper"))(this);
        this.ida = new (require("./InstantDistributionAgreementV1Helper"))(
            this
        );
        console.debug(
            `ConstantFlowAgreementV1: TruffleContract .agreements.cfa @${cfaAddress} | Helper .cfa`
        );
        console.debug(
            `InstantDistributionAgreementV1: TruffleContract .agreements.ida @${idaAddress} | Helper .ida`
        );

        // load tokens
        this.tokens = {};
        if (this._tokens) {
            for (let i = 0; i < this._tokens.length; ++i) {
                const tokenSymbol = this._tokens[i];
                const tokenAddress = await this.resolver.get(
                    `tokens.${tokenSymbol}`
                );
                if (tokenAddress === ZERO_ADDRESS) {
                    throw new Error(`Token ${tokenSymbol} is not registered`);
                }
                const superTokenAddress = await this.resolver.get(
                    `supertokens.${this.version}.${tokenSymbol}x`
                );
                if (superTokenAddress === ZERO_ADDRESS) {
                    throw new Error(
                        `Token ${tokenSymbol} doesn't have a super token wrapper`
                    );
                }
                const superToken = await this.contracts.ISuperToken.at(
                    superTokenAddress
                );
                const superTokenSymbol = await superToken.symbol();
                this.tokens[
                    tokenSymbol
                ] = await this.contracts.ERC20WithTokenInfo.at(tokenAddress);
                this.tokens[superTokenSymbol] = superToken;
                console.debug(
                    `${tokenSymbol}: ERC20WithTokenInfo .tokens["${tokenSymbol}"] @${tokenAddress}`
                );
                console.debug(
                    `${superTokenSymbol}: ISuperToken .tokens["${superTokenSymbol}"] @${superTokenAddress}`
                );
            }
        }

        this.utils = new (require("./Utils"))(this);
        if (this._gasReportType) {
            const defaultGasPrice = await this.web3.eth.getGasPrice();
            this._gasMetering = new GasMeter(
                this.web3,
                this._gasReportType,
                defaultGasPrice,
                "USD",
                "500"
            );
        }
    }

    /**
     * @dev Create the ERC20 wrapper from underlying token
     * @param {Any} tokenInfo the TokenInfo contract object to the underlying token
     * @param {address} from (optional) send transaction from
     * @param {address} upgradability (optional) send transaction from
     * @return {Promise<Transaction>} web3 transaction object
     */
    async createERC20Wrapper(tokenInfo, { from, upgradability } = {}) {
        const tokenName = await tokenInfo.name.call();
        const tokenSymbol = await tokenInfo.symbol.call();
        const superTokenSymbol = `${tokenSymbol}x`;
        const factory = await this.contracts.ISuperTokenFactory.at(
            await this.host.getSuperTokenFactory()
        );
        upgradability =
            typeof upgradability === "undefined" ? 1 : upgradability;
        const tx = await factory.createERC20Wrapper(
            tokenInfo.address,
            upgradability,
            `Super ${tokenName}`,
            superTokenSymbol,
            ...((from && [{ from }]) || []) // don't mind this silly js stuff, thanks to web3.js
        );
        this._pushTxForGasReport(tx, "createERC20Wrapper");
        const wrapperAddress = tx.logs[0].args.token;
        const u = ["Non upgradable", "Semi upgrdable", "Full upgradable"][
            upgradability
        ];
        console.log(
            `${u} super token ${superTokenSymbol} created at ${wrapperAddress}`
        );
        return this.contracts.ISuperToken.at(wrapperAddress);
    }
    /**
     * @dev call to add a tx in the gas report. Does nothing if gas report type is not set.
     * @param {tx oject} tx as returned by truffleContract action
     * @param {str} actionName action title for row in report
     */
    _pushTxForGasReport(tx, actionName) {
        this._gasMetering ? this._gasMetering.pushTx(tx, actionName) : null;
    }

    /**
     * @dev generate gas report with transactions pushed until this call
     * @param {str} name file name for gas report
     * @throws if gas report type was not indicated in constructor
     */
    generateGasReport(name) {
        if (!this._gasMetering) {
            throw new Error("No gas metering configured");
        }
        this._gasMetering.generateReport(name);
    }

    user({ address, token, options }) {
        try {
            if (!address) throw "Please provide an address";
            if (!token) throw "Please provide a token";
            validateAddress(address);
            // TODO: validate token
            return new User({ sf: this, address, token, options });
        } catch (e) {
            throw getErrorResponse(e, "Framework", "user");
        }
    }
};
