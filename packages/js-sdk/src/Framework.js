const { id } = require("@ethersproject/hash");

const loadContracts = require("./loadContracts");
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
     * @param {string} options.version (Default: v1) protocol release version.
     * @param {boolean} options.isTruffle (Default: false) if the framework is used within truffle environment.
     * @param {Web3} options.web3  Injected web3 instance (version has to be 1.x)
     * @param {Ethers} options.ethers  Injected ethers instance
     *
     * @param {Array} options.additionalContracts (Optional) additional contracts to be loaded
     * @param {string[]} options.tokens (Optional) Tokens to be loaded with a list of (in order of preference):
     *    - super chain-native token symbol (see getConfig.js),
     *    - underlying token resolver key (tokens.{KEY}),
     *    - super token key  (supertokens.{protocol_release_version}.{KEY})
     * @param {bool} options.loadSuperNativeToken Load super native token (e.g. ETHx) if possible
     * @param {Function} options.contractLoader (Optional) alternative contract loader function
     *
     * @param {string} options.resolverAddress force resolver address
     * @param {string} options.gasReportType output type for gas reporting. Currently HTML only
     * @return {Framework} The Framework object
     *
     * NOTE: You should call async function Framework.initialize to initialize the object.
     */
    constructor(options) {
        this._options = options;
        this.version = options.version || "v1";

        if (options.isTruffle && (options.ethers || options.web3))
            throw Error(
                "@superfluid-finaince/js-sdk: Flag 'isTruffle' cannot be 'true' when using a web3/ethers instance."
            );
        if (!options.isTruffle && !options.ethers && !options.web3)
            throw Error(
                "@superfluid-finaince/js-sdk: You must provide a web3 or ethers instance."
            );
        if (options.ethers && options.web3)
            throw Error(
                `@superfluid-finaince/js-sdk: You cannot provide both a web3 and ethers instance.
                Please choose only one.`
            );
        this.web3 = options.isTruffle ? global.web3 : options.web3;
        this.ethers = options.ethers;

        if (options.gasReportType) {
            if (
                options.gasReportType !== "HTML" &&
                options.gasReportType !== "JSON"
            ) {
                throw new Error(
                    "Unsuported gas report type: " + options.gasReportType
                );
            }
            console.debug("Enabling gas report type:", options.gasReportType);
            this._gasReportType = options.gasReportType;
        }
    }

    /**
     * @dev Initialize the framework object
     * @return {Promise}
     */
    async initialize() {
        console.log("Initializing Superfluid Framework...");
        let networkType;
        let chainId;
        if (this.ethers) {
            const network = await this.ethers.getNetwork();
            networkType = network.name;
            chainId = network.chainId;
        } else {
            // NOTE: querying network type first,
            // Somehow web3.eth.net.getId may send bogus number if this was not done first
            // It could be a red-herring issue, but it makes it more stable.
            networkType = await this.web3.eth.net.getNetworkType();
            chainId = await this.web3.eth.net.getId(); // TODO use eth.getChainId;
        }
        console.log("networkType", networkType);
        console.log("chainId", chainId);

        this.config = getConfig(chainId);

        this.contracts = await loadContracts({
            isTruffle: this._options.isTruffle,
            web3: this._options.web3,
            ethers: this._options.ethers,
            from: this._options.from,
            additionalContracts: this._options.additionalContracts,
            contractLoader: this._options.contractLoader,
        });

        const resolverAddress =
            this._options.resolverAddress || this.config.resolverAddress;
        console.debug("Resolver at", resolverAddress);
        this.resolver = await this.contracts.IResolver.at(resolverAddress);

        // load superfluid host contract
        console.debug("Resolving contracts with version", this.version);
        const superfluidAddress = await this.resolver.get(
            `Superfluid.${this.version}`
        );
        this.host = await this.contracts.ISuperfluid.at(superfluidAddress);
        console.debug(
            "Superfluid host contract: TruffleContract .host",
            superfluidAddress
        );

        this.agreements = {};
        this.tokens = {};
        this.superTokens = {};

        // load agreement classes
        [this.agreements.cfa, this.agreements.ida] = await Promise.all([
            // load agreements
            ...[
                [
                    id(
                        "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                    ),
                    this.contracts.IConstantFlowAgreementV1,
                ],
                [
                    id(
                        "org.superfluid-finance.agreements.InstantDistributionAgreement.v1"
                    ),
                    this.contracts.IInstantDistributionAgreementV1,
                ],
            ].map(async (data) => {
                const address = await this.host.getAgreementClass(data[0]);
                return await data[1].at(address);
            }),
            // load tokens
            ...[
                ...(this._options.tokens ? this._options.tokens : []),
                ...(this._options.loadSuperNativeToken &&
                this.config.nativeTokenSymbol
                    ? [this.config.nativeTokenSymbol]
                    : []),
            ].map(this.loadToken.bind(this)),
        ]);

        // load agreement helpers
        this.cfa = new (require("./ConstantFlowAgreementV1Helper"))(this);
        this.ida = new (require("./InstantDistributionAgreementV1Helper"))(
            this
        );
        console.debug(
            "ConstantFlowAgreementV1: TruffleContract .agreements.cfa | Helper .cfa",
            this.agreements.cfa.address
        );
        console.debug(
            "InstantDistributionAgreementV1: TruffleContract .agreements.ida | Helper .ida",
            this.agreements.ida.address
        );

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
        console.log("Superfluid Framework initialized.");
    }

    /**
     * @dev Load additional token using resolver
     * @param {String} superTokenKey super token key used to query resolver
     */
    async isSuperTokenListed(superTokenKey) {
        const superTokenAddress = await this.resolver.get(
            `supertokens.${this.version}.${superTokenKey}`
        );
        return superTokenAddress !== ZERO_ADDRESS;
    }

    /**
     * @dev Load additional token using resolver
     * @param {String} tokenKey token key used to query resolver (in order of preference):
     *    - super chain-native token symbol (see getConfig.js),
     *    - underlying token resolver key (tokens.{KEY}),
     *    - super token key  (supertokens.{protocol_release_version}.{KEY})
     */
    async loadToken(tokenKey) {
        // load underlying token
        //  but we don't need to load native tokens
        let underlyingToken;
        let checkUnderlyingToken;
        let superTokenKey;
        if (tokenKey !== this.config.nativeTokenSymbol) {
            const tokenAddress = await this.resolver.get(`tokens.${tokenKey}`);
            if (tokenAddress !== ZERO_ADDRESS) {
                underlyingToken = await this.contracts.ERC20WithTokenInfo.at(
                    tokenAddress
                );
                this.tokens[tokenKey] = underlyingToken;
                console.debug(
                    `${tokenKey}: ERC20WithTokenInfo .tokens["${tokenKey}"]`,
                    tokenAddress
                );
                superTokenKey = tokenKey + "x";
            } else {
                superTokenKey = tokenKey;
            }
            checkUnderlyingToken = true;
        } else {
            superTokenKey = this.config.nativeTokenSymbol + "x";
        }

        // load super token
        const superTokenAddress = await this.resolver.get(
            `supertokens.${this.version}.${superTokenKey}`
        );
        if (superTokenAddress === ZERO_ADDRESS) {
            throw new Error(`Super Token for ${tokenKey} cannot be found`);
        }
        let superToken;
        if (tokenKey !== this.config.nativeTokenSymbol) {
            superToken = await this.contracts.ISuperToken.at(superTokenAddress);
        } else {
            superToken = await this.contracts.ISETH.at(superTokenAddress);
        }
        this.tokens[superTokenKey] = superToken;
        this.superTokens[superTokenKey] = superToken;

        if (checkUnderlyingToken) {
            const underlyingTokenAddress = await superToken.getUnderlyingToken.call();
            if (underlyingTokenAddress !== ZERO_ADDRESS) {
                // if underlying token is not undefined and not equal to getUnderlyingToken() returned address
                if (
                    underlyingToken &&
                    underlyingTokenAddress.toLowerCase() !==
                        underlyingToken.address.toLowerCase()
                ) {
                    throw new Error(
                        `Underlying token address are different for ${tokenKey}`
                    );
                }

                // if underlying token is null or undefined
                if (!underlyingToken) {
                    underlyingToken = await this.contracts.ERC20WithTokenInfo.at(
                        underlyingTokenAddress
                    );
                    const symbol = await underlyingToken.symbol();
                    this.tokens[symbol] = underlyingToken;
                }
            } else {
                if (underlyingToken) {
                    throw new Error(
                        `Unexpected underlying token for ${tokenKey}`
                    );
                }
            }
        }
        superToken.underlyingToken = underlyingToken;

        console.debug(
            `${superTokenKey}: ISuperToken .tokens["${superTokenKey}"]`,
            superTokenAddress
        );
    }

    /**
     * @dev Create the ERC20 wrapper from underlying token
     * @param {Any} tokenInfo the TokenInfo contract object to the underlying token
     * @param {string} superTokenName (optional) overriding superTokenName
     * @param {string} superTokenSymbol (optional) overriding superTokenSymbol
     * @param {address} from (optional) send transaction from
     * @param {address} upgradability (optional) send transaction from
     * @return {Promise<Transaction>} web3 transaction object
     */
    async createERC20Wrapper(
        tokenInfo,
        { superTokenSymbol, superTokenName, from, upgradability } = {}
    ) {
        const tokenName = await tokenInfo.name.call();
        const tokenSymbol = await tokenInfo.symbol.call();
        superTokenName = superTokenName || `Super ${tokenName}`;
        superTokenSymbol = superTokenSymbol || `${tokenSymbol}x`;
        const factory = await this.contracts.ISuperTokenFactory.at(
            await this.host.getSuperTokenFactory()
        );
        upgradability =
            typeof upgradability === "undefined" ? 1 : upgradability;
        const tx = await factory.createERC20Wrapper(
            tokenInfo.address,
            upgradability,
            superTokenName,
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
        const superToken = await this.contracts.ISuperToken.at(wrapperAddress);
        superToken.tx = tx;
        return superToken;
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
};
