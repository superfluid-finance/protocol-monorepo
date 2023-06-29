const loadContracts = require("./loadContracts");
const getConfig = require("./getConfig");

/**
 * @dev Superfluid Framework class
 */
module.exports = class Framework {
    /**
     * @dev Create new Superfluid framework object
     * @param {string} options.version (Default: v1) protocol release version.
     * @param {boolean} options.isTruffle (Default: false) if the framework is used within truffle environment.
     * @param {Web3} options.web3  Injected web3 instance (version has to be 1.x)
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

        if (options.isTruffle && options.web3)
            throw Error(
                "@superfluid-finance/js-sdk: Flag 'isTruffle' cannot be 'true' when using a web3 instance."
            );
        if (!options.isTruffle && !options.web3)
            throw Error(
                "@superfluid-finance/js-sdk: You must provide a web3 instance."
            );

        this.web3 = options.isTruffle ? global.web3 : options.web3;

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
        // NOTE: querying network type first,
        // Somehow web3.eth.net.getId may send bogus number if this was not done first
        // It could be a red-herring issue, but it makes it more stable.
        this.networkType = await this.web3.eth.net.getNetworkType();
        this.networkId = await this.web3.eth.net.getId();
        this.chainId = await this.web3.eth.getChainId();

        console.log("version", this.version);
        console.log("networkType", this.networkType);
        console.log("networkId", this.networkId);
        console.log("chainId", this.chainId);

        this.config = getConfig(this.chainId, this.version);

        this.contracts = await loadContracts({
            isTruffle: this._options.isTruffle,
            web3: this._options.web3,
            from: this._options.from,
            additionalContracts: this._options.additionalContracts,
            contractLoader: this._options.contractLoader,
            networkId: this.networkId,
        });

        const resolverAddress =
            this._options.resolverAddress || this.config.resolverAddress;
        console.debug("Resolver at", resolverAddress);
        this.resolver = await this.contracts.IResolver.at(resolverAddress);

        // get framework loader and load
        this.loader = await this.contracts.SuperfluidLoader.at(
            await this.resolver.get("SuperfluidLoader-v1")
        );
        console.debug("Superfluid Loader v1", this.loader.address);
        console.debug("Loading framework with release version", this.version);
        const loaderResult = await this.loader.loadFramework(this.version);

        console.debug(
            "Superfluid host contract: TruffleContract .host",
            loaderResult.superfluid
        );
        console.debug(
            "SuperTokenFactory address:",
            loaderResult.superTokenFactory
        );
        console.debug(
            "ConstantFlowAgreementV1: TruffleContract .agreements.cfa | Helper .cfa",
            loaderResult.agreementCFAv1
        );
        console.debug(
            "InstantDistributionAgreementV1: TruffleContract .agreements.ida | Helper .ida",
            loaderResult.agreementIDAv1
        );

        this.agreements = {};
        this.tokens = {};
        this.superTokens = {};

        // load agreement classes
        [this.host, this.agreements.cfa, this.agreements.ida] =
            await Promise.all([
                // load host
                this.contracts.ISuperfluid.at(loaderResult.superfluid),
                // load agreements
                this.contracts.IConstantFlowAgreementV1.at(
                    loaderResult.agreementCFAv1
                ),
                this.contracts.IInstantDistributionAgreementV1.at(
                    loaderResult.agreementIDAv1
                ),
            ]);
        console.log("Superfluid Framework initialized.");
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
        {superTokenSymbol, superTokenName, from, upgradability} = {}
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
            ...((from && [{from}]) || []) // don't mind this silly js stuff, thanks to web3.js
        );
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
};
