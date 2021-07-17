export = Framework;
declare class Framework {
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
    constructor(options: any);
    _options: any;
    version: any;
    web3: any;
    ethers: any;
    _gasReportType: any;
    /**
     * @dev Initialize the framework object
     * @return {Promise}
     */
    initialize(): Promise<any>;
    config: any;
    contracts: {} | undefined;
    resolver: any;
    host: any;
    agreements: {} | undefined;
    tokens: {} | undefined;
    superTokens: {} | undefined;
    cfa: ConstantFlowAgreementV1Helper | undefined;
    ida: InstantDistributionAgreementV1Helper | undefined;
    utils: import("./Utils") | undefined;
    _gasMetering: GasMeter | undefined;
    /**
     * @dev Load additional token using resolver
     * @param {String} superTokenKey super token key used to query resolver
     */
    isSuperTokenListed(superTokenKey: string): Promise<boolean>;
    /**
     * @dev Load additional token using resolver
     * @param {String} tokenKey token key used to query resolver (in order of preference):
     *    - super chain-native token symbol (see getConfig.js),
     *    - underlying token resolver key (tokens.{KEY}),
     *    - super token key  (supertokens.{protocol_release_version}.{KEY})
     */
    loadToken(tokenKey: string): Promise<void>;
    /**
     * @dev Create the ERC20 wrapper from underlying token
     * @param {Any} tokenInfo the TokenInfo contract object to the underlying token
     * @param {string} superTokenName (optional) overriding superTokenName
     * @param {string} superTokenSymbol (optional) overriding superTokenSymbol
     * @param {address} from (optional) send transaction from
     * @param {address} upgradability (optional) send transaction from
     * @return {Promise<Transaction>} web3 transaction object
     */
    createERC20Wrapper(tokenInfo: any, { superTokenSymbol, superTokenName, from, upgradability }?: string): Promise<any>;
    user({ address, token, options }: {
        address: any;
        token: any;
        options: any;
    }): User;
    batchCall(calls: any): any;
    /**
     * @dev call to add a tx in the gas report. Does nothing if gas report type is not set.
     * @param {tx oject} tx as returned by truffleContract action
     * @param {str} actionName action title for row in report
     */
    _pushTxForGasReport(tx: any, actionName: any): void;
    /**
     * @dev generate gas report with transactions pushed until this call
     * @param {str} name file name for gas report
     * @throws if gas report type was not indicated in constructor
     */
    generateGasReport(name: any): void;
}
import ConstantFlowAgreementV1Helper = require("./ConstantFlowAgreementV1Helper");
import InstantDistributionAgreementV1Helper = require("./InstantDistributionAgreementV1Helper");
import GasMeter = require("./utils/gasMetering/gasMetering");
import User = require("./User");
