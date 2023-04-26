const fs = require("fs");
const util = require("util");
const getConfig = require("./libs/getConfig");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    getCodeAddress,
    extractWeb3Options,
} = require("./libs/common");

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.skipTokens Don't iterate through tokens
 *                  (overriding env: SKIP_TOKENS)
 *
 * Usage: npx truffle exec ops-scripts/info-print-contract-addresses : {OUTPUT_FILE}
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    let output = "";

    let {
        protocolReleaseVersion,
        skipTokens
    } = options;
    skipTokens = skipTokens || process.env.SKIP_TOKENS;

    if (args.length !== 1) {
        throw new Error("Wrong number of arguments");
    }
    const outputFilename = args.shift();

    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    const config = getConfig(chainId);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        tokens: skipTokens ? [] : config.tokenList,
        loadSuperNativeToken: !skipTokens, // defaults to true
        additionalContracts: ["UUPSProxiable"],
    });
    await sf.initialize();

    const SuperToken = artifacts.require("SuperToken");
    const UUPSProxiable = artifacts.require("UUPSProxiable");
    const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
    // const GeneralDistributionAgreementV1 = artifacts.require(
    //     "GeneralDistributionAgreementV1"
    // );
    // const SuperfluidUpgradeableBeacon = artifacts.require(
    //     "SuperfluidUpgradeableBeacon"
    // );

    output += `NETWORK_ID=${networkId}\n`;
    output += `SUPERFLUID_LOADER=${sf.loader.address}\n`;
    output += `SUPERFLUID_HOST_PROXY=${sf.host.address}\n`;
    output += `SUPERFLUID_HOST_LOGIC=${await getCodeAddress(
        UUPSProxiable,
        sf.host.address
    )}\n`;
    output += `SUPERFLUID_GOVERNANCE=${await sf.host.getGovernance()}\n`;
    output += `SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY=${await sf.host.getSuperTokenFactory()}\n`;
    output += `SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC=${await sf.host.getSuperTokenFactoryLogic()}\n`;
    output += `CFA_PROXY=${sf.agreements.cfa.address}\n`;
    output += `CFA_LOGIC=${await getCodeAddress(
        UUPSProxiable,
        sf.agreements.cfa.address
    )}\n`;
    output += `IDA_PROXY=${sf.agreements.ida.address}\n`;
    output += `SLOTS_BITMAP_LIBRARY_ADDRESS=${
        "0x" +
        (
            await web3.eth.call({
                to: sf.agreements.ida.address,
                data: "0x3fd4176a", //SLOTS_BITMAP_LIBRARY_ADDRESS()
            })
        ).slice(-40)
    }\n`;
    output += `IDA_LOGIC=${await getCodeAddress(
        UUPSProxiable,
        sf.agreements.ida.address
    )}\n`;
    // const gdaProxy = await sf.host.getAgreementClass(
    //     web3.utils.sha3(
    //         "org.superfluid-finance.agreements.GeneralDistributionAgreement.v1"
    //     )
    // );
    // output += `GDA_PROXY=${gdaProxy}\n`;
    // output += `GDA_LOGIC=${await getCodeAddress(UUPSProxiable, gdaProxy)}\n`;

    // const gdaContract = await GeneralDistributionAgreementV1.at(gdaProxy);
    // const superTokenPoolBeaconContract = await SuperfluidUpgradeableBeacon.at(
    //     await gdaContract.superTokenPoolBeacon()
    // );
    // output += `SUPER_TOKEN_POOL_DEPLOYER_ADDRESS=${await gdaContract.SUPER_TOKEN_POOL_DEPLOYER_ADDRESS()}\n}`;
    // output += `SUPER_TOKEN_POOL_BEACON=${superTokenPoolBeaconContract.address}\n`;
    // output += `SUPER_TOKEN_POOL_LOGIC=${await superTokenPoolBeaconContract.implementation()}\n`;

    const superTokenLogicAddress = await (
        await ISuperTokenFactory.at(await sf.host.getSuperTokenFactory())
    ).getSuperTokenLogic();
    output += `SUPERFLUID_SUPER_TOKEN_LOGIC=${superTokenLogicAddress}\n`;

    const superTokenLogicContract = await SuperToken.at(superTokenLogicAddress);

    const constantOutflowNFTProxyAddress =
        await superTokenLogicContract.CONSTANT_OUTFLOW_NFT();
    output += `CONSTANT_OUTFLOW_NFT_PROXY=${constantOutflowNFTProxyAddress}\n`;

    const constantOutflowNFTLogicAddress = await (
        await UUPSProxiable.at(constantOutflowNFTProxyAddress)
    ).getCodeAddress();
    output += `CONSTANT_OUTFLOW_NFT_LOGIC=${constantOutflowNFTLogicAddress}\n`;

    const constantInflowNFTProxyAddress =
        await superTokenLogicContract.CONSTANT_INFLOW_NFT();
    output += `CONSTANT_INFLOW_NFT_PROXY=${constantInflowNFTProxyAddress}\n`;

    const constantInflowNFTLogicAddress = await (
        await UUPSProxiable.at(constantInflowNFTProxyAddress)
    ).getCodeAddress();
    output += `CONSTANT_INFLOW_NFT_LOGIC=${constantInflowNFTLogicAddress}\n`;

    if (! skipTokens) {
        await Promise.all(
            config.tokenList.map(async (tokenName) => {
                output += `SUPER_TOKEN_${tokenName.toUpperCase()}=${
                    sf.tokens[tokenName].address
                }\n`;
                const underlyingTokenSymbol = await sf.tokens[
                    tokenName
                ].underlyingToken.symbol.call();
                output += `NON_SUPER_TOKEN_${underlyingTokenSymbol.toUpperCase()}=${
                    sf.tokens[tokenName].underlyingToken.address
                }\n`;
            })
        );
        if (sf.config.nativeTokenSymbol) {
            output += `SUPER_TOKEN_NATIVE_COIN=${
                sf.tokens[sf.config.nativeTokenSymbol + "x"].address
            }\n`;
        }
    }

    await util.promisify(fs.writeFile)(outputFilename, output);
});
