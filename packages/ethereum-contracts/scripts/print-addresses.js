const fs = require("fs");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    detectTruffleAndConfigure,
    parseColonArgs,
    getCodeAddress,
    extractWeb3Options,
} = require("./utils");

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/print-addresses : output_file
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);

        const args = parseColonArgs(argv || process.argv);
        if (args.length != 1) {
            throw new Error("Not enough arguments");
        }
        const outputFilename = args.shift();

        const networkId = await web3.eth.net.getId();
        console.log("network ID: ", networkId);

        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version: process.env.RELEASE_VERSION || "test",
            tokens,
            loadSuperNativeToken: true,
            additionalContracts: ["UUPSProxiable"],
        });
        await sf.initialize();

        const { UUPSProxiable, ISuperTokenFactory } = sf.contracts;

        let output = "";
        output += `NETWORK_ID=${networkId}\n`;
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
        output += `SUPERFLUID_SUPER_TOKEN_LOGIC=${await (
            await ISuperTokenFactory.at(await sf.host.getSuperTokenFactory())
        ).getSuperTokenLogic()}\n`;
        tokens.forEach((tokenName) => {
            output += `TEST_TOKEN_${tokenName.toUpperCase()}=${
                sf.tokens[tokenName].address
            }\n`;
            output += `SUPER_TOKEN_${tokenName.toUpperCase()}=${
                sf.tokens[tokenName + "x"].address
            }\n`;
        });
        if (sf.config.nativeTokenSymbol) {
            output += `SUPER_TOKEN_${sf.config.nativeTokenSymbol.toUpperCase()}X=${
                sf.tokens[sf.config.nativeTokenSymbol + "x"].address
            }\n`;
        }

        await fs.writeFile(outputFilename, output, callback);
    } catch (err) {
        callback(err);
    }
};
