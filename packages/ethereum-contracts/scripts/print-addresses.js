const fs = require("fs");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const loadContracts = require("./loadContracts");
const {
    parseColonArgs,
    extractWeb3Arguments,
    detectIsTruffle
} = require("./utils");

/**
 * @dev Inspect accounts and their agreements
 *
 * Usage: npx truffle exec scripts/print-addresses : output_file
 */
module.exports = async function(callback, argv, options = {}) {
    try {
        options.isTruffle =
            options.isTruffle || eval(`(${detectIsTruffle.toString()})()`);
        const web3Arguments = extractWeb3Arguments(options);

        const args = parseColonArgs(argv || process.argv);
        if (args.length != 1) {
            throw new Error("Not enough arguments");
        }
        const outputFilename = args.shift();

        const { UUPSProxiable, ISuperTokenFactory } = loadContracts({
            web3: web3 || options.web3,
            ethers: options.ethers,
            from: options.from
        });

        const getCodeAddress = async proxyAddress => {
            const proxiable = await UUPSProxiable.at(proxyAddress);
            return await proxiable.getCodeAddress();
        };

        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        const sf = new SuperfluidSDK.Framework({
            ...web3Arguments,
            version: process.env.RELEASE_VERSION || "test",
            tokens
        });
        await sf.initialize();
        if (sf.config.nativeTokenSymbol) {
            await sf.loadToken(sf.config.nativeTokenSymbol);
        }

        let output = "";
        output += `SUPERFLUID_HOST_PROXY=${sf.host.address}\n`;
        output += `SUPERFLUID_HOST_LOGIC=${await getCodeAddress(
            sf.host.address
        )}\n`;
        output += `SUPERFLUID_GOVERNANCE=${await sf.host.getGovernance()}\n`;
        output += `SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY=${await sf.host.getSuperTokenFactory()}\n`;
        output += `SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC=${await sf.host.getSuperTokenFactoryLogic()}\n`;
        output += `CFA_PROXY=${sf.agreements.cfa.address}\n`;
        output += `CFA_LOGIC=${await getCodeAddress(
            sf.agreements.cfa.address
        )}\n`;
        output += `IDA_PROXY=${sf.agreements.ida.address}\n`;
        output += `IDA_LOGIC=${await getCodeAddress(
            sf.agreements.ida.address
        )}\n`;
        output += `SUPERFLUID_SUPER_TOKEN_LOGIC=${await (
            await ISuperTokenFactory.at(await sf.host.getSuperTokenFactory())
        ).getSuperTokenLogic()}\n`;
        tokens.forEach(tokenName => {
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
