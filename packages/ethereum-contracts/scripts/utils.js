const path = require("path");
const { promisify } = require("util");
const readline = require("readline");

// promisify the readline
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});
// Prepare readline.question for promisification
rl.question[promisify.custom] = question => {
    return new Promise(resolve => {
        rl.question(question, resolve);
    });
};

// Provide arguments to the script through ":" separator
function parseColonArgs(argv) {
    const argIndex = argv.indexOf(":");
    if (argIndex < 0) {
        throw new Error("No colon arguments provided");
    }
    const args = argv.slice(argIndex + 1);
    console.log("Colon arguments", args);
    return args;
}

const ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";

async function hasCode(web3, address) {
    const code = await web3.eth.getCode(address);
    return code.length > 3;
}

async function codeChanged(web3, contract, address) {
    const bytecodeFromCompiler = contract.bytecode;
    const code = await web3.eth.getCode(address);

    // no code
    if (code.length <= 3) return true;

    // SEE: https://github.com/ConsenSys/bytecode-verifier/blob/master/src/verifier.js
    // find the second occurance of the init code
    const codeTrimed = code.slice(code.lastIndexOf("6080604052"));

    // console.log(code);
    // console.log(bytecodeFromCompiler);
    // console.log(bytecodeFromCompiler.indexOf(code.slice(2)));
    return bytecodeFromCompiler.indexOf(codeTrimed) === -1;
}

async function getCodeAddress(UUPSProxiable, proxyAddress) {
    const proxiable = await UUPSProxiable.at(proxyAddress);
    return await proxiable.getCodeAddress();
}

async function isProxiable(UUPSProxiable, address) {
    const p = await UUPSProxiable.at(address);
    const codeAddress = await p.getCodeAddress.call();
    return codeAddress !== ZERO_ADDRESS;
}

/**
 * @dev Detect if we are running inside the truffle exec and configure the options
 * @param {Object} options the options object to be configured
 *
 * NOTE:
 * This has to be invoked within the same context of the caller, in order
 * to use "web3", "artifacts" from the truffle execution context.
 * The correct way of using this then should be:
 * ```
 * eval(`(${detectIsTruffle.toString()})()`)
 * ```
 */
async function detectTruffleAndConfigure(options) {
    // if isTruffle already set explicitly
    if ("isTruffle" in options) return;
    const stackTrace = require("stack-trace");
    const trace = stackTrace.get();
    //trace.forEach(callSite => console.debug(callSite.getFileName()));
    options.isTruffle =
        trace.filter(callSite =>
            (callSite.getFileName() || "").endsWith(
                "truffle/build/commands.bundled.js"
            )
        ).length > 0;
    if (options.isTruffle) {
        // set these globally so that it's available throughout the executions
        global.web3 = web3;
        global.artifacts = artifacts;
    }
}

/// @dev Extract the web3 options used to initialize the SDK
function extractWeb3Options({ isTruffle, web3, ethers, from }) {
    return { isTruffle, web3, ethers, from };
}

/// @dev Load contract from truffle built artifacts
function builtTruffleContractLoader(name) {
    try {
        const directoryPath = path.join(__dirname, "../build/contracts");
        const builtContract = require(path.join(directoryPath, name + ".json"));
        return builtContract;
    } catch (e) {
        throw new Error(
            `Cannot load built truffle contract ${name}. Have you built?`
        );
    }
}

module.exports = {
    ZERO_ADDRESS,
    parseColonArgs,
    hasCode,
    codeChanged,
    getCodeAddress,
    isProxiable,
    extractWeb3Options,
    detectTruffleAndConfigure,
    rl: promisify(rl.question),
    builtTruffleContractLoader
};
