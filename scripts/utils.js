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

async function hasCode(address) {
    const code = await web3.eth.getCode(address);
    return code !== "0x" && code !== "";
}

async function codeChanged(contract, address) {
    const code = await web3.eth.getCode(address);
    // SEE: https://github.com/ConsenSys/bytecode-verifier/blob/master/src/verifier.js
    // find the second occurance of the init code
    const startingPoint = contract.bytecode.slice(11).indexOf("6080604052") + 11;
    const bytecodeFromCompiler = contract.bytecode.slice(startingPoint);
    return code.slice(2) !== bytecodeFromCompiler;
}

async function proxiableCodeChanged(Proxiable, contract, address) {
    const p = await Proxiable.at(address);
    return await codeChanged(contract, await p.getCodeAddress());
}

module.exports = {
    parseColonArgs,
    ZERO_ADDRESS,
    hasCode,
    codeChanged,
    proxiableCodeChanged
};
