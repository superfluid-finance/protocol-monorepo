const { promisify } = require("util");
const readline = require("readline");

// promisify the readline
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});
// Prepare readline.question for promisification
rl.question[promisify.custom] = (question) => {
    return new Promise((resolve) => {
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

async function hasCode(address) {
    const code = await web3.eth.getCode(address);
    return code.length > 3;
}

async function codeChanged(contract, address) {
    const code = await web3.eth.getCode(address);
    // no code
    if (code.length <= 3) return true;
    // SEE: https://github.com/ConsenSys/bytecode-verifier/blob/master/src/verifier.js
    // find the second occurance of the init code
    const bytecodeFromCompiler = contract.bytecode;
    // console.log(code);
    // console.log(bytecodeFromCompiler);
    // console.log(bytecodeFromCompiler.indexOf(code.slice(2)));
    return bytecodeFromCompiler.indexOf(code.slice(2)) === -1;
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
    proxiableCodeChanged,
    rl: promisify(rl.question),
};
