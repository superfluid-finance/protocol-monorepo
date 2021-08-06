const path = require("path");
const { promisify } = require("util");
const readline = require("readline");

async function rl() {
    // promisify the readline
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
    });
    // Prepare readline.question for promisification
    rl.question[promisify.custom] = (question) => {
        return new Promise((resolve) => {
            rl.question(question, resolve);
        });
    };

    const answer = await promisify(rl.question).apply(null, arguments);

    rl.close();

    return answer;
}

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

async function codeChanged(
    web3,
    contract,
    address,
    replacements = [],
    debug = false
) {
    // use .binary instead of .bytecode
    // since .binary will have the linked library addresses
    const binaryFromCompiler = contract.binary;
    const code = await web3.eth.getCode(address);

    // no code
    if (code.length <= 3) return true;

    // SEE: https://github.com/ConsenSys/bytecode-verifier/blob/master/src/verifier.js
    // find the second occurance of the init code
    let codeTrimed = code.slice(code.lastIndexOf("6080604052")).toLowerCase();
    const binaryTrimed = binaryFromCompiler
        .slice(binaryFromCompiler.lastIndexOf("6080604052"))
        .toLowerCase();

    // extra replacements usually for constructor parameters
    replacements.forEach((r) => {
        let codeTrimed2 = codeTrimed.replace(
            new RegExp(r, "g"),
            "0".repeat(r.length)
        );
        if (codeTrimed === codeTrimed2)
            throw new Error("Code replacement not found");
        codeTrimed = codeTrimed2;
    });

    if (debug) {
        console.debug(codeTrimed);
        console.debug(binaryTrimed);
    }
    // console.log(code);
    // console.log(bytecodeFromCompiler);
    // console.log(bytecodeFromCompiler.indexOf(code.slice(2)));
    return binaryTrimed !== codeTrimed;
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
 * 1. This has to be invoked within the same context of the caller, in order
 * to use "web3", "artifacts" from the truffle execution context.
 * The correct way of using this then should be:
 * ```
 * eval(`(${detectIsTruffle.toString()})()`)
 * ```
 * 2. This will expose `web3` object to global
 */
async function detectTruffleAndConfigure(options) {
    function _detectTruffle() {
        const stackTrace = require("stack-trace");
        const trace = stackTrace.get();
        //trace.forEach((callSite) => console.debug(callSite.getFileName()));
        return (
            trace.filter((callSite) =>
                (callSite.getFileName() || "").match(
                    /node_modules\/truffle\/build\/[^/]+\.bundled\.js/
                )
            ).length > 0
        );
    }

    const truffleDetected = _detectTruffle();
    // if isTruffle is not set explicitly
    if (!("isTruffle" in options)) {
        if ("DISABLE_NATIVE_TRUFFLE" in process.env) {
            options.isTruffle = !process.env.DISABLE_NATIVE_TRUFFLE;
        } else {
            options.isTruffle = truffleDetected;
        }
    }
    if (options.isTruffle) {
        if (options.web3) {
            throw Error(
                "Flag 'isTruffle' cannot be 'true' when using a web3 instance."
            );
        }
        // set these globally so that it's available throughout the executions
        global.web3 = web3;
        global.artifacts = artifacts;
    } else {
        if (!truffleDetected) {
            if (!options.web3) {
                throw Error(
                    "A web3 instance is not provided when not using truffle."
                );
            }
            global.web3 = options.web3;
        } else {
            // use web3 of truffle
            options.web3 = global.web3 = web3;
        }
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

async function setResolver(sf, key, value) {
    console.log(`Setting resolver ${key} -> ${value} ...`);
    const resolver = await sf.contracts.TestResolver.at(sf.resolver.address);
    switch (process.env.ADMIN_TYPE) {
        case "MULTISIG": {
            console.log("Admin type: MultiSig");
            // assuming governance owner manages the resolver too...
            const multis = await sf.contracts.IMultiSigWallet.at(
                await (
                    await sf.contracts.Ownable.at(
                        await sf.host.getGovernance.call()
                    )
                ).owner()
            );
            console.log("MultiSig address: ", multis.address);
            const data = resolver.contract.methods.set(key, value).encodeABI();
            console.log("MultiSig data", data);
            console.log("Sending admin action to multisig...");
            await multis.submitTransaction(resolver.address, 0, data);
            console.log(
                "Admin action sent, but it may still need confirmation(s)."
            );
            break;
        }
        default: {
            console.log("Admin type: Direct Ownership (default)");
            console.log("Executing admin action...");
            await resolver.set(key, value);
            console.log("Admin action executed.");
        }
    }
}

async function sendGovernanceAction(sf, actionFn) {
    const gov = await sf.contracts.SuperfluidGovernanceBase.at(
        await sf.host.getGovernance.call()
    );
    switch (process.env.GOVERNANCE_TYPE) {
        case "MULTISIG": {
            console.log("Governance type: MultiSig");
            const multis = await sf.contracts.IMultiSigWallet.at(
                await (await sf.contracts.Ownable.at(gov.address)).owner()
            );
            console.log("MultiSig address: ", multis.address);
            const data = actionFn(gov.contract.methods).encodeABI();
            console.log("MultiSig data", data);
            console.log("Sending governance action to multisig...");
            await multis.submitTransaction(gov.address, 0, data);
            console.log(
                "Governance action sent, but it may still need confirmation(s)."
            );
            break;
        }
        default: {
            console.log("Governance type: Direct Ownership (default)");
            console.log("Executing governance action...");
            await actionFn(gov);
            console.log("Governance action executed.");
        }
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
    rl,
    builtTruffleContractLoader,
    setResolver,
    sendGovernanceAction,
};
