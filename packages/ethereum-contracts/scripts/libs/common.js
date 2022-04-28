const path = require("path");
const async = require("async");
const {promisify} = require("util");
const readline = require("readline");

const ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";

/****************************************************************
 * Truffle scripts utilities
 ****************************************************************/
/**
 * @dev Promisified readline question utility
 */
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

/// @dev Extract the web3 options used to initialize the SDK
function extractWeb3Options({isTruffle, web3, ethers, from}) {
    return {isTruffle, web3, ethers, from};
}

/// @dev Load contract from truffle built artifacts
function builtTruffleContractLoader(name) {
    try {
        const directoryPath = path.join(__dirname, "../../build/contracts");
        const builtContract = require(path.join(directoryPath, name + ".json"));
        return builtContract;
    } catch (e) {
        throw new Error(
            `Cannot load built truffle contract ${name}. Have you built?`
        );
    }
}

//
// Detect truffle environment
//
function detectTruffle() {
    const stackTrace = require("stack-trace");
    const trace = stackTrace.get();
    //trace.forEach((callSite) => console.debug(callSite.getFileName()));
    const truffleDetected =
        trace.filter((callSite) =>
            (callSite.getFileName() || "").match(
                /node_modules\/truffle\/build\/[^/]+\.bundled\.js/
            )
        ).length > 0;
    console.log("truffle detected", truffleDetected);
    return truffleDetected;
}

/****************************************************************
 * Contracts upgradability utilities
 ****************************************************************/

/**
 * @dev Is the address a contract (it has code, not an EOA)
 */
async function hasCode(web3, address) {
    const code = await web3.eth.getCode(address);
    return code.length > 3;
}

/**
 * @dev Check if the code at the address differs from the contract object provided
 */
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
        codeTrimed = codeTrimed.replace(
            new RegExp(r, "g"),
            "0".repeat(r.length)
        );
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

/**
 * @dev Check if the address is a UUPS proxiable
 */
async function isProxiable(UUPSProxiable, address) {
    const p = await UUPSProxiable.at(address);
    const codeAddress = await p.getCodeAddress.call();
    return codeAddress !== ZERO_ADDRESS;
}

/**
 * @dev Get code address from an UUPS proxiable
 */
async function getCodeAddress(UUPSProxiable, proxyAddress) {
    const proxiable = await UUPSProxiable.at(proxyAddress);
    return await proxiable.getCodeAddress();
}

/****************************************************************
 * Admin (resolver and governance) utilities
 ****************************************************************/

/**
 * @dev Set resolver key-value
 *
 * process.env.RESOLVER_ADMIN_TYPE:
 * - MULTISIG
 * - (default) ownable
 */
async function setResolver(sf, key, value) {
    console.log(`Setting resolver ${key} -> ${value} ...`);
    const resolver = await sf.contracts.Resolver.at(sf.resolver.address);
    switch (process.env.RESOLVER_ADMIN_TYPE) {
        case "MULTISIG": {
            console.log("Resolver Admin type: MultiSig");
            const ADMIN_ROLE = "0x" + "0".repeat(64);
            const ac = await sf.contracts.IAccessControlEnumerable.at(
                sf.resolver.address
            );
            const rmCnt = (await ac.getRoleMemberCount(ADMIN_ROLE)).toNumber();
            // always picks the last admin set (could be more than one)
            const resolverAdmin = await ac.getRoleMember(ADMIN_ROLE, rmCnt - 1);
            const multis = await sf.contracts.IMultiSigWallet.at(resolverAdmin);
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
            console.log("Resolver Admin type: Direct Ownership (default)");
            console.log("Executing admin action...");
            await resolver.set(key, value);
            console.log("Admin action executed.");
        }
    }
}

/**
 * @dev Send governance action
 *
 * process.env.GOVERNANCE_ADMIN_TYPE:
 * - MULTISIG
 * - (default) ownable
 */
async function sendGovernanceAction(sf, actionFn) {
    const gov = await sf.contracts.SuperfluidGovernanceBase.at(
        await sf.host.getGovernance.call()
    );
    switch (process.env.GOVERNANCE_ADMIN_TYPE) {
        case "MULTISIG": {
            console.log("Governance Admin Type: MultiSig");
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
            console.log("Governance Admin Type: Direct Ownership (default)");
            console.log("Executing governance action...");
            await actionFn(gov);
            console.log("Governance action executed.");
        }
    }
}

/****************************************************************
 * Event queries
 ****************************************************************/

function _toHex(n) {
    return "0x" + n.toString(16);
}

async function getPastEvents({config, contract, eventName, filter, topics}) {
    const initialBlockNumber = config.data.initialBlockNumber || 0;
    const latestBlock = await web3.eth.getBlock("latest");
    let blockRanges = [];
    if (!config.data.getLogsRange) {
        blockRanges.push([
            _toHex(initialBlockNumber),
            _toHex(latestBlock.number),
        ]);
    } else {
        let i = initialBlockNumber;
        do {
            blockRanges.push([_toHex(i), _toHex(i + config.data.getLogsRange)]);
        } while ((i += config.data.getLogsRange) <= latestBlock.number);
        console.debug(
            "blockRanges",
            blockRanges.length,
            initialBlockNumber,
            latestBlock.number
        );
    }
    const result = await async.concatSeries(blockRanges, async (r) => {
        if (blockRanges.length > 1) process.stdout.write(".");
        let ret;
        if (contract) {
            ret = contract.getPastEvents(eventName, {
                fromBlock: r[0],
                toBlock: r[1],
                filter,
            });
        } else {
            ret = web3.eth.getPastLogs({
                fromBlock: r[0],
                toBlock: r[1],
                topics,
            });
        }
        if (blockRanges.length > 1 && ret.length > 0)
            process.stdout.write(ret.length.toString());
        return ret;
    });
    if (blockRanges.length > 1) process.stdout.write("\n");
    return result;
}

/****************************************************************
 * Script Runner helpers
 ****************************************************************/

/**
 * @dev Get script runner factory
 *
 * NOTE:
 * Due tue truffle only injecting web3, artifacts, etc. in node execution context,
 * it is required that the script can access this context within the same
 * script file. Hence you would need to do something like this:
 * ```
 *  eval(`(${getScriptRunnerFactory.toString()})()`)
 * ```
 * Crazy stuff.
 */
function getScriptRunnerFactory(runnerOpts = {}) {
    return (logicFn) => {
        const {detectTruffle} = require("./libs/common");
        return require("./libs/truffleScriptRunnerFactory")(
            () => ({
                artifacts:
                    typeof artifacts !== "undefined" ? artifacts : undefined,
                web3: typeof web3 !== "undefined" ? web3 : undefined,
                truffleDetected: detectTruffle(),
            }),
            logicFn,
            runnerOpts
        );
    };
}

module.exports = {
    ZERO_ADDRESS,

    rl,
    extractWeb3Options,
    builtTruffleContractLoader,
    detectTruffle,

    hasCode,
    codeChanged,
    getCodeAddress,
    isProxiable,

    setResolver,
    sendGovernanceAction,

    getPastEvents,

    getScriptRunnerFactory,
};
