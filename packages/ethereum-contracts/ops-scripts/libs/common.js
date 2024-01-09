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
        const directoryPath = path.join(__dirname, "../../build/truffle");
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
 * @param replacements should contain all immutable contract fields, encoded as words
 * TODO: this isn't always working as intended, see https://github.com/superfluid-finance/protocol-monorepo/issues/1448
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

    // since the Resolver implements AccessControlEnumerable, it could have multiple admins.
    // we're currently using a single admin, thus can just pick the last one here.
    const ADMIN_ROLE = "0x" + "0".repeat(64);
    const ac = await sf.contracts.IAccessControlEnumerable.at(
        sf.resolver.address
    );
    const nrAdmins = (await ac.getRoleMemberCount(ADMIN_ROLE)).toNumber();
    const resolverAdmin = nrAdmins > 0 ?
        await ac.getRoleMember(ADMIN_ROLE, nrAdmins - 1):
        await (async () => {
            // This is for eth-goerli (and maybe other networks too)
            console.log(`!!! resolver.getRoleMemberCount() returned 0. Trying account[0] as resolver admin.`);
            return (await web3.eth.getAccounts())[0];
        })();

    const adminType = process.env.RESOLVER_ADMIN_TYPE
        || await autodetectAdminType(sf, resolverAdmin);

    switch (adminType) {
        case "MULTISIG": {
            console.log("Resolver Admin type: MultiSig");
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
        case "OWNABLE": {
            console.log("Resolver Admin type: Direct Ownership (default)");
            console.log("Executing admin action...");
            await resolver.set(key, value);
            console.log("Admin action executed.");
            break;
        }
        case "SAFE": {
            await executeSafeTransaction(
                resolverAdmin,
                resolver.address,
                resolver.contract.methods.set(key, value).encodeABI()
            );
            break;
        }
        default: {
            throw new Error("No known admin type specified and autodetect failed");
        }
    }
}

/**
 * @dev Send governance action
 *
 * process.env.GOVERNANCE_ADMIN_TYPE:
 * - MULTISIG
 * - OWNABLE
 * - SAFE
 * - (default) auto-detect (doesn't yet detect Safe)
 *
 * @param sf instance of SuperfluidSDK
 * @param actionFn function that gets governance methods as argument
 *
 * @note if the caller intends to invoke methods only available in SuperfluidGovernanceII
 * (e.g. UUPSProxiable or Ownable), it must provide the SuperfluidGovernanceII artifact
 * in the sf object.
 */
async function sendGovernanceAction(sf, actionFn) {
    const govAddr = await sf.host.getGovernance.call();
    console.log("Governance address:", govAddr);
    const gov = sf.contracts.SuperfluidGovernanceII !== undefined ?
        await sf.contracts.SuperfluidGovernanceII.at(govAddr) :
        await sf.contracts.SuperfluidGovernanceBase.at(govAddr);

    const govOwner = await (await sf.contracts.Ownable.at(gov.address)).owner();
    console.log("Governance owner:", govOwner);

    const adminType = process.env.GOVERNANCE_ADMIN_TYPE
        || await autodetectAdminType(sf, govOwner);

    switch (adminType) {
        case "MULTISIG": {
            console.log("Governance Admin Type: MultiSig");
            const multis = await sf.contracts.IMultiSigWallet.at(
                await (await sf.contracts.Ownable.at(gov.address)).owner()
            );
            console.log("MultiSig address:", multis.address);
            const data = actionFn(gov.contract.methods).encodeABI();
            console.log("MultiSig data", data);
            console.log("Sending governance action to multisig...");
            await multis.submitTransaction(gov.address, 0, data);
            console.log(
                "Governance action sent, but it may still need confirmation(s)."
            );
            break;
        }
        case "OWNABLE": {
            console.log("Governance Admin Type: Direct Ownership (default)");
            console.log("Executing governance action...");
            await actionFn(gov);
            console.log("Governance action executed.");
            break;
        }
        case "SAFE": {
            await executeSafeTransaction(
                govOwner, // Safe address
                gov.address, // target contract address
                actionFn(gov.contract.methods).encodeABI() // safeTxData
            );
            break;
        }
        default: {
            throw new Error("No known admin type specified and autodetect failed");
        }
    }
}

/****************************************************************
 * Multisig helpers
 ****************************************************************/

// Probes the given account to see what kind of admin it is.
// Possible return values: "MULTISIG", "OWNABLE".
// Throws when encountering an unknown contract.
// TODO: add support for detecting SAFE
async function autodetectAdminType(sf, account) {
    if (!await hasCode(web3, account)) {
        console.log("account has no code");
        return "OWNABLE";
    }

    try {
        const multis = await sf.contracts.IMultiSigWallet.at(account);
        await multis.required();
        return "MULTISIG";
    } catch(e) {
        console.log("not detecting legacy multisig fingerprint");
    }

    try {
        const safe = await sf.contracts.ISafe.at(account);
        const safeVersion = await safe.VERSION();
        console.log("detected Safe version", safeVersion);
        return "SAFE";
    } catch(e) {
        console.log("not detecting Safe fingerprint");
    }

    throw new Error(`Unknown admin contract type of account ${account}`);
}

// returns the Safe Tx Service URL or throws if none available
// source: https://github.com/safe-global/safe-docs/blob/main/safe-core-api/available-services.md
function getSafeTxServiceUrl(chainId) {
    const safeChainNames = {
        // mainnets
        1: "mainnet",
        10: "optimism",
        56: "bsc",
        100: "gnosis-chain",
        137: "polygon",
        8453: "base",
        42161: "arbitrum",
        43114: "avalanche",
        // testnets
        5: "goerli",
        84531: "base-testnet"
    };
    if (safeChainNames[chainId] === undefined) {
        throw new Error(`no Safe tx service url known for chainId ${chainId}`);
    }
    return `https://safe-transaction-${safeChainNames[chainId]}.safe.global`;
}

// safeTxData is the ABI encoded transaction data of the inner call to be made by the Safe
async function executeSafeTransaction(safeAddr, targetContractAddr, safeTxData) {
    const Web3Adapter = require('@safe-global/safe-web3-lib').default;
    const Safe = require('@safe-global/safe-core-sdk').default;
    const SafeServiceClient = require('@safe-global/safe-service-client').default;

    const safeOwner = (await web3.eth.getAccounts())[0]; // tx sender
    console.log("Safe signer being used:", safeOwner);

    const ethAdapterOwner1 = new Web3Adapter({
        web3,
        signerAddress: safeOwner
    });

    const safeSdk = await Safe.create({ ethAdapter: ethAdapterOwner1, safeAddress: safeAddr });

    const safeService = new SafeServiceClient({
        txServiceUrl: getSafeTxServiceUrl(await web3.eth.getChainId()),
        ethAdapter: ethAdapterOwner1
    });

    const data = safeTxData;
    const nextNonce = await safeService.getNextNonce(safeAddr);
    const safeTransactionData = {
        to: targetContractAddr,
        value: 0,
        data: data,
        nonce: process.env.SAFE_REPLACE_LAST_TX ? nextNonce-1 : nextNonce
    };
    const safeTransaction = await safeSdk.createTransaction({ safeTransactionData });
    console.log("Safe tx:", safeTransaction);

    const safeTxHash = await safeSdk.getTransactionHash(safeTransaction);
    console.log("Safe tx hash:", safeTxHash);
    const signature = await safeSdk.signTransactionHash(safeTxHash);
    console.log("Signature:", signature);

    const transactionConfig = {
        safeAddress: safeAddr,
        safeTransactionData: safeTransaction.data,
        safeTxHash: safeTxHash,
        senderAddress: safeOwner,
        senderSignature: signature.data,
        origin: "ops-scripts"
    };

    const pendingTxsBefore = await safeService.getPendingTransactions(safeAddr);

    // according to the docs this should return the tx hash, but always returns undefined although succeeding
    const ret = await safeService.proposeTransaction(transactionConfig);
    console.log("returned:", ret);

    const pendingTxsAfter = await safeService.getPendingTransactions(safeAddr);
    console.log(`pending txs before ${pendingTxsBefore.count}, after ${pendingTxsAfter.count}`);

    // workaround for verifying that the proposal was added
    if (!pendingTxsAfter.count > pendingTxsBefore.count) {
        throw new Error("Safe pending transactions count didn't increase, propose may have failed!");
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

/****************************************************************
 * Helpers to store versionString in Resolver
 ****************************************************************/

// versionString format: [x]x.[y]y.[z]z-rrrrrrrr
// x: major version, y: minor version, z: patch, r: 8-digit git revision (hex)

// takes an argument of the form [x]x.[y]y.[z]z-rrrrrrrr and returns a pseudo address
function versionStringToPseudoAddress(versionString) {
    const [versions, suffix] = versionString.split('-');
    const [major, minor, patch] = versions.split('.').map(v => v.padStart(2, '0'));  // Pad with leading zeros
    return `0x000000000000000000${major}${minor}${patch}${suffix}`;
}

// takes a pseudo address as argument and decodes it to a versionString
function pseudoAddressToVersionString(pseudoAddress) {
    const str = pseudoAddress.replace(/^0x/, '').toLowerCase(); // remove leading 0x
    const major = parseInt(str.slice(18, 20), 10);
    const minor = parseInt(str.slice(20, 22), 10);
    const patch = parseInt(str.slice(22, 24), 10);
    const revision = str.slice(24);

    if (
        !str.startsWith("000000000000000000") ||
        isNaN(major) || isNaN(minor) || isNaN(patch)
    ) {
        throw new Error("Provided address doesn't encode a valid versionString");
    }

    return `${major}.${minor}.${patch}-${revision}`;
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

    versionStringToPseudoAddress,
    pseudoAddressToVersionString,
};
