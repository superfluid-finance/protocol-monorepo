const async = require("async");
const fs = require("fs");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {ZERO_ADDRESS} = require("./libs/common");

const MAX_REQUESTS = 20;

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Upgrade a managed super token to the latest logic
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 * @param {boolean} options.dryRun Print out what would be done without actually creating/executing a gov action
 *                  (overriding env: DRY_RUN) - NOT set by default
 * @param {string} options.skipTokensFile Name of a JSON file with a list of tokens (by chainId) to be skipped
 *                  (overriding env: SKIP_TOKENS_FILE) - defaults to "./upgrade_skip_tokens.json"
 * @param {string} options.outputFile Name of file where to log addresses of tokens (to be) updated
 *                  (overriding env: OUTPUT_FILE)
 * @param {string} options.superTokenLogic override address for the logic to upgrade to instead of the canonical one
 *                  (overriding env: SUPER_TOKEN_LOGIC
 *
 * Usage: npx truffle exec ops-scripts/gov-upgrade-super-token-logic.js : ALL | {SUPER_TOKEN_ADDRESS} ...
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Upgrade super token logic ========");
    let {protocolReleaseVersion, skipTokensFile, dryRun, outputFile, superTokenLogic} = options;

    console.log("protocol release version:", protocolReleaseVersion);
    dryRun = dryRun || process.env.DRY_RUN !== undefined;
    console.log("dry run:", dryRun);

    skipTokensFile = skipTokensFile || process.env.SKIP_OUTPUT_FILE || "./upgrade_skip_tokens.json";
    let skipTokens = [];
    if (fs.existsSync(skipTokensFile)) {
        console.log(`File with tokens to be skipped: ${skipTokensFile}`);
        const chainId = await web3.eth.getChainId();
        const fileContents = JSON.parse(fs.readFileSync(skipTokensFile));
        skipTokens = fileContents[chainId] || [];
        for (const t of skipTokens) {
            if (!web3.utils.isAddress(t)) {
                throw new Error(`Invalid address: ${t}`);
            }
        }
        console.log(`tokens to be skipped: ${JSON.stringify(skipTokens, null, 2)}`);
    } else {
        console.warn(`File with tokens to be skipped doesn't exist: ${skipTokensFile}`);
    }

    outputFile = outputFile || process.env.OUTPUT_FILE;
    console.log("output file: ", outputFile);

    superTokenLogic = superTokenLogic || process.env.SUPER_TOKEN_LOGIC;

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "SuperfluidGovernanceBase",
            "Resolver",
            "UUPSProxiable",
            "SETHProxy",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const canonicalSuperTokenLogic = await getCanonicalSuperTokenLogic(sf);
    console.log(`current canonical super token logic: ${canonicalSuperTokenLogic}`);

    let tokensToBeUpgraded = (args.length === 1 && args[0] === "ALL") ?
        await getTokensToBeUpgraded(sf, canonicalSuperTokenLogic, skipTokens) :
        Array.from(args);

    console.log(`${tokensToBeUpgraded.length} tokens to be upgraded)`);

    const superTokenLogicAddr = superTokenLogic !== undefined ?
        superTokenLogic :
        canonicalSuperTokenLogic;

    console.log("SuperToken logic to update to:", superTokenLogicAddr);

    if (tokensToBeUpgraded.length > 0) {
        console.log(`${tokensToBeUpgraded.length} tokens to be upgraded`);
        if (outputFile !== undefined) {
            fs.writeFileSync(outputFile, JSON.stringify(tokensToBeUpgraded, null, 2));
            console.log(`List of tokens to be upgraded written to ${outputFile}`);
        }

        const batchSize = parseInt(process.env.BATCH_SIZE) || 1000;
        for (let offset = 0; offset < tokensToBeUpgraded.length; offset += batchSize) {
            const batch = tokensToBeUpgraded.slice(offset, offset + batchSize);
            console.log(
                `*** Batch ${offset/batchSize+1} | upgrading ${batch.length} super tokens: ${JSON.stringify(batch, null, 2)}`
            );
            if (!dryRun) {
                // a non-canonical logic address can be provided in an extra array (batchUpdateSuperTokenLogic is overloaded)
                const govAction = superTokenLogic !== undefined ?
                    (gov) => gov.batchUpdateSuperTokenLogic(sf.host.address, batch, [...new Array(batch.length)].map(e => superTokenLogicAddr)) :
                    (gov) => gov.batchUpdateSuperTokenLogic(sf.host.address, batch)

                await sendGovernanceAction(sf, govAction);

                // When first updating to the version adding native flow NFTs, this needs to be run twice
                console.log("checking if 2nd run needed...");
                try {
                    const beaconST = await sf.contracts.ISuperToken.at(batch[0]);
                    const cofAddr = await beaconST.CONSTANT_OUTFLOW_NFT();
                    if (cofAddr === ZERO_ADDRESS) {
                        console.log("...running upgrade again for NFT initialization...");
                        // the first time it is to get the code to initialize the NFT proxies there
                        // the second time is to actually execute that code in updateCode
                        await sendGovernanceAction(sf, govAction);
                    } else {
                        console.log("...not needed");
                    }
                } catch (e) {
                    console.log(`failed to read constantOutflowNFT addr: ${e.toString()}`);
                    console.log("this is expected if running against a pre-1.6.0 deployment");
                }
            }
        }
    }
});

// returns the current canonical SuperToken logic of the SF deployment
async function getCanonicalSuperTokenLogic(sf) {
    const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
        await sf.host.getSuperTokenFactory.call()
    );
    return superTokenFactory.getSuperTokenLogic();
}

// gets a list of tokens (addresses) to be upgraded
// starts from the list of all SuperTokens returned by the subgraph,
// from there filters out those
// - having a different host
// - not being a proxy or not having a logic address
// - already pointing to the latest logic
// - in the skip list (e.g. because not managed by SF gov)
async function getTokensToBeUpgraded(sf, canonicalSuperTokenLogic, skipList) {
    const maxItems = parseInt(process.env.MAX_ITEMS) || 1000;
    const skipItems = parseInt(process.env.SKIP_ITEMS) || 0;

    const candidateTokens = (await sf.subgraphQuery(`{
        tokens(where: {isSuperToken: true}, first: ${maxItems}, skip: ${skipItems}) {
            id
        }
    }`)).tokens.map((i) => i.id);

    console.log(`Got ${candidateTokens.length} candidates`);
    if (candidateTokens.length >= maxItems) {
        console.warn("### There's more items than returned by the query");
    }

    return (await async.mapLimit(
        candidateTokens,
        MAX_REQUESTS,
        async (superTokenAddress) => {
            const superToken = await sf.contracts.ISuperToken.at(superTokenAddress);
            try {
                const symbol = await superToken.symbol();
                if ((await superToken.getHost()) !== sf.host.address) {
                    throw new Error(
                        "Super token is from a different universe"
                    );
                }
                const superTokenLogic = await (
                    await sf.contracts.UUPSProxiable.at(superTokenAddress)
                ).getCodeAddress();

                if (superTokenLogic === ZERO_ADDRESS) {
                    console.log(
                        `SuperToken@${superToken.address} (${symbol}) is likely an uninitalized proxy`
                    );
                } else if (canonicalSuperTokenLogic !== superTokenLogic) {
                    console.log(
                        `SuperToken@${superToken.address} (${symbol}) logic needs upgrade from ${superTokenLogic}`
                    );
                    return superTokenAddress;
                } else {
                    console.log(
                        `SuperToken@${superToken.address} (${symbol}) logic is up to date`
                    );
                    return undefined;
                }
            } catch {
                console.warn(
                    `[WARN] SuperToken@${superToken.address} is smelly`
                );
                return undefined;
            }
        }
    )).filter((i) => typeof i !== "undefined")
    .filter((item) => !skipList.map(e => e.toLowerCase()).includes(item.toLowerCase()));
}