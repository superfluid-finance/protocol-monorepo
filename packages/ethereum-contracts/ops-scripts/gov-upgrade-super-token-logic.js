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
 *
 * Usage: npx truffle exec ops-scripts/gov-upgrade-super-token-logic.js : ALL | {SUPER_TOKEN_ADDRESS} ...
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Upgrade super token logic ========");
    let {protocolReleaseVersion, skipTokensFile, dryRun, outputFile} = options;

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

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "ISafe",
            "SuperfluidGovernanceBase",
            "Resolver",
            "UUPSProxiable",
            "SETHProxy",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const {UUPSProxiable, ISuperToken} = sf.contracts;

    const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
        await sf.host.getSuperTokenFactory.call()
    );

    let tokensToBeChecked;
    const maxItems = parseInt(process.env.MAX_ITEMS) || 1000;
    const skipItems = parseInt(process.env.SKIP_ITEMS) || 0;
    if (args.length === 1 && args[0] === "ALL") {
        tokensToBeChecked = (
            await sf.subgraphQuery(`{
            tokenStatistics(first: ${maxItems}, skip: ${skipItems}) {
                token {
                    id
                }
            }
        }`)
        ).tokenStatistics.map((i) => i.token.id);
    } else {
        tokensToBeChecked = Array.from(args);
    }

    console.log(`Got ${tokensToBeChecked.length} candidates`);
    if (tokensToBeChecked.length >= maxItems) {
        console.warn("### There's more items than returned by the query");
    }

    const latestSuperTokenLogic = await superTokenFactory.getSuperTokenLogic();
    console.log("Latest SuperToken logic address", latestSuperTokenLogic);

    // before applying skip list
    let tokensToMaybeBeUpgraded = (
        await async.mapLimit(
            tokensToBeChecked,
            MAX_REQUESTS,
            async (superTokenAddress) => {
                const superToken = await ISuperToken.at(superTokenAddress);
                try {
                    const symbol = await superToken.symbol();
                    if ((await superToken.getHost()) !== sf.host.address) {
                        throw new Error(
                            "Super token is from a different universe"
                        );
                    }
                    const superTokenLogic = await (
                        await UUPSProxiable.at(superTokenAddress)
                    ).getCodeAddress();

                    if (superTokenLogic === ZERO_ADDRESS) {
                        console.log(
                            `SuperToken@${superToken.address} (${symbol}) is likely a not initalized proxy`
                        );
                    } else if (latestSuperTokenLogic !== superTokenLogic) {
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
        )
    ).filter((i) => typeof i !== "undefined");

    console.log(`${tokensToMaybeBeUpgraded.length} tokens to maybe be upgraded (before applying skip list)`);

    const tokensToBeUpgraded = tokensToMaybeBeUpgraded.filter(
        (item) => !skipTokens.map(e => e.toLowerCase()).includes(item.toLowerCase()));

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
                await sendGovernanceAction(sf, (gov) =>
                    gov.batchUpdateSuperTokenLogic(sf.host.address, batch)
                );

                console.log("checking if 2nd run needed");
                try {
                    const beaconST = await ISuperToken.at(batch[0]);
                    const cofAddr = await beaconST.CONSTANT_OUTFLOW_NFT();
                    if (cofAddr === ZERO_ADDRESS) {
                        console.log("running upgrade again for NFT initialization...");
                        // the first time it is to get the code to initialize the NFT proxies there
                        // the second time is to actually execute that code in updateCode
                        await sendGovernanceAction(sf, (gov) =>
                            gov.batchUpdateSuperTokenLogic(sf.host.address, batch)
                        );
                    }
                } catch (e) {
                    console.log(`failed to read constantOutflowNFT addr: ${e.toString()}`);
                    console.log("this is expected if running against a pre-1.6.0 deployment");
                }
            }
        }
    }
});