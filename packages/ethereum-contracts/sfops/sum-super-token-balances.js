#!/usr/bin/env node

const networks = require("../../metadata/main/networks/list.cjs");
const { ethers } = require("ethers");

const DEFAULT_NETWORK = "eth-mainnet";
const DEFAULT_TOKEN = "USDCx";
const DEFAULT_PAGE_SIZE = 1000;
const DEFAULT_RPC_BATCH_SIZE = 100;

const TOKEN_INTERFACE = new ethers.utils.Interface([
    "function balanceOf(address account) view returns (uint256)",
    "function realtimeBalanceOfNow(address account) view returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit, uint256 timestamp)",
    "function totalSupply() view returns (uint256)",
    "function symbol() view returns (string)",
    "function decimals() view returns (uint8)",
]);

function printHelp() {
    console.log(`Usage: node sfops/sum-super-token-balances.js [options]

Defaults:
  --token ${DEFAULT_TOKEN}
  --network ${DEFAULT_NETWORK}

Options:
  --token <symbol-or-address>  Super Token symbol or address
  --network <name-or-chainId>  Superfluid network name, short name, or chainId
  --rpc-url <url>              Override the network RPC URL
  --subgraph-url <url>         Override the protocol-v1 subgraph URL
  --page-size <n>              GraphQL page size for AccountTokenSnapshots
  --rpc-batch-size <n>         Accounts per JSON-RPC batch
  --help                       Show this help
`);
}

function parseArgs(argv) {
    const options = {
        token: DEFAULT_TOKEN,
        network: DEFAULT_NETWORK,
        pageSize: DEFAULT_PAGE_SIZE,
        rpcBatchSize: DEFAULT_RPC_BATCH_SIZE,
    };

    for (let i = 0; i < argv.length; i += 1) {
        const arg = argv[i];
        if (arg === "--help") {
            options.help = true;
            continue;
        }
        if (!arg.startsWith("--")) {
            throw new Error(`Unexpected positional argument: ${arg}`);
        }

        const key = arg.slice(2);
        const value = argv[i + 1];
        if (!value || value.startsWith("--")) {
            throw new Error(`Missing value for --${key}`);
        }
        i += 1;

        if (key === "token") {
            options.token = value;
        } else if (key === "network") {
            options.network = value;
        } else if (key === "rpc-url") {
            options.rpcUrl = value;
        } else if (key === "subgraph-url") {
            options.subgraphUrl = value;
        } else if (key === "page-size") {
            options.pageSize = parsePositiveInt(value, "page-size");
        } else if (key === "rpc-batch-size") {
            options.rpcBatchSize = parsePositiveInt(value, "rpc-batch-size");
        } else {
            throw new Error(`Unknown option: --${key}`);
        }
    }

    return options;
}

function parsePositiveInt(value, label) {
    const parsed = Number.parseInt(value, 10);
    if (!Number.isInteger(parsed) || parsed <= 0) {
        throw new Error(`--${label} must be a positive integer`);
    }
    return parsed;
}

function getNetworkConfig(networkArg) {
    const normalized = String(networkArg).toLowerCase();
    const match = networks.find((network) => {
        const candidates = [
            network.name,
            network.shortName,
            network.humanReadableName,
            network.subgraphV1?.cliName,
            String(network.chainId),
        ]
            .filter(Boolean)
            .map((value) => String(value).toLowerCase());
        return candidates.includes(normalized);
    });

    if (!match) {
        throw new Error(`Could not resolve Superfluid network: ${networkArg}`);
    }

    const rpcUrl = `https://rpc-endpoints.superfluid.dev/${match.name}`;
    const subgraphUrl = match.subgraphV1?.hostedEndpoint;
    if (!subgraphUrl) {
        throw new Error(`No protocol-v1 subgraph configured for network: ${match.name}`);
    }

    return {
        chainId: match.chainId,
        name: match.name,
        rpcUrl,
        subgraphUrl,
    };
}

async function graphqlRequest(url, query, variables) {
    const response = await fetch(url, {
        method: "POST",
        headers: {
            "content-type": "application/json",
        },
        body: JSON.stringify({query, variables}),
    });

    if (!response.ok) {
        const body = await response.text();
        throw new Error(`GraphQL request failed with ${response.status}: ${body}`);
    }

    const payload = await response.json();
    if (payload.errors?.length) {
        throw new Error(`GraphQL error: ${JSON.stringify(payload.errors)}`);
    }

    return payload.data;
}

function isAddress(value) {
    return /^0x[a-fA-F0-9]{40}$/.test(value);
}

async function resolveToken(subgraphUrl, tokenArg) {
    if (isAddress(tokenArg)) {
        const data = await graphqlRequest(
            subgraphUrl,
            `
                query ResolveTokenByAddress($id: ID!) {
                    token(id: $id) {
                        id
                        symbol
                        name
                        decimals
                        isSuperToken
                        isListed
                    }
                }
            `,
            {id: tokenArg.toLowerCase()},
        );

        if (!data.token) {
            throw new Error(`Token not found in subgraph: ${tokenArg}`);
        }
        if (!data.token.isSuperToken) {
            throw new Error(`${tokenArg} is not a Super Token in the protocol subgraph`);
        }
        return data.token;
    }

    const data = await graphqlRequest(
        subgraphUrl,
        `
            query ResolveTokenBySymbol($symbol: String!) {
                tokens(
                    first: 10
                    where: {symbol: $symbol, isSuperToken: true}
                    orderBy: id
                    orderDirection: asc
                ) {
                    id
                    symbol
                    name
                    decimals
                    isSuperToken
                    isListed
                }
            }
        `,
        {symbol: tokenArg},
    );

    const listedMatches = data.tokens.filter((token) => token.isListed);
    const matches = listedMatches.length > 0 ? listedMatches : data.tokens;

    if (matches.length === 0) {
        throw new Error(`No Super Token found for symbol ${tokenArg}`);
    }
    if (matches.length > 1) {
        const choices = matches.map((token) => `${token.symbol}:${token.id}`).join(", ");
        throw new Error(
            `Multiple Super Tokens matched ${tokenArg}. Re-run with --token <address>. Matches: ${choices}`,
        );
    }

    return matches[0];
}

async function fetchTokenStatistic(subgraphUrl, tokenId) {
    const data = await graphqlRequest(
        subgraphUrl,
        `
            query FetchTokenStatistic($id: ID!) {
                tokenStatistic(id: $id) {
                    id
                    updatedAtTimestamp
                    updatedAtBlockNumber
                    totalSupply
                    totalNumberOfAccounts
                    totalNumberOfHolders
                }
            }
        `,
        {id: tokenId},
    );

    return data.tokenStatistic;
}

async function fetchAllAccountTokenSnapshots(subgraphUrl, tokenId, pageSize) {
    const snapshots = [];
    let lastId = "";

    for (;;) {
        const data = await graphqlRequest(
            subgraphUrl,
            `
                query FetchSnapshots($token: String!, $lastId: String!, $first: Int!) {
                    accountTokenSnapshots(
                        first: $first
                        orderBy: id
                        orderDirection: asc
                        where: {token: $token, id_gt: $lastId}
                    ) {
                        id
                        account {
                            id
                        }
                    }
                }
            `,
            {
                token: tokenId,
                lastId,
                first: pageSize,
            },
        );

        const page = data.accountTokenSnapshots;
        if (page.length === 0) {
            return snapshots;
        }

        snapshots.push(...page);
        lastId = page[page.length - 1].id;
        console.error(`Fetched ${snapshots.length} account-token snapshots so far...`);
    }
}

async function fetchAllPools(subgraphUrl, tokenId, pageSize) {
    const pools = [];
    let lastId = "";

    for (;;) {
        const data = await graphqlRequest(
            subgraphUrl,
            `
                query FetchPools($token: String!, $lastId: String!, $first: Int!) {
                    pools(
                        first: $first
                        orderBy: id
                        orderDirection: asc
                        where: {token: $token, id_gt: $lastId}
                    ) {
                        id
                    }
                }
            `,
            {
                token: tokenId,
                lastId,
                first: pageSize,
            },
        );

        const page = data.pools;
        if (page.length === 0) {
            return pools;
        }

        pools.push(...page);
        lastId = page[page.length - 1].id;
        console.error(`Fetched ${pools.length} pools so far...`);
    }
}

async function rpcBatch(url, payload) {
    const response = await fetch(url, {
        method: "POST",
        headers: {
            "content-type": "application/json",
        },
        body: JSON.stringify(payload),
    });

    if (!response.ok) {
        const body = await response.text();
        throw new Error(`RPC request failed with ${response.status}: ${body}`);
    }

    const batchResponse = await response.json();
    if (!Array.isArray(batchResponse)) {
        throw new Error(`Expected batch RPC response array, received: ${JSON.stringify(batchResponse)}`);
    }

    const resultMap = new Map();
    for (const entry of batchResponse) {
        resultMap.set(entry.id, entry);
    }
    return resultMap;
}

function encodeCall(fragment, args) {
    return TOKEN_INTERFACE.encodeFunctionData(fragment, args);
}

function decodeResult(fragment, result) {
    return TOKEN_INTERFACE.decodeFunctionResult(fragment, result);
}

async function fetchLiveTotals(rpcUrl, tokenAddress, accounts, rpcBatchSize) {
    let cumulativeBalanceOf = 0n;
    let cumulativeAvailableBalance = 0n;
    let cumulativePositiveAvailableBalance = 0n;
    let cumulativeDeposit = 0n;
    let cumulativeOwedDeposit = 0n;
    let cumulativeRealtimeBalance = 0n;
    let cumulativePositiveRealtimeBalance = 0n;

    for (let i = 0; i < accounts.length; i += rpcBatchSize) {
        const chunk = accounts.slice(i, i + rpcBatchSize);
        const payload = [];
        const ids = [];

        for (const account of chunk) {
            const balanceId = `${account}:balanceOf`;
            const realtimeId = `${account}:realtimeBalanceOfNow`;

            ids.push({account, balanceId, realtimeId});

            payload.push({
                jsonrpc: "2.0",
                id: balanceId,
                method: "eth_call",
                params: [
                    {
                        to: tokenAddress,
                        data: encodeCall("balanceOf", [account]),
                    },
                    "latest",
                ],
            });
            payload.push({
                jsonrpc: "2.0",
                id: realtimeId,
                method: "eth_call",
                params: [
                    {
                        to: tokenAddress,
                        data: encodeCall("realtimeBalanceOfNow", [account]),
                    },
                    "latest",
                ],
            });
        }

        const results = await rpcBatch(rpcUrl, payload);

        for (const {balanceId, realtimeId} of ids) {
            const balanceEntry = results.get(balanceId);
            const realtimeEntry = results.get(realtimeId);

            if (balanceEntry?.error) {
                throw new Error(`RPC error for ${balanceId}: ${JSON.stringify(balanceEntry.error)}`);
            }
            if (realtimeEntry?.error) {
                throw new Error(`RPC error for ${realtimeId}: ${JSON.stringify(realtimeEntry.error)}`);
            }

            const [balanceOf] = decodeResult("balanceOf", balanceEntry.result);
            const [availableBalance, deposit, owedDeposit] = decodeResult(
                "realtimeBalanceOfNow",
                realtimeEntry.result,
            );

            const balanceOfBigInt = BigInt(balanceOf.toString());
            const availableBalanceBigInt = BigInt(availableBalance.toString());
            const depositBigInt = BigInt(deposit.toString());
            const owedDepositBigInt = BigInt(owedDeposit.toString());
            const realtimeBalanceBigInt =
                availableBalanceBigInt +
                (depositBigInt > owedDepositBigInt
                    ? depositBigInt - owedDepositBigInt
                    : 0n);

            cumulativeBalanceOf += balanceOfBigInt;
            cumulativeAvailableBalance += availableBalanceBigInt;
            if (availableBalanceBigInt > 0n) {
                cumulativePositiveAvailableBalance += availableBalanceBigInt;
            }
            cumulativeDeposit += depositBigInt;
            cumulativeOwedDeposit += owedDepositBigInt;
            cumulativeRealtimeBalance += realtimeBalanceBigInt;
            if (realtimeBalanceBigInt > 0n) {
                cumulativePositiveRealtimeBalance += realtimeBalanceBigInt;
            }
        }

        console.error(`Queried live balances for ${Math.min(i + chunk.length, accounts.length)} / ${accounts.length} accounts...`);
    }

    return {
        cumulativeBalanceOf,
        cumulativeAvailableBalance,
        cumulativePositiveAvailableBalance,
        cumulativeDeposit,
        cumulativeOwedDeposit,
        cumulativeRealtimeBalance,
        cumulativePositiveRealtimeBalance,
    };
}

async function fetchSingleAccountState(rpcUrl, tokenAddress, account) {
    const payload = [
        {
            jsonrpc: "2.0",
            id: "balanceOf",
            method: "eth_call",
            params: [
                {
                    to: tokenAddress,
                    data: encodeCall("balanceOf", [account]),
                },
                "latest",
            ],
        },
        {
            jsonrpc: "2.0",
            id: "realtimeBalanceOfNow",
            method: "eth_call",
            params: [
                {
                    to: tokenAddress,
                    data: encodeCall("realtimeBalanceOfNow", [account]),
                },
                "latest",
            ],
        },
    ];

    const results = await rpcBatch(rpcUrl, payload);
    const [balanceOf] = decodeResult("balanceOf", results.get("balanceOf").result);
    const [availableBalance, deposit, owedDeposit, timestamp] = decodeResult(
        "realtimeBalanceOfNow",
        results.get("realtimeBalanceOfNow").result,
    );

    const balanceOfBigInt = BigInt(balanceOf.toString());
    const availableBalanceBigInt = BigInt(availableBalance.toString());
    const depositBigInt = BigInt(deposit.toString());
    const owedDepositBigInt = BigInt(owedDeposit.toString());
    const realtimeBalanceBigInt =
        availableBalanceBigInt +
        (depositBigInt > owedDepositBigInt ? depositBigInt - owedDepositBigInt : 0n);

    return {
        account: ethers.utils.getAddress(account),
        balanceOf: balanceOfBigInt,
        availableBalance: availableBalanceBigInt,
        deposit: depositBigInt,
        owedDeposit: owedDepositBigInt,
        realtimeBalance: realtimeBalanceBigInt,
        timestamp: BigInt(timestamp.toString()),
    };
}

async function fetchOnchainTokenState(rpcUrl, tokenAddress) {
    const payload = [
        {
            jsonrpc: "2.0",
            id: "totalSupply",
            method: "eth_call",
            params: [
                {
                    to: tokenAddress,
                    data: encodeCall("totalSupply", []),
                },
                "latest",
            ],
        },
        {
            jsonrpc: "2.0",
            id: "symbol",
            method: "eth_call",
            params: [
                {
                    to: tokenAddress,
                    data: encodeCall("symbol", []),
                },
                "latest",
            ],
        },
        {
            jsonrpc: "2.0",
            id: "decimals",
            method: "eth_call",
            params: [
                {
                    to: tokenAddress,
                    data: encodeCall("decimals", []),
                },
                "latest",
            ],
        },
    ];

    const results = await rpcBatch(rpcUrl, payload);
    const [totalSupply] = decodeResult("totalSupply", results.get("totalSupply").result);
    const [symbol] = decodeResult("symbol", results.get("symbol").result);
    const [decimals] = decodeResult("decimals", results.get("decimals").result);

    return {
        totalSupply: BigInt(totalSupply.toString()),
        symbol,
        decimals,
    };
}

function formatUnits(value, decimals) {
    const formatted = ethers.utils.formatUnits(value.toString(), decimals);
    if (!formatted.includes(".")) {
        return formatted;
    }

    return formatted.replace(/(\.\d*?[1-9])0+$/u, "$1").replace(/\.0$/u, "");
}

function formatPercent(diff, totalSupply, decimals = 6) {
    if (totalSupply === 0n) {
        return null;
    }

    const sign = diff < 0n ? "-" : "";
    const absoluteDiff = diff < 0n ? -diff : diff;
    const factor = 10n ** BigInt(decimals);
    const scaled = (absoluteDiff * 100n * factor) / totalSupply;
    const whole = scaled / factor;
    const fraction = String(scaled % factor).padStart(decimals, "0").replace(/0+$/u, "");

    return fraction.length > 0 ? `${sign}${whole}.${fraction}%` : `${sign}${whole}%`;
}

function printSummary({
    network,
    toga,
    token,
    accountCount,
    poolCount,
    uniqueAddressCount,
    accountSet,
    tokenStatistic,
    onchain,
    totals,
    togaState,
}) {
    const balanceVsSupply = totals.cumulativeBalanceOf - onchain.totalSupply;
    const balancePlusDepositVsSupply =
        totals.cumulativeBalanceOf + totals.cumulativeDeposit - onchain.totalSupply;
    const positiveRealtimeVsSupply =
        totals.cumulativePositiveAvailableBalance - onchain.totalSupply;
    const realtimeVsSupply = totals.cumulativeRealtimeBalance - onchain.totalSupply;
    const positiveRealtimeBalanceVsSupply =
        totals.cumulativePositiveRealtimeBalance - onchain.totalSupply;

    const summary = {
        network,
        token: {
            address: token.id,
            symbol: onchain.symbol,
            name: token.name,
            decimals: onchain.decimals,
        },
        subgraph: {
            tokenStatisticUpdatedAtTimestamp: tokenStatistic?.updatedAtTimestamp ?? null,
            tokenStatisticUpdatedAtBlockNumber: tokenStatistic?.updatedAtBlockNumber ?? null,
            totalNumberOfAccounts: tokenStatistic?.totalNumberOfAccounts ?? accountCount,
            totalNumberOfHolders: tokenStatistic?.totalNumberOfHolders ?? null,
            enumeratedAccountCount: accountCount,
            enumeratedPoolCount: poolCount,
            uniqueQueriedAddressCount: uniqueAddressCount,
        },
        specialAccounts: {
            toga: {
                address: toga,
                includedInEnumeratedAccounts: accountSet.has(toga.toLowerCase()),
                stateWei: {
                    balanceOf: togaState.balanceOf.toString(),
                    availableBalance: togaState.availableBalance.toString(),
                    deposit: togaState.deposit.toString(),
                    owedDeposit: togaState.owedDeposit.toString(),
                    realtimeBalance: togaState.realtimeBalance.toString(),
                    timestamp: togaState.timestamp.toString(),
                },
                stateFormatted: {
                    balanceOf: formatUnits(togaState.balanceOf, onchain.decimals),
                    availableBalance: formatUnits(
                        togaState.availableBalance,
                        onchain.decimals,
                    ),
                    deposit: formatUnits(togaState.deposit, onchain.decimals),
                    owedDeposit: formatUnits(
                        togaState.owedDeposit,
                        onchain.decimals,
                    ),
                    realtimeBalance: formatUnits(
                        togaState.realtimeBalance,
                        onchain.decimals,
                    ),
                },
            },
        },
        totalsWei: {
            balanceOf: totals.cumulativeBalanceOf.toString(),
            realtimeAvailableBalance: totals.cumulativeAvailableBalance.toString(),
            positiveRealtimeAvailableBalance:
                totals.cumulativePositiveAvailableBalance.toString(),
            deposit: totals.cumulativeDeposit.toString(),
            owedDeposit: totals.cumulativeOwedDeposit.toString(),
            balancePlusDeposit: (
                totals.cumulativeBalanceOf + totals.cumulativeDeposit
            ).toString(),
            realtimeBalance: totals.cumulativeRealtimeBalance.toString(),
            positiveRealtimeBalance: totals.cumulativePositiveRealtimeBalance.toString(),
            totalSupply: onchain.totalSupply.toString(),
            balanceOfMinusTotalSupply: balanceVsSupply.toString(),
            balancePlusDepositMinusTotalSupply: balancePlusDepositVsSupply.toString(),
            realtimeBalanceMinusTotalSupply: realtimeVsSupply.toString(),
            positiveRealtimeBalanceMinusTotalSupply:
                positiveRealtimeBalanceVsSupply.toString(),
            positiveRealtimeAvailableBalanceMinusTotalSupply:
                positiveRealtimeVsSupply.toString(),
        },
        totalsFormatted: {
            balanceOf: formatUnits(totals.cumulativeBalanceOf, onchain.decimals),
            realtimeAvailableBalance: formatUnits(
                totals.cumulativeAvailableBalance,
                onchain.decimals,
            ),
            positiveRealtimeAvailableBalance: formatUnits(
                totals.cumulativePositiveAvailableBalance,
                onchain.decimals,
            ),
            deposit: formatUnits(totals.cumulativeDeposit, onchain.decimals),
            owedDeposit: formatUnits(totals.cumulativeOwedDeposit, onchain.decimals),
            balancePlusDeposit: formatUnits(
                totals.cumulativeBalanceOf + totals.cumulativeDeposit,
                onchain.decimals,
            ),
            realtimeBalance: formatUnits(
                totals.cumulativeRealtimeBalance,
                onchain.decimals,
            ),
            positiveRealtimeBalance: formatUnits(
                totals.cumulativePositiveRealtimeBalance,
                onchain.decimals,
            ),
            totalSupply: formatUnits(onchain.totalSupply, onchain.decimals),
            balanceOfMinusTotalSupply: formatUnits(balanceVsSupply, onchain.decimals),
            balancePlusDepositMinusTotalSupply: formatUnits(
                balancePlusDepositVsSupply,
                onchain.decimals,
            ),
            realtimeBalanceMinusTotalSupply: formatUnits(
                realtimeVsSupply,
                onchain.decimals,
            ),
            positiveRealtimeBalanceMinusTotalSupply: formatUnits(
                positiveRealtimeBalanceVsSupply,
                onchain.decimals,
            ),
            positiveRealtimeAvailableBalanceMinusTotalSupply: formatUnits(
                positiveRealtimeVsSupply,
                onchain.decimals,
            ),
        },
        percentages: {
            balanceOfMinusTotalSupply: formatPercent(balanceVsSupply, onchain.totalSupply),
            balancePlusDepositMinusTotalSupply: formatPercent(
                balancePlusDepositVsSupply,
                onchain.totalSupply,
            ),
            realtimeBalanceMinusTotalSupply: formatPercent(
                realtimeVsSupply,
                onchain.totalSupply,
            ),
            positiveRealtimeBalanceMinusTotalSupply: formatPercent(
                positiveRealtimeBalanceVsSupply,
                onchain.totalSupply,
            ),
        },
        notes: [
            "balanceOf is the ERC20-compatible balance and clamps negative realtime balances to zero.",
            "realtimeAvailableBalance comes from realtimeBalanceOfNow(account) and can be negative.",
            "realtimeBalance = availableBalance + max(deposit - owedDeposit, 0).",
            "balancePlusDeposit = balanceOf + deposit, without subtracting owedDeposit.",
            "positiveRealtimeAvailableBalance is realtimeAvailableBalance clamped at zero before summing.",
        ],
    };

    console.log(JSON.stringify(summary, null, 2));
}

async function main() {
    const options = parseArgs(process.argv.slice(2));
    if (options.help) {
        printHelp();
        return;
    }

    const network = getNetworkConfig(options.network);
    const rpcUrl = options.rpcUrl ?? network.rpcUrl;
    const subgraphUrl = options.subgraphUrl ?? network.subgraphUrl;

    console.error(`Resolving token ${options.token} on ${network.name}...`);
    const token = await resolveToken(subgraphUrl, options.token);

    console.error(`Fetching token statistics for ${token.symbol} (${token.id})...`);
    const tokenStatistic = await fetchTokenStatistic(subgraphUrl, token.id);

    console.error("Enumerating account-token snapshots from the protocol subgraph...");
    const snapshots = await fetchAllAccountTokenSnapshots(
        subgraphUrl,
        token.id,
        options.pageSize,
    );
    console.error("Enumerating pools from the protocol subgraph...");
    const pools = await fetchAllPools(subgraphUrl, token.id, options.pageSize);
    const accounts = snapshots.map((snapshot) => ethers.utils.getAddress(snapshot.account.id));
    const poolAddresses = pools.map((pool) => ethers.utils.getAddress(pool.id));
    const addressSet = new Set();
    for (const account of accounts) {
        addressSet.add(account.toLowerCase());
    }
    for (const poolAddress of poolAddresses) {
        addressSet.add(poolAddress.toLowerCase());
    }
    const queriedAddresses = [...addressSet].map((address) => ethers.utils.getAddress(address));

    console.error(`Fetching onchain token state from ${rpcUrl}...`);
    const onchain = await fetchOnchainTokenState(rpcUrl, token.id);
    const toga = networks.find((item) => item.name === network.name)?.contractsV1?.toga;
    if (!toga) {
        throw new Error(`No TOGA configured for network ${network.name}`);
    }

    console.error(`Batch querying live balances for ${queriedAddresses.length} addresses...`);
    const totals = await fetchLiveTotals(
        rpcUrl,
        token.id,
        queriedAddresses,
        options.rpcBatchSize,
    );
    console.error(`Fetching TOGA state for ${toga}...`);
    const togaState = await fetchSingleAccountState(rpcUrl, token.id, toga);

    printSummary({
        network: network.name,
        toga,
        token,
        accountCount: accounts.length,
        poolCount: poolAddresses.length,
        uniqueAddressCount: queriedAddresses.length,
        accountSet: addressSet,
        tokenStatistic,
        onchain,
        totals,
        togaState,
    });
}

main().catch((error) => {
    console.error(error instanceof Error ? error.message : error);
    process.exitCode = 1;
});
