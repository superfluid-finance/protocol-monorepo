// eslint-disable-next-line no-global-assign
if (typeof module === "undefined") module = {};

// eslint-disable-next-line no-undef
Superfluid_getConfig = module.exports = function getConfig(chainId, version) {
    const DEFAULT_CONFIGS = {
        //
        // Local testing
        //
        1337: {
            // for default ganache setup
            nativeTokenSymbol: "ETH",
        },
        4447: {
            // for local testing (truffle internal ganache and TestEnvironment)
            nativeTokenSymbol: "ETH",
        },
        5777: {
            // for local testing (external ganache)
            nativeTokenSymbol: "ETH",
        },
        31337: {
            // for local testing hardhat
            nativeTokenSymbol: "ETH",
        },

        //
        // ETHEREUM
        //
        5: {
            // goerli
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-goerli",
                },
            },
        },
        4: {
            // rinkeby
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-rinkeby",
                },
            },
        },

        3: {
            // ropsten
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-ropsten",
                },
            },
        },
        42: {
            // kovan
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-kovan",
                },
            },
        },

        //
        // MATIC: https://docs.matic.network/docs/develop/network-details/network/
        //
        137: {
            // (matic) mainnet
            nativeTokenSymbol: "MATIC",
            resolverAddress: "0xE0cc76334405EE8b39213E620587d815967af39C",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-matic",
                },
            },
        },
        80001: {
            // (matic) mumbai testnet
            nativeTokenSymbol: "MATIC",
            resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-mumbai",
                },
            },
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        0x64: {
            nativeTokenSymbol: "xDAI",
            resolverAddress: "0xD2009765189164b495c110D61e4D301729079911",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-xdai",
                },
            },
        },

        // ARTIS
        0x03c401: {
            // (artis) tau1 testnet
            resolverAddress: "0x79D426CD219eDCFEB2dCbcf7ea0F8B3642C56F47",
        },

        //
        // Optimistic Ethereum: https://community.optimism.io/docs/
        //
        10: {
            // op mainnet
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x743B5f46BC86caF41bE4956d9275721E0531B186",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-mainnet",
                },
            },
        },
        69: {
            // op kovan testnet
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x218B65780615Ff134f9Ad810CB98839534D3C0D6",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-kovan",
                },
            },
        },

        //
        // Arbitrum: https://developer.offchainlabs.com
        //
        42161: {
            // arbitrum one mainnet
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x609b9d9d6Ee9C3200745A79B9d3398DBd63d509F",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-arbitrum-one",
                },
            },
        },
        421611: {
            // arbitrum rinkeby testnet
            nativeTokenSymbol: "ETH",
            resolverAddress: "0xa2C0C70A1E922f5f060ec20EE3aF002C163b4567",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-arbitrum-rinkeby",
                },
            },
        },

        //
        // Avalanche C-Chain: https://docs.avax.network/learn/platform-overview#contract-chain-c-chain
        //
        43114: {
            // avalanche c-chain mainnet
            nativeTokenSymbol: "AVAX",
        },
        43113: {
            // avalanche c-chain fuji testnet
            nativeTokenSymbol: "AVAX",
            resolverAddress: "0x141920741bC45b962B59c833cd849bA617F7ef38",
            versions: {
                v1: {
                    subgraphQueryEndpoint:
                        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-avalanche-fuji",
                },
            },
        },

        //
        // Celo: https://github.com/celo-org/celo-monorepo#docs
        //
        42220: {
            // celo mainnet
            nativeTokenSymbol: "CELO",
        },
    };

    let configs = {
        ...DEFAULT_CONFIGS[chainId],
    };
    // load version specific configs
    if (configs.versions) {
        configs = {
            ...configs,
            ...configs.versions[version],
        };
        delete configs.versions;
    }
    // overriding environment variables
    if (global && global.process && global.process.env.RESOLVER_ADDRESS) {
        configs.resolverAddress = global.process.env.RESOLVER_ADDRESS;
    }

    return configs;
};
