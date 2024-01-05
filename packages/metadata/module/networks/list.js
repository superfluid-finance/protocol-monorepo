/* eslint-disable */
export default
[
    {
        "name": "eth-goerli",
        "isTestnet": true,
        "networkId": 5,
        "chainId": 5,
        "sfId": 101,
        "shortName": "goerli",
        "uppercaseName": "ETH_GOERLI",
        "humanReadableName": "Goerli",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0x5943f705abb6834cad767e6e4bb258bc48d9c947",
        "contractsV1": {
            "resolver": "0x071077b24943D4DC09a6A57B8aFfE2BdE9bD4B89",
            "host": "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9",
            "governance": "0x3a648764a6d66440ca096343937c711a7ac1b1e9",
            "cfaV1": "0xEd6BcbF6907D4feEEe8a8875543249bEa9D308E8",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0xfDdcdac21D64B639546f3Ce2868C7EF06036990c",
            "gdaV1": "0x3dB8Abd8B696F6c4150212A85961f954825Dd4B9",
            "gdaV1Forwarder": "0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08",
            "superTokenFactory": "0x94f26B4c8AD12B18c12f38E878618f7664bdcCE2",
            "constantOutflowNFT": "0xB18cbFeA12b5CB2626C74c94920dB1B37Ae91506",
            "constantInflowNFT": "0xF07df8b66ed80399B1E00981D61aD34EB4293032",
            "superfluidLoader": "0x08C6Ad85c040076dD3e33Bbd963C4c37DBF6fCc4",
            "toga": "0xa54FC15FC75693447d70a57262F37a70B614721b",
            "flowScheduler": "0xf428308b426D7cD7Ad8eBE549d750f31C8E060Ca",
            "vestingScheduler": "0xF9240F930d847F70ad900aBEE8949F25649Bf24a",
            "autowrap": {
                "manager": "0x0B82D14E9616ca4d260E77454834AdCf5887595F",
                "wrapStrategy": "0xea49af829d3e28d3ec49e0e0a0ba1e7860a56f60"
            },
            "existentialNFTCloneFactory": "0x3e5e8449477F80a3Dc0bA394882AeAB300c7F807"
        },
        "startBlockV1": 3550000,
        "logsQueryRange": 10000,
        "explorer": "https://goerli.etherscan.io",
        "subgraphV1": {
            "cliName": "goerli",
            "name": "protocol-v1-goerli",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-goerli"
        },
        "subgraphVesting": {
            "name": "vesting-v1-eth-goerli",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-eth-goerli"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-eth-goerli",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-eth-goerli"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-eth-goerli",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-eth-goerli"
        },
        "publicRPCs": [
            "https://rpc.ankr.com/eth_goerli",
            "https://goerli.infura.io/v3/9aa3d95b3bc440fa88ea12eaa4456161"
        ],
        "trustedForwarders": [
            "0xE041608922d06a4F26C0d4c27d8bCD01daf1f792",
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "polygon-mumbai",
        "isTestnet": true,
        "networkId": 80001,
        "chainId": 80001,
        "sfId": 102,
        "shortName": "mumbai",
        "uppercaseName": "POLYGON_MUMBAI",
        "humanReadableName": "Polygon Mumbai",
        "nativeTokenSymbol": "MATIC",
        "nativeTokenWrapper": "0x96B82B65ACF7072eFEb00502F45757F254c2a0D4",
        "contractsV1": {
            "resolver": "0x2feFd8B23e4770B674b0d2fAda5EE8a2bE0B525F",
            "host": "0xEB796bdb90fFA0f28255275e16936D25d3418603",
            "cfaV1": "0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x804348D4960a61f2d5F9ce9103027A3E849E09b8",
            "gdaV1": "0x63ab406B6eF6c8be732c1edbd15464de16a8F46D",
            "gdaV1Forwarder": "0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08",
            "superTokenFactory": "0xB798553db6EB3D3C56912378409370145E97324B",
            "constantOutflowNFT": "0x502CC982947216C0f94e433BC78c413806301C07",
            "constantInflowNFT": "0x9906A7e948C642B6bc74b9A5EAfCddB3580b44e0",
            "superfluidLoader": "0xFcc617a382988a6d081B1821c8d17C47DFF9EDE1",
            "toga": "0x38DD80876DBA048d0050D28828522c313967D073",
            "superSpreader": "0x74CDF863b00789c29734F8dFd9F83423Bc55E4cE",
            "flowScheduler": "0x59A3Ba9d34c387FB70b4f4e4Fbc9eD7519194139",
            "vestingScheduler": "0x3962EE56c9f7176215D149938BA685F91aBB633B",
            "autowrap": {
                "manager": "0x3eAB3c6207F488E475b7955B631B564F0E6317B9",
                "wrapStrategy": "0x544728AFDBeEafBeC9e1329031788edb53017bC4"
            },
            "existentialNFTCloneFactory": "0xfC4E49AfcDDDcc2B90794DC214cc324d17B15619"
        },
        "startBlockV1": 8100000,
        "logsQueryRange": 10000,
        "explorer": "https://mumbai.polygonscan.com",
        "subgraphV1": {
            "cliName": "mumbai",
            "name": "protocol-v1-mumbai",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-mumbai"
        },
        "subgraphVesting": {
            "name": "vesting-v1-polygon-mumbai",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-polygon-mumbai"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-polygon-mumbai",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-polygon-mumbai"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-polygon-mumbai",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-polygon-mumbai"
        },
        "publicRPCs": [
            "https://rpc.ankr.com/polygon_mumbai",
            "https://matic-mumbai.chainstacklabs.com"
        ],
        "trustedForwarders": [
            "0x9399bb24dbb5c4b782c70c2969f58716ebbd6a3b",
            "0x69015912AA33720b842dCD6aC059Ed623F28d9f7",
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "optimism-goerli",
        "isTestnet": true,
        "networkId": 420,
        "chainId": 420,
        "sfId": 103,
        "shortName": "opgoerli",
        "uppercaseName": "OPTIMISM_GOERLI",
        "humanReadableName": "Optimism Goerli",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0xE01F8743677Da897F4e7De9073b57Bf034FC2433",
        "contractsV1": {
            "resolver": "0x21d4E9fbB9DB742E6ef4f29d189a7C18B0b59136",
            "host": "0xE40983C2476032A0915600b9472B3141aA5B5Ba9",
            "cfaV1": "0xff48668fa670A85e55A7a822b352d5ccF3E7b18C",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x96215257F2FcbB00135578f766c0449d239bd92F",
            "gdaV1": "0xe87F46A15C410F151309Bf7516e130087Fc6a5E5",
            "gdaV1Forwarder": "0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08",
            "superTokenFactory": "0xfafe31cf998Df4e5D8310B03EBa8fb5bF327Eaf5",
            "constantOutflowNFT": "0xDF874BA132D8C68FEb5De513790f7612Fe20dDbd",
            "constantInflowNFT": "0xf88dd7208438Fdc5Ad05857eA701b7b51cdae0a9",
            "superfluidLoader": "0x5Bb5908dcCC9Bb0fC39a78CfDf9e47B4C08E9521",
            "existentialNFTCloneFactory": "0xFCb633f4ff90241391aD92E4678eC2a11a94a990"
        },
        "startBlockV1": 340000,
        "logsQueryRange": 50000,
        "explorer": "https://goerli-optimism.etherscan.io",
        "subgraphV1": {
            "cliName": "optimism-goerli",
            "name": "protocol-v1-optimism-goerli",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-goerli"
        },
        "publicRPCs": [
            "https://goerli.optimism.io",
            "https://opt-goerli.g.alchemy.com/v2/demo"
        ],
        "trustedForwarders": [
            "0x9C73373C70F23920EA54F7883dCB1F85b162Df40",
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "arbitrum-goerli",
        "isTestnet": true,
        "networkId": 421613,
        "chainId": 421613,
        "sfId": 104,
        "shortName": "arbgoerli",
        "uppercaseName": "ARBITRUM_GOERLI",
        "humanReadableName": "Arbitrum Goerli",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0xE01F8743677Da897F4e7De9073b57Bf034FC2433",
        "contractsV1": {
            "resolver": "0x21d4E9fbB9DB742E6ef4f29d189a7C18B0b59136",
            "host": "0xE40983C2476032A0915600b9472B3141aA5B5Ba9",
            "cfaV1": "0xff48668fa670A85e55A7a822b352d5ccF3E7b18C",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x96215257F2FcbB00135578f766c0449d239bd92F",
            "gdaV1": "0xe87F46A15C410F151309Bf7516e130087Fc6a5E5",
            "gdaV1Forwarder": "0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08",
            "superTokenFactory": "0xfafe31cf998Df4e5D8310B03EBa8fb5bF327Eaf5",
            "constantOutflowNFT": "0xDF874BA132D8C68FEb5De513790f7612Fe20dDbd",
            "constantInflowNFT": "0xf88dd7208438Fdc5Ad05857eA701b7b51cdae0a9",
            "superfluidLoader": "0x5Bb5908dcCC9Bb0fC39a78CfDf9e47B4C08E9521",
            "existentialNFTCloneFactory": "0x22f391AB6755F0A8986AFD543289C43701beaCAa"
        },
        "startBlockV1": 93000,
        "logsQueryRange": 50000,
        "explorer": "https://goerli.arbiscan.io",
        "subgraphV1": {
            "cliName": "arbitrum-goerli",
            "name": "protocol-v1-arbitrum-goerli",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-arbitrum-goerli"
        },
        "publicRPCs": [
            "https://goerli-rollup.arbitrum.io/rpc",
            "https://arb-goerli.g.alchemy.com/v2/demo"
        ],
        "trustedForwarders": [
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "avalanche-fuji",
        "isTestnet": true,
        "networkId": 43113,
        "chainId": 43113,
        "sfId": 105,
        "shortName": "fuji",
        "uppercaseName": "AVALANCHE_FUJI",
        "humanReadableName": "Avalanche Fuji",
        "nativeTokenSymbol": "AVAX",
        "nativeTokenWrapper": "0xfFD0f6d73ee52c68BF1b01C8AfA2529C97ca17F3",
        "contractsV1": {
            "resolver": "0xf0ec6A8842Ca72Aec8A4D4573E731242389e18A8",
            "host": "0x85Fe79b998509B77BF10A8BD4001D58475D29386",
            "cfaV1": "0x16843ac25Ccc58Aa7960ba05f61cBB17b36b130A",
            "cfaV1Forwarder": "0x2CDd45c5182602a36d391F7F16DD9f8386C3bD8D",
            "idaV1": "0xA44dEC7A0Dde1a56AeDe4143C1ef89cf5d956782",
            "gdaV1": "0x51f571D934C59185f13d17301a36c07A2268B814",
            "gdaV1Forwarder": "0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08",
            "superTokenFactory": "0x1C92042426B6bAAe497bEf461B6d8342D03aEc92",
            "constantOutflowNFT": "0x49583f57EFeBe733EC872c5d5437116085a3eE3c",
            "constantInflowNFT": "0x67d0Efab10b390206b356BA7FB453Ab56AAB7480",
            "superfluidLoader": "0x36446Ec9C7909608065dEB7f491701d815B880e5",
            "autowrap": {
                "manager": "0x30aE282CF477E2eF28B14d0125aCEAd57Fe1d7a1",
                "wrapStrategy": "0x1D65c6d3AD39d454Ea8F682c49aE7744706eA96d"
            },
            "existentialNFTCloneFactory": "0xF76529ddEE7AA7890323eCa40C212758DD93B888"
        },
        "startBlockV1": 3220000,
        "logsQueryRange": 50000,
        "explorer": "https://testnet.snowtrace.io",
        "subgraphV1": {
            "cliName": "fuji",
            "name": "protocol-v1-avalanche-fuji",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-avalanche-fuji"
        },
        "publicRPCs": [
            "https://api.avax-test.network/ext/bc/C/rpc",
            "https://avalanche-fuji-c-chain.publicnode.com"
        ],
        "trustedForwarders": [
            "0x6271Ca63D30507f2Dcbf99B52787032506D75BBF",
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "eth-sepolia",
        "isTestnet": true,
        "networkId": 11155111,
        "chainId": 11155111,
        "sfId": 106,
        "shortName": "sepolia",
        "uppercaseName": "ETH_SEPOLIA",
        "humanReadableName": "Sepolia",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0x30a6933Ca9230361972E413a15dC8114c952414e",
        "contractsV1": {
            "resolver": "0x6813edE4E78ecb830d380d0F7F684c12aAc95F02",
            "host": "0x109412E3C84f0539b43d39dB691B08c90f58dC7c",
            "cfaV1": "0x6836F23d6171D74Ef62FcF776655aBcD2bcd62Ef",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x9358C7dCCc6B8CA6F526311e8ac266F8C861B7ea",
            "superTokenFactory": "0x254C2e152E8602839D288A7bccdf3d0974597193",
            "constantOutflowNFT": "0xfBE332e001D6b54e1F4B63c2343B8E7746d99Ece",
            "constantInflowNFT": "0xC95346B7394009ccEfaA62Eca28797804B2bCF1C",
            "superfluidLoader": "0x554c06487bEc8c890A0345eb05a5292C1b1017Bd",
            "existentialNFTCloneFactory": "0x0D1F0d4629B722b4dFabd195c14F12f2095418d9"
        },
        "startBlockV1": 3322400,
        "logsQueryRange": 10000,
        "explorer": "https://sepolia.etherscan.io",
        "subgraphV1": {
            "cliName": "sepolia",
            "name": "protocol-v1-eth-sepolia",
            "satsumaEndpoint": "https://subgraph.satsuma-prod.com/c5br3jaVlJI6/superfluid/eth-sepolia/api"
        },
        "publicRPCs": [
            "https://rpc.sepolia.org",
            "https://ethereum-sepolia.blockpi.network/v1/rpc/public"
        ]
    },
    {
        "name": "base-goerli",
        "isTestnet": true,
        "networkId": 84531,
        "chainId": 84531,
        "sfId": 107,
        "shortName": "bgoerli",
        "uppercaseName": "BASE_GOERLI",
        "humanReadableName": "Base Goerli",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0x7fFCE315B2014546bA461d54eDed7AAc70DF4f53",
        "contractsV1": {
            "resolver": "0x598D5dB9902cbBd6e8Ee9CDb3A231377cdA2f018",
            "host": "0x507c3a7C6Ccc253884A2e3a3ee2A211cC7E796a6",
            "cfaV1": "0x4C476F2Fb27272680F2f6f2592E94d9e704691bC",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0xaa4FCc799B8857FA87b2945Dc6572D5d76b35485",
            "superTokenFactory": "0x1015BE31D7711D95d2c3444708FB53cC851ba856",
            "constantOutflowNFT": "0x4E89088Cd14064f38E5B2F309cFaB9C864F9a8e6",
            "constantInflowNFT": "0xda6db863cb2EE39b196edB8159c38A1ed5c55344",
            "superfluidLoader": "0x15F0Ca26781C3852f8166eD2ebce5D18265cceb7",
            "existentialNFTCloneFactory": "0x42B332F56BCD15F0D7cd523E5eC26Cd0734621eD"
        },
        "startBlockV1": 5249500,
        "logsQueryRange": 10000,
        "explorer": "https://goerli.basescan.org/",
        "subgraphV1": {
            "name": "protocol-v1-base-goerli"
        },
        "publicRPCs": [
            "https://goerli.base.org",
            "https://base-goerli.public.blastapi.io"
        ],
        "trustedForwarders": [
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "polygon-zkevm-testnet",
        "isTestnet": true,
        "networkId": 1442,
        "chainId": 1442,
        "sfId": 108,
        "shortName": "pzkevmtest",
        "uppercaseName": "POLYGON_ZKEVM_TESTNET",
        "humanReadableName": "Polygon zkEVM Testnet",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0x6345Aa6cec42a85160CF436810F97661e28c1876",
        "contractsV1": {
            "resolver": "0x642332562BC60a4Bd9681E7bb1588f7456A497aC",
            "host": "0xe64f81d5dDdA1c7172e5C6d964E8ef1BD82D8704",
            "cfaV1": "0x1EAa5ceA064aab2692AF257FB31f5291fdA3Cdee",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0xBf22019a4A4430bA67D3B0c8B4d5Edc48F913301",
            "superTokenFactory": "0x0F3B163623F05b2BfF42956f7C7bd31456bd83a2",
            "constantOutflowNFT": "0xDBD6f113E46A99D7BF95edfa47390c0c8127E922",
            "constantInflowNFT": "0xcb05535bd212eCFC4B7b9db81d6C2C768b726776",
            "superfluidLoader": "0xda6db863cb2EE39b196edB8159c38A1ed5c55344",
            "existentialNFTCloneFactory": "0x70bbB7a057A13070dF11d533e8f299357D778637"
        },
        "startBlockV1": 726000,
        "logsQueryRange": 20000,
        "explorer": "https://testnet-zkevm.polygonscan.org/",
        "subgraphV1": {
            "cliName": "polygon-zkevm-testnet",
            "name": "protocol-v1-polygon-zkevm-testnet"
        },
        "publicRPCs": ["https://rpc.public.zkevm-test.net"],
        "trustedForwarders": [
            "0xb539068872230f20456CF38EC52EF2f91AF4AE49",
            "0xd8253782c45a12053594b9deB72d8e8aB2Fca54c"
        ]
    },
    {
        "name": "xdai-mainnet",
        "isTestnet": false,
        "networkId": 100,
        "chainId": 100,
        "sfId": 1,
        "shortName": "xdai",
        "uppercaseName": "XDAI_MAINNET",
        "humanReadableName": "Gnosis Chain",
        "nativeTokenSymbol": "xDAI",
        "nativeTokenWrapper": "0x59988e47A3503AaFaA0368b9deF095c818Fdca01",
        "contractsV1": {
            "resolver": "0x9634Fa35188C59DD3B0ADa0Dd9adD7e4ECefc0a9",
            "host": "0x2dFe937cD98Ab92e59cF3139138f18c823a4efE7",
            "governance": "0xaCc7380323681fdb8a0B9F2FE7d69dDFf0664478",
            "cfaV1": "0xEbdA4ceF883A7B12c4E669Ebc58927FBa8447C7D",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x7888ac96F987Eb10E291F34851ae0266eF912081",
            "superTokenFactory": "0x23410e2659380784498509698ed70E414D384880",
            "constantOutflowNFT": "0xfC00dEE8a980110c5608A823a5B3af3872635456",
            "constantInflowNFT": "0x1497440B4E92DC4ca0F76223b28C20Cb9cB8a0f1",
            "superfluidLoader": "0xf941528d82D0eD2314FaBBA8bC8f285D1956A657",
            "toga": "0xb7DE52F4281a7a276E18C40F94cd93159C4A2d22",
            "batchLiquidator": "0x27636F8E129cdd4ccA0F30E2b4C116DDaC773bE5",
            "superSpreader": "0x74CDF863b00789c29734F8dFd9F83423Bc55E4cE",
            "flowScheduler": "0x9cC7fc484fF588926149577e9330fA5b2cA74336",
            "vestingScheduler": "0x0170FFCC75d178d426EBad5b1a31451d00Ddbd0D",
            "wrapManager": "0x7a2899D179a8F205C8EDAd2e52954cA5f6d48D1A",
            "autowrap": {
                "manager": "0x8082e58681350876aFe8f52d3Bf8672034A03Db0",
                "wrapStrategy": "0x51FBAbD31A615E14b1bC12E9d887f60997264a4E"
            },
            "existentialNFTCloneFactory": "0xCd67c5bC1dfA3FF7d86b5ABc62A65C912Cbd2DA7"
        },
        "startBlockV1": 14820000,
        "logsQueryRange": 20000,
        "explorer": "https://gnosisscan.io",
        "subgraphV1": {
            "cliName": "gnosis",
            "name": "protocol-v1-xdai",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-xdai",
            "satsumaEndpoint": "https://subgraph.satsuma-prod.com/c5br3jaVlJI6/superfluid/xdai/api"
        },
        "subgraphVesting": {
            "name": "vesting-v1-xdai-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-xdai-mainnet"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-xdai-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-xdai-mainnet"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-xdai-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-xdai-mainnet"
        },
        "publicRPCs": [
            "https://rpc.gnosischain.com",
            "https://gnosischain-rpc.gateway.pokt.network"
        ],
        "coinGeckoId": "xdai"
    },
    {
        "name": "polygon-mainnet",
        "isTestnet": false,
        "networkId": 137,
        "chainId": 137,
        "sfId": 2,
        "shortName": "matic",
        "uppercaseName": "POLYGON_MAINNET",
        "humanReadableName": "Polygon",
        "nativeTokenSymbol": "MATIC",
        "nativeTokenWrapper": "0x3aD736904E9e65189c3000c7DD2c8AC8bB7cD4e3",
        "contractsV1": {
            "resolver": "0x8bDCb5613153f41B2856F71Bd7A7e0432F6dbe58",
            "host": "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
            "governance": "0x3AD3f7A0965Ce6f9358AD5CCE86Bc2b05F1EE087",
            "cfaV1": "0x6EeE6060f715257b970700bc2656De21dEdF074C",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0xB0aABBA4B2783A72C52956CDEF62d438ecA2d7a1",
            "superTokenFactory": "0x2C90719f25B10Fc5646c82DA3240C76Fa5BcCF34",
            "constantOutflowNFT": "0x554e2bbaCF43FD87417b7201A9F1649a3ED89d68",
            "constantInflowNFT": "0x55909bB8cd8276887Aae35118d60b19755201c68",
            "superfluidLoader": "0xA3d2A6Db274722e802AC025d667041ecAb094FE7",
            "toga": "0x6AEAeE5Fd4D05A741723D752D30EE4D72690A8f7",
            "batchLiquidator": "0xA6Cdb472e7E22Bf30ae6fB752E4a13eBF3c12165",
            "flowScheduler": "0x55F7758dd99d5e185f4CC08d4Ad95B71f598264D",
            "vestingScheduler": "0xcFE6382B33F2AdaFbE46e6A26A88E0182ae32b0c",
            "autowrap": {
                "manager": "0x2581c27E7f6D6AF452E63fCe884EDE3EDd716b32",
                "wrapStrategy": "0xb4afa36BAd8c76976Dc77a21c9Ad711EF720eE4b"
            },
            "existentialNFTCloneFactory": "0x497aa106Da00BFa8f8BC266EF0793131930Fa630"
        },
        "startBlockV1": 11650500,
        "logsQueryRange": 10000,
        "explorer": "https://polygonscan.com",
        "subgraphV1": {
            "cliName": "matic",
            "name": "protocol-v1-matic",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-matic",
            "satsumaEndpoint": "https://subgraph.satsuma-prod.com/c5br3jaVlJI6/superfluid/matic/api"
        },
        "subgraphVesting": {
            "name": "vesting-v1-polygon-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-polygon-mainnet"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-polygon-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-polygon-mainnet"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-polygon-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-polygon-mainnet"
        },
        "publicRPCs": [
            "https://polygon-rpc.com",
            "https://poly-rpc.gateway.pokt.network"
        ],
        "coinGeckoId": "polygon-pos"
    },
    {
        "name": "optimism-mainnet",
        "isTestnet": false,
        "networkId": 10,
        "chainId": 10,
        "sfId": 3,
        "shortName": "optimism",
        "uppercaseName": "OPTIMISM_MAINNET",
        "humanReadableName": "Optimism",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0x4ac8bD1bDaE47beeF2D1c6Aa62229509b962Aa0d",
        "contractsV1": {
            "resolver": "0x743B5f46BC86caF41bE4956d9275721E0531B186",
            "host": "0x567c4B141ED61923967cA25Ef4906C8781069a10",
            "governance": "0x0170FFCC75d178d426EBad5b1a31451d00Ddbd0D",
            "cfaV1": "0x204C6f131bb7F258b2Ea1593f5309911d8E458eD",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0xc4ce5118C3B20950ee288f086cb7FC166d222D4c",
            "superTokenFactory": "0x8276469A443D5C6B7146BED45e2abCaD3B6adad9",
            "constantOutflowNFT": "0xFb2b126660BE2fdEBa254b1F6e4348644E8482e7",
            "constantInflowNFT": "0x0C6D90a98426bfD572a5c5Be572a7f6Bd1C5ED76",
            "superfluidLoader": "0x8E310ce29Ab7Fa2878944A65BB0eaF97B1853d40",
            "toga": "0xA3c8502187fD7a7118eAD59dc811281448946C8f",
            "batchLiquidator": "0x36Df169DBf5CE3c6f58D46f0addeF58F01381232",
            "flowScheduler": "0x55c8fc400833eEa791087cF343Ff2409A39DeBcC",
            "vestingScheduler": "0x65377d4dfE9c01639A41952B5083D58964782892",
            "autowrap": {
                "manager": "0x1fA76f2Cd0C3fe6c399A80111408d9C42C0CAC23",
                "wrapStrategy": "0x0Cf060a501c0040e9CCC708eFE94079F501c6Bb4"
            },
            "existentialNFTCloneFactory": "0xCb0Ff4D0cA186f0Fc0301258066Fe3fA258417a6"
        },
        "startBlockV1": 4300000,
        "logsQueryRange": 50000,
        "explorer": "https://optimistic.etherscan.io",
        "subgraphV1": {
            "cliName": "optimism",
            "name": "protocol-v1-optimism-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-mainnet"
        },
        "subgraphVesting": {
            "name": "vesting-v1-optimism-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-optimism-mainnet"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-optimism-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-optimism-mainnet"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-optimism-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-optimism-mainnet"
        },
        "publicRPCs": [
            "https://mainnet.optimism.io",
            "https://rpc.ankr.com/optimism"
        ],
        "coinGeckoId": "optimistic-ethereum"
    },
    {
        "name": "arbitrum-one",
        "isTestnet": false,
        "networkId": 42161,
        "chainId": 42161,
        "sfId": 4,
        "shortName": "arbone",
        "uppercaseName": "ARBITRUM_ONE",
        "humanReadableName": "Arbitrum One",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0xe6C8d111337D0052b9D88BF5d7D55B7f8385ACd3",
        "contractsV1": {
            "resolver": "0x609b9d9d6Ee9C3200745A79B9d3398DBd63d509F",
            "host": "0xCf8Acb4eF033efF16E8080aed4c7D5B9285D2192",
            "governance": "0x0170FFCC75d178d426EBad5b1a31451d00Ddbd0D",
            "cfaV1": "0x731FdBB12944973B500518aea61942381d7e240D",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x2319C7e07EB063340D2a0E36709B0D65fda75986",
            "superTokenFactory": "0x1C21Ead77fd45C84a4c916Db7A6635D0C6FF09D6",
            "constantOutflowNFT": "0x051e766e2d8dc65ae2bFCF084A50AD0447634227",
            "constantInflowNFT": "0x0043d7c85C8b96a49A72A92C0B48CdC4720437d7",
            "superfluidLoader": "0xB99cA401e10D277345BcFb281AC148a2e16Db466",
            "toga": "0xFC63B7C762B10670Eda15cF3ca3970bCDB28C9eF",
            "batchLiquidator": "0x6C66e5c5D201A753ff497F2e9eC5D545631854d0",
            "flowScheduler": "0x3fA8B653F9abf91428800C0ba0F8D145a71F97A1",
            "vestingScheduler": "0x55c8fc400833eEa791087cF343Ff2409A39DeBcC",
            "autowrap": {
                "manager": "0xf01825eAFAe5CD1Dab5593EFAF218efC8968D272",
                "wrapStrategy": "0x342076aA957B0ec8bC1d3893af719b288eA31e61"
            },
            "existentialNFTCloneFactory": "0xF353978890204756fc5fa6dfbD16a91eac9E6f4d"
        },
        "startBlockV1": 7600000,
        "logsQueryRange": 50000,
        "explorer": "https://arbiscan.io",
        "subgraphV1": {
            "cliName": "arbitrum-one",
            "name": "protocol-v1-arbitrum-one",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-arbitrum-one"
        },
        "subgraphVesting": {
            "name": "vesting-v1-arbitrum-one",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-arbitrum-one"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-arbitrum-one",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-arbitrum-one"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-arbitrum-one",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-arbitrum-one"
        },
        "publicRPCs": [
            "https://arb1.arbitrum.io/rpc",
            "https://arbitrum.blockpi.network/v1/rpc/public"
        ],
        "coinGeckoId": "arbitrum-one"
    },
    {
        "name": "avalanche-c",
        "isTestnet": false,
        "networkId": 43114,
        "chainId": 43114,
        "sfId": 5,
        "shortName": "avalanche",
        "uppercaseName": "AVALANCHE_C",
        "humanReadableName": "Avalanche C",
        "nativeTokenSymbol": "AVAX",
        "nativeTokenWrapper": "0xBE916845D8678b5d2F7aD79525A62D7c08ABba7e",
        "contractsV1": {
            "resolver": "0x24a3F04F70B7f07B9673EadD3e146391BcfEa5c1",
            "host": "0x60377C7016E4cdB03C87EF474896C11cB560752C",
            "governance": "0xF74390BabA510ec2fE196c2e02B037380d7a6F12",
            "cfaV1": "0x6946c5B38Ffea373b0a2340b4AEf0De8F6782e58",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x1fA9fFe8Db73F701454B195151Db4Abb18423cf2",
            "superTokenFactory": "0x464AADdBB2B80f3Cb666522EB7381bE610F638b4",
            "constantOutflowNFT": "0x4247bA6C3658Fa5C0F523BAcea8D0b97aF1a175e",
            "constantInflowNFT": "0x82b9D8A91A5b333b5A6e78439551ea0E7da153E3",
            "superfluidLoader": "0x2601E69a2D16C8Ccc8aEe8cE9F59d24a59986B9E",
            "toga": "0x3D9A67D5ec1E72CEcA8157e028855056786b6159",
            "batchLiquidator": "0xdddaD64A9Fe7709A729C4a5428617e369278e0b6",
            "flowScheduler": "0xF7AfF590E9DE493D7ACb421Fca7f1E35C1ad4Ce5",
            "vestingScheduler": "0x3fA8B653F9abf91428800C0ba0F8D145a71F97A1",
            "autowrap": {
                "manager": "0x8082e58681350876aFe8f52d3Bf8672034A03Db0",
                "wrapStrategy": "0x51FBAbD31A615E14b1bC12E9d887f60997264a4E"
            },
            "existentialNFTCloneFactory": "0x94aE5f52E401A5766b0877d2f030cFb9C3792BD7"
        },
        "startBlockV1": 14700000,
        "logsQueryRange": 50000,
        "explorer": "https://snowtrace.io",
        "subgraphV1": {
            "cliName": "avalanche",
            "name": "protocol-v1-avalanche-c",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-avalanche-c"
        },
        "subgraphVesting": {
            "name": "vesting-v1-avalanche-c",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-avalanche-c"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-avalanche-c",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-avalanche-c"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-avalanche-c",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-avalanche-c"
        },
        "publicRPCs": [
            "https://api.avax.network/ext/bc/C/rpc",
            "https://rpc.ankr.com/avalanche"
        ],
        "coinGeckoId": "avalanche"
    },
    {
        "name": "bsc-mainnet",
        "isTestnet": false,
        "networkId": 56,
        "chainId": 56,
        "sfId": 5,
        "shortName": "bsc",
        "uppercaseName": "BSC_MAINNET",
        "humanReadableName": "BNB Smart Chain",
        "nativeTokenSymbol": "BNB",
        "nativeTokenWrapper": "0x529A4116F160c833c61311569D6B33dFF41fD657",
        "contractsV1": {
            "resolver": "0x69604aA4e9e8BF44A73C680997205Edb03A92E41",
            "host": "0xd1e2cFb6441680002Eb7A44223160aB9B67d7E6E",
            "governance": "0xee07D9fce4Cf2a891BC979E9d365929506C2982f",
            "cfaV1": "0x49c38108870e74Cb9420C0991a85D3edd6363F75",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x594ed9Cd773584B645aC1F5B11020d3b32cDF07d",
            "superTokenFactory": "0x8bde47397301F0Cd31b9000032fD517a39c946Eb",
            "constantOutflowNFT": "0xcb05535bd212eCFC4B7b9db81d6C2C768b726776",
            "constantInflowNFT": "0xbF7BCcE8D60A9C3F6bFaEc9346Aa85B9f781a4e9",
            "superfluidLoader": "0x3C26e5bEbc68EaAf4efcd88F8E5A6Aeb77b65579",
            "toga": "0xFCD84210f5d51Cd40a30443d44d6A5500d5D10dF",
            "batchLiquidator": "0x5487d078CA8933e83d91d5E7AFBe3A7bfC3412d6",
            "flowScheduler": "0x2f9e2A2A59405682d4F86779275CF5525AD7eC2B",
            "vestingScheduler": "0x9B91c27f78376383003C6A12Ad12B341d016C5b9",
            "autowrap": {
                "manager": "0x2AcdD61ac1EFFe1535109449c31889bdE8d7f325",
                "wrapStrategy": "0x9e308cb079ae130790F604b1030cDf386670f199"
            },
            "existentialNFTCloneFactory": "0xe9F27eb8cdAaA0B01a4034e109496C3026b01bd9"
        },
        "startBlockV1": 18800000,
        "logsQueryRange": 5000,
        "explorer": "https://bscscan.com",
        "subgraphV1": {
            "cliName": "bsc",
            "name": "protocol-v1-bsc-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-bsc-mainnet"
        },
        "subgraphVesting": {
            "name": "vesting-v1-bsc-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-bsc-mainnet"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-bsc-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-bsc-mainnet"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-bsc-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-bsc-mainnet"
        },
        "publicRPCs": [
            "https://bscrpc.com",
            "https://bsc-dataseed.binance.org"
        ],
        "coinGeckoId": "binance-smart-chain"
    },
    {
        "name": "eth-mainnet",
        "isTestnet": false,
        "networkId": 1,
        "chainId": 1,
        "sfId": 6,
        "shortName": "mainnet",
        "uppercaseName": "ETH_MAINNET",
        "humanReadableName": "Ethereum",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0xC22BeA0Be9872d8B7B3933CEc70Ece4D53A900da",
        "contractsV1": {
            "resolver": "0xeE4cD028f5fdaAdeA99f8fc38e8bA8A57c90Be53",
            "host": "0x4E583d9390082B65Bef884b629DFA426114CED6d",
            "governance": "0xe2E14e2C4518cB06c32Cd0818B4C01f53E1Ba653",
            "cfaV1": "0x2844c1BBdA121E9E43105630b9C8310e5c72744b",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0xbCF9cfA8Da20B591790dF27DE65C1254Bf91563d",
            "superTokenFactory": "0x0422689cc4087b6B7280e0a7e7F655200ec86Ae1",
            "superfluidLoader": "0x9775EEBdfF8AE845e7Ad3d1D04B85e6c6d284aCB",
            "toga": "0x8B5a2CF69a56d7F8Fa027edcA23594cdDF544dDc",
            "batchLiquidator": "0x554c06487bEc8c890A0345eb05a5292C1b1017Bd",
            "flowScheduler": "0xAA0cD305eD020137E302CeCede7b18c0A05aCCDA",
            "vestingScheduler": "0x39D5cBBa9adEBc25085a3918d36D5325546C001B",
            "autowrap": {
                "manager": "0x30aE282CF477E2eF28B14d0125aCEAd57Fe1d7a1",
                "wrapStrategy": "0x1D65c6d3AD39d454Ea8F682c49aE7744706eA96d"
            }
        },
        "startBlockV1": 15870000,
        "logsQueryRange": 10000,
        "explorer": "https://etherscan.io",
        "subgraphV1": {
            "cliName": "mainnet",
            "name": "protocol-v1-eth-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-eth-mainnet",
            "satsumaEndpoint": "https://subgraph.satsuma-prod.com/c5br3jaVlJI6/superfluid/eth-mainnet/api"
        },
        "subgraphVesting": {
            "name": "vesting-v1-eth-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/vesting-v1-eth-mainnet"
        },
        "subgraphFlowScheduler": {
            "name": "scheduling-v1-eth-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/scheduling-v1-eth-mainnet"
        },
        "subgraphAutoWrap": {
            "name": "auto-wrap-v1-eth-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/auto-wrap-v1-eth-mainnet"
        },
        "publicRPCs": [
            "https://cloudflare-eth.com",
            "https://eth-rpc.gateway.pokt.network"
        ],
        "coinGeckoId": "ethereum"
    },
    {
        "name": "celo-mainnet",
        "isTestnet": false,
        "networkId": 42220,
        "chainId": 42220,
        "sfId": 7,
        "shortName": "celo",
        "uppercaseName": "CELO_MAINNET",
        "humanReadableName": "Celo",
        "nativeTokenSymbol": "CELO",
        "nativeTokenWrapper": "0x671425Ae1f272Bc6F79beC3ed5C4b00e9c628240",
        "contractsV1": {
            "resolver": "0x05eE721BD4D803d6d477Aa7607395452B65373FF",
            "host": "0xA4Ff07cF81C02CFD356184879D953970cA957585",
            "governance": "0x0170FFCC75d178d426EBad5b1a31451d00Ddbd0D",
            "cfaV1": "0x9d369e78e1a682cE0F8d9aD849BeA4FE1c3bD3Ad",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x26747Fe93fAC8bF28E1e24A558a2bC7E4d9846cA",
            "superTokenFactory": "0x36be86dEe6BC726Ed0Cbd170ccD2F21760BC73D9",
            "constantOutflowNFT": "0xbe49ac1EadAc65dccf204D4Df81d650B50122aB2",
            "constantInflowNFT": "0x0FB7694c990CF19001127391Dbe53924dd7a61c7",
            "superfluidLoader": "0xAd6e6849d8eEa62AF9271808afb726610fB451a6",
            "toga": "0x9bCa3a623e7b2e248510d88B2894F54898d88F91",
            "batchLiquidator": "0x21d4E9fbB9DB742E6ef4f29d189a7C18B0b59136",
            "existentialNFTCloneFactory": "0x051e766e2d8dc65ae2bFCF084A50AD0447634227"
        },
        "startBlockV1": 16393000,
        "logsQueryRange": 20000,
        "explorer": "https://celoscan.io",
        "subgraphV1": {
            "cliName": "celo",
            "name": "protocol-v1-celo-mainnet",
            "hostedEndpoint": "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-celo-mainnet"
        },
        "publicRPCs": ["https://forno.celo.org", "https://rpc.ankr.com/celo"],
        "coinGeckoId": "celo"
    },
    {
        "name": "base-mainnet",
        "isTestnet": false,
        "networkId": 8453,
        "chainId": 8453,
        "sfId": 8,
        "shortName": "base",
        "uppercaseName": "BASE_MAINNET",
        "humanReadableName": "Base",
        "nativeTokenSymbol": "ETH",
        "nativeTokenWrapper": "0x46fd5cfB4c12D87acD3a13e92BAa53240C661D93",
        "contractsV1": {
            "resolver": "0x6a214c324553F96F04eFBDd66908685525Da0E0d",
            "host": "0x4C073B3baB6d8826b8C5b229f3cfdC1eC6E47E74",
            "governance": "0x55F7758dd99d5e185f4CC08d4Ad95B71f598264D",
            "cfaV1": "0x19ba78B9cDB05A877718841c574325fdB53601bb",
            "cfaV1Forwarder": "0xcfA132E353cB4E398080B9700609bb008eceB125",
            "idaV1": "0x66DF3f8e14CF870361378d8F61356D15d9F425C4",
            "superTokenFactory": "0xe20B9a38E0c96F61d1bA6b42a61512D56Fea1Eb3",
            "constantOutflowNFT": "0xD3C78bb5a16Ea4ab584844eeb8F90Ac710c16355",
            "constantInflowNFT": "0x2d51962A9EE4D3C2819EF585eab7412c2a2C31Ac",
            "superfluidLoader": "0x6B24A6F34ab96267Fef9F8AC9fe6CC104C228127",
            "toga": "0xA87F76e99f6C8Ff8996d14f550ceF47f193D9A09",
            "batchLiquidator": "0x889ebeCaD5F8C34Fd96023456d0fC5cE54eb74Cb",
            "existentialNFTCloneFactory": "0x642332562BC60a4Bd9681E7bb1588f7456A497aC"
        },
        "startBlockV1": 1000000,
        "logsQueryRange": 20000,
        "explorer": "https://basescan.org",
        "subgraphV1": {
            "cliName": "base",
            "name": "protocol-v1-base-mainnet"
        },
        "publicRPCs": ["https://developer-access-mainnet.base.org"],
        "coinGeckoId": "base"
    }
]
