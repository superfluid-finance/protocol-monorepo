TRUFFLE_NETWORK=$1

case $TRUFFLE_NETWORK in
    eth-goerli | eth-rinkeby | eth-ropsten | eth-kovan | \
    polygon-mumbai | polygon-matic | \
    optimism-mainnet | optimism-kovan | \
    arbitrum-one | arbitrum-rinkeby | \
    avalanche-mainnet | avalance-fuji | \
    bsc )
        echo "$TRUFFLE_NETWORK is supported by Etherscan or alike"
        exit 0;
        ;;
    *)
        echo "$TRUFFLE_NETWORK is not supported by Etherscan"
        exit 1
        ;;
esac
