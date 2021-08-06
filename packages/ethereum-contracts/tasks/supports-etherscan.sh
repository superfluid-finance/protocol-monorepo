TRUFFLE_NETWORK=$1

case $TRUFFLE_NETWORK in
    goerli | rinkeby | ropsten | kovan | mumbai | matic )
        echo "$TRUFFLE_NETWORK is supported by Etherscan"
        exit 0;
        ;;
    *)
        echo "$TRUFFLE_NETWORK is not supported by Etherscan"
        exit 1
        ;;
esac
