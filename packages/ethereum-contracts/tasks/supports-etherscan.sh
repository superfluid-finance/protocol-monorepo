TRUFFLE_NETWORK=$1

case $TRUFFLE_NETWORK in
    goerli | rinkeby | ropsten | kovan | mumbai | matic | opmainnet | opkovan | arbone | arbrinkeby | avalanche | avafuji | bsc )
        echo "$TRUFFLE_NETWORK is supported by Etherscan or alike"
        exit 0;
        ;;
    *)
        echo "$TRUFFLE_NETWORK is not supported by Etherscan"
        exit 1
        ;;
esac
