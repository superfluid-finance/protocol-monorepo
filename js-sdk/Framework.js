//const Web3 = require("web3");
const TruffleContract = require("@truffle/contract");
const SuperfluidABI = require("../build/abi");
//const getConfig = require("./getConfig");

function Framework({ web3Provider, truffleArtifacts }) {
    const contractNames = Object.keys(SuperfluidABI);

    // load contracts
    this.contracts = {};
    if (typeof(truffleArtifacts) === "undefined") {
        // load contracts from ABI
        contractNames.forEach(i => {
            const c = this.contracts[i] = TruffleContract({
                contractName: i,
                abi: SuperfluidABI[i]
            });
            c.setProvider(web3Provider);
        });
    } else {
        // load contracts from truffle artifacts
        contractNames.forEach(i => {
            this.contracts[i] = truffleArtifacts.require(i);
        });
    }

    //this.web3 = new Web3(web3Provider);
    //const chainId = await this.web3.eth.net.getId(); // TODO use eth.getChainId;
    //const config = SuperfluidSDK.getConfig(chainId);
}

Framework.prototype.getERC20Wrapper = async function(tokenInfo) {
    const tokenInfoSymbol = await tokenInfo.symbol.call();
    const tokenInfoDecimals = await tokenInfo.decimals.call();
    return await this.host.getERC20Wrapper.call(
        `${tokenInfoSymbol}x`,
        tokenInfoDecimals.toString(),
        tokenInfo.address
    );
};

module.exports = Framework;
