const Web3 = require("web3");
const deploy = require("../scripts/deploy-erc1820");

module.exports = async function(deployer) {
    const errorHandler = err => { if (err) throw err; };
    global.web3 = new Web3(deployer.provider);
    await deploy(errorHandler);
};
