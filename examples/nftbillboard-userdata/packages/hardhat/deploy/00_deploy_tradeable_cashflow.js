require("dotenv").config();
const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const { Framework } = require("@superfluid-finance/sdk-core");
const ethers = require("ethers")
const { web3 } = require("hardhat");
const { defaultNetwork } = require("../hardhat.config");

//your address and RPC here...
const owner = process.env.OWNER_ADDRESS;
const rpcProvider = new ethers.providers.JsonRpcProvider(process.env.GOERLI_URL);


module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;

  let sf;
  const { deployer } = await getNamedAccounts();

  if (defaultNetwork == 'ganache' || defaultNetwork == 'localhost') {

    const errorHandler = (err) => {
      if (err) throw err;
    };
        
    await deployFramework(errorHandler, {
      web3,
      from: deployer,
    });
  
    await deployTestToken(errorHandler, [":", "fDAI"], {
      web3,
      from: deployer,
    });
    await deploySuperToken(errorHandler, [":", "fDAI"], {
      web3,
      from: deployer,
    });

    sf = await Framework.create({
      networkName: "custom",
      provider: web3,
      dataMode: "WEB3_ONLY",
      resolverAddress: process.env.RESOLVER_ADDRESS, //this is how you get the resolver address
      protocolReleaseVersion: "test",
    });

    const fDAIx = await sf.loadSuperToken("fDAIx");
    
    console.log(deployer)
    await deploy("TradeableCashflow", {
      from: deployer,
      args: [deployer, 'nifty_billboard', 'NFTBoard', sf.settings.config.hostAddress, fDAIx.address],
      log: true,
    })
  }

  else {    

    sf = await Framework.create({
      chainId: (await rpcProvider.getNetwork()).chainId,
      provider: rpcProvider
    });

    const fDAIx = await sf.loadSuperToken("fDAIx");

    await deploy("TradeableCashflow", {
      from: deployer,
      args: [deployer, 'nifty_billboard', 'NFTBoard', sf.settings.config.hostAddress, fDAIx.address],
      log: true,
    })
  }

};
module.exports.tags = ["TradeableCashflow"];
