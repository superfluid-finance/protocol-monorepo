require("dotenv").config();

// mumbai addresses - change if using a different network
const host = '0xEB796bdb90fFA0f28255275e16936D25d3418603';
const cfa = '0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873';
const fDAIx = '0x5D8B4C2554aeB7e86F387B4d6c00Ac33499Ed01f';

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const { Framework } = require("@superfluid-finance/sdk-core")
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const Web3 = require("web3");
const { defaultNetwork } = require("../hardhat.config");
const config = require("../hardhat.config");

require("dotenv");
//your address here...
const owner = process.env.OWNER_ADDRESS;

module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;

  const { deployer } = await getNamedAccounts();
  console.log(deployer);

  const errorHandler = (err) => {
    if (err) throw err;
  };

  if (defaultNetwork == 'ganache' || defaultNetwork == 'localhost') {
        
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
  
    let sf = new SuperfluidSDK.Framework({
      web3,
      version: "test",
      tokens: ["fDAI"],
    });
  
    await sf.initialize();
  
    console.log(sf.host.address)
    console.log(sf.agreements.cfa.address);
    console.log(sf.tokens.fDAIx.address)
  
    await deploy("TradeableCashflow", {
      from: deployer,
      args: [deployer, 'nifty_billboard', 'NFTBoard', sf.host.address, sf.tokens.fDAIx.address],
      log: true,
    })
  }

  else {
    await deploy("TradeableCashflow", {
      from: deployer,
      args: [deployer, 'nifty_billboard', 'NFTBoard', host, cfa, fDAIx],
      log: true,
    })
  }

};
module.exports.tags = ["TradeableCashflow"];
