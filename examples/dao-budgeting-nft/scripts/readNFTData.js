// We require the Hardhat Runtime Environment explicitly here. This is optional
// but useful for running the script in a standalone fashion through `node <script>`.
//

//read data on budget NFT contract

const hre = require("hardhat");
require("dotenv");
const Web3 = require("web3");
const ethers = require("ethers");

const hostAddress = '0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3';
const cfaAddress = '0xECa8056809e7e8db04A8fF6e4E82cD889a46FE2F';
const fDAIx = '0xe3cb950cb164a31c66e32c320a800d477019dcff';

//all addresses hardcoded for kovan
const hostJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol/ISuperfluid.json")
const hostABI = hostJSON.abi;

const cfaJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json")
const cfaABI = cfaJSON.abi;

const budgetNFTJSON = require("../artifacts/contracts/BudgetNFT.sol/BudgetNFT.json");
const budgetNFTABI = budgetNFTJSON.abi; 

//temporarily hardcode contract address and sender address
//need to manually enter contract address and sender address here
const deployedBudgetNFT = require("../deployments/kovan/BudgetNFT.json");
const budgetNFTAddress = deployedBudgetNFT.address;


//address of owner of option here..need to change this
const _sender = "0x9C040e2d6Fd83A8b35069aa7154b69674961e0F7";

// When running the script with `npx hardhat run <script>` you'll find the Hardhat
// Runtime Environment's members available in the global scope.

async function main() {
  // Hardhat always runs the compile task when running scripts with its command
  // line interface.
  //
  // If this script is run directly using `node` you may want to call compile
  // manually to make sure everything is compiled
  // await hre.run('compile');

  // 0x5FbDB2315678afecb367f032d93F642f64180aa3
  const web3 = new Web3(new Web3.providers.HttpProvider(process.env.ALCHEMY_URL));
  const budgetNFT = await new web3.eth.Contract(budgetNFTABI, budgetNFTAddress);


  const nextId = await budgetNFT.methods.nextId().call();

  console.log(`Previous ID: ${nextId - 1}`);

}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
