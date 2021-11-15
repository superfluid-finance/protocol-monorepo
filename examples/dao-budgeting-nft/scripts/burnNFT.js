// We require the Hardhat Runtime Environment explicitly here. This is optional
// but useful for running the script in a standalone fashion through `node <script>`.
//
//burn an NFT
//as of now, this does not delete the flow

const hre = require("hardhat");
require("dotenv");
const Web3 = require("web3");
const ethers = require("ethers");

const hostAddress = '0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3';
const cfaAddress = '0xECa8056809e7e8db04A8fF6e4E82cD889a46FE2F';
const fDAIx = '0xe3CB950Cb164a31C66e32c320A800D477019DCFF';

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


//address of receiver of the NFT
const _receiver = "0xF538b8d65C4ae4D09503A0F06F38486019750Aa4";
//address of the caller of the contract (the issuer)
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
  const web3 = new Web3(new Web3.providers.HttpProvider(process.env.ALCHEMY_URL));
  const budgetNFT = await new web3.eth.Contract(budgetNFTABI, budgetNFTAddress);
  const nonce = await web3.eth.getTransactionCount(_sender, 'latest'); // nonce starts counting from 0

  //make sure to change the first param to the correct flowId
  const txData = (await budgetNFT.methods.burnNFT(0)).encodeABI() //~$1 per mo

  //send the tx to the contract
  let tx = {
    'to': budgetNFTAddress,
    'gas': 3000000,
    'nonce': nonce,
    'data': txData
  }

  let signedTx = await web3.eth.accounts.signTransaction(tx, process.env.PK);

  await web3.eth.sendSignedTransaction(signedTx.rawTransaction, function(error, hash) {
    if (!error) {
      console.log("🎉 The hash of your transaction is: ", hash, "\n Check Alchemy's Mempool to view the status of your transaction!");
    } else {
      console.log("❗Something went wrong while submitting your transaction:", error)
    }
   });


}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });