//script which exercises the option

const hre = require("hardhat");
require("dotenv");
const Web3 = require("web3");
const ethers = require("ethers");

//all addresses hardcoded for rinkeby
const hostJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol/ISuperfluid.json")
const hostABI = hostJSON.abi;
const hostAddress = "0xeD5B5b32110c3Ded02a07c8b8e97513FAfb883B6";

const cfaJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json")
const cfaABI = cfaJSON.abi;
const cfaAddress = "0xF4C5310E51F6079F601a5fb7120bC72a70b96e2A";

const tradeableCashflowOptionJSON = require("../artifacts/contracts/TradeableCashflowOption.sol/TradeableCashflowOption.json");
const tradeableCashflowOptionABI = tradeableCashflowOptionJSON.abi; 

//temporarily hardcode contract address and sender address
//need to manually enter contract address and sender address here
const deployedTradeableCashflowOption = require("../deployments/rinkeby/TradeableCashflowOption.json");
const tradeableCashflowOptionAddress = deployedTradeableCashflowOption.address;

//address of owner of option here..need to change this
const _sender = "0x9421FE8eCcAfad76C3A9Ec8f9779fAfA05A836B3";

//create a flow
async function main() {

    const web3 = new Web3(new Web3.providers.HttpProvider(process.env.RINKEBY_ALCHEMY_URL));
  
    //create contract instances for each of these
    const host = new web3.eth.Contract(hostABI, hostAddress);
    const cfa = new web3.eth.Contract(cfaABI, cfaAddress);
    const tradeableCashflowOption = new web3.eth.Contract(tradeableCashflowOptionABI, tradeableCashflowOptionAddress);
    
    const fDAIx = "0x15F0Ca26781C3852f8166eD2ebce5D18265cceb7";
  
    const nonce = await web3.eth.getTransactionCount(_sender, 'latest'); // nonce starts counting from 0
  
    //create flow by calling host directly in this function
    //create flow from sender to tradeable cashflow address
    async function exerciseOption() {

        // ERC20 underlyingAsset, 
        //     uint256 underlyingAmount, 
        //     uint8 underlyingDecimals,
        //     AggregatorV3Interface priceFeed, 
        //     int96 requiredFlowRate, 
        //     uint256 expirationDate, 
        //     int256 strikePrice)  

        
    let txData = (await tradeableCashflowOption.methods.exerciseOption().encodeABI());

  
    //send the tx to the tradeableCashflowOption
      let tx = {
        'to': tradeableCashflowOptionAddress,
        'gas': 3500000,
        'nonce': nonce,
        'data': txData
      }
  
      let signedTx = await web3.eth.accounts.signTransaction(tx, process.env.RINKEBY_DEPLOYER_PRIVATE_KEY);
  
      await web3.eth.sendSignedTransaction(signedTx.rawTransaction, function(error, hash) {
        if (!error) {
          console.log("🎉 The hash of your transaction is: ", hash, "\n Check Alchemy's Mempool to view the status of your transaction!");
        } else {
          console.log("❗Something went wrong while submitting your transaction:", error)
        }
       });
  
      }
    
  
    await exerciseOption();
  
    }
  
  // We recommend this pattern to be able to use async/await everywhere
  // and properly handle errors.
  main()
    .then(() => process.exit(0))
    .catch((error) => {
      console.error(error);
      process.exit(1);
    });