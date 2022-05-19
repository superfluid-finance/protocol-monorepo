//script which initiates the option contract

const hre = require("hardhat");
require("dotenv");
const Web3 = require("web3");
// const ethers = require("@nomiclabs/hardhat-ethers");
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
const _sender = "0x5966aa11c794893774a382d9a19743B8be6BFFd1";

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
    async function startOption() {
        
        let txData = (await tradeableCashflowOption.methods.createOption(
            "0x01BE23585060835E02B77ef475b0Cc51aA1e0709", //LINK rinkeby token
            web3.utils.toWei("1", "ether"), //1 unit
            18, //link has 18 decimals
            "0xd8bD0a1cB028a31AA859A21A3758685a95dE4623", //LINK/USD price feed
            8, //price feed will return 8 decimal value
            "38580246913580", //~100 per mo
            1638362980,  //Dec 1, 2021,
            web3.utils.toWei("28", "ether") //strike price of this call option is $28
        ).encodeABI());

  
    //send the tx to the tradeableCashflowOption
      let tx = {
        'to': tradeableCashflowOptionAddress,
        'gas': 3000000,
        'nonce': nonce,
        'data': txData
      }
  
      let signedTx = await web3.eth.accounts.signTransaction(tx, process.env.RINKEBY_RECIEVER_PRIVATE_KEY);
  
      await web3.eth.sendSignedTransaction(signedTx.rawTransaction, function(error, hash) {
        if (!error) {
          console.log("🎉 The hash of your transaction is: ", hash, "\n Check Alchemy's Mempool to view the status of your transaction!");
        } else {
          console.log("❗Something went wrong while submitting your transaction:", error)
        }
       });
  
      }
    
  
    await startOption();
  
    }
  
  // We recommend this pattern to be able to use async/await everywhere
  // and properly handle errors.
  main()
    .then(() => process.exit(0))
    .catch((error) => {
      console.error(error);
      process.exit(1);
    });