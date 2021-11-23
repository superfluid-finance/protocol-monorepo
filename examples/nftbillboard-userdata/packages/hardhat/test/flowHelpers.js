const hre = require("hardhat");
const Web3 = require('web3');
// const web3 = new Web3(new Web3.providers.HttpProvider(process.env.MUMBAI_ALCHEMY_URL));
const web3 = new Web3(hre)
const hostJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol/ISuperfluid.json")
const hostABI = hostJSON.abi;
const hostAddress = "0xEB796bdb90fFA0f28255275e16936D25d3418603";

const cfaJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json")
const cfaABI = cfaJSON.abi;
const cfaAddress = "0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873";

host = new web3.eth.Contract(hostABI, hostAddress);
cfa = new web3.eth.Contract(cfaABI, cfaAddress);



module.exports = {

async createFlow(tokenAddress, tradeableCashflowAddress, flowRate, userDataString, _sender) {

  let nonce = await web3.eth.getTransactionCount(_sender, 'latest'); // nonce starts counting from 0
    
    let userData = web3.eth.abi.encodeParameter('string', userDataString);

    let cfaTx = (await cfa.methods
        .createFlow(
         tokenAddress,
         // _sender,
         //must be string
         tradeableCashflowAddress,
         //note: flowRate must be string
         flowRate,
         "0x"
        )
        .encodeABI())
   
        let txData = (await host.methods.callAgreement(
         cfaAddress, 
         cfaTx, 
         userData
       ).encodeABI());
   
       let tx = {
         'to': hostAddress,
         'gas': 3000000,
         'nonce': nonce,
         'data': txData
       }
   
       let signedTx = await web3.eth.accounts.signTransaction(tx, process.env.MUMBAI_DEPLOYER_PRIV_KEY);
   
       await web3.eth.sendSignedTransaction(signedTx.rawTransaction, function(error, hash) {
         if (!error) {
           console.log("🎉 The hash of your transaction is: ", hash, "\n Check Alchemy's Mempool to view the status of your transaction!");
         } else {
           console.log("❗Something went wrong while submitting your transaction:", error)
         }
        });
   
    },

async updateFlow(tokenAddress, tradeableCashflowAddress, flowRate, userDataString, _sender) {

  let nonce = await web3.eth.getTransactionCount(_sender, 'latest'); // nonce starts counting from 0

    let userData = web3.eth.abi.encodeParameter('string', userDataString);

    let cfaTx = (await cfa.methods
        .updateFlow(
         tokenAddress,
         // _sender,
         tradeableCashflowAddress,
         //must be string, try to make it different than createFlow's flowRate
         flowRate,
         "0x"
        )
        .encodeABI())
   
        let txData = (await host.methods.callAgreement(
         cfaAddress, 
         cfaTx, 
         userData
       ).encodeABI());
   
       let tx = {
         'to': hostAddress,
         'gas': 3000000,
         'nonce': nonce,
         'data': txData
       }
   
       let signedTx = await web3.eth.accounts.signTransaction(tx, process.env.MUMBAI_DEPLOYER_PRIV_KEY);
   
       await web3.eth.sendSignedTransaction(signedTx.rawTransaction, function(error, hash) {
         if (!error) {
           console.log("🎉 The hash of your transaction is: ", hash, "\n Check Alchemy's Mempool to view the status of your transaction!");
         } else {
           console.log("❗Something went wrong while submitting your transaction:", error)
         }
        });
},

async deleteFlow(tokenAddress, tradeableCashflowAddress, _sender) {
  
let nonce = await web3.eth.getTransactionCount(_sender, 'latest'); // nonce starts counting from 0

let cfaTx = (await cfa.methods
     .deleteFlow(
      tokenAddress,
      _sender,
      tradeableCashflowAddress,
      "0x"
     )
     .encodeABI())
    //try using callAgreement vs callagreement w context
     let txData = (await host.methods.callAgreement(
      cfaAddress, 
      cfaTx, 
      //pass in empty field for userData
      "0x"
    ).encodeABI());

    let tx = {
      'to': hostAddress,
      'gas': 3000000,
      'nonce': nonce,
      'data': txData
    }

    let signedTx = await web3.eth.accounts.signTransaction(tx, process.env.MUMBAI_DEPLOYER_PRIV_KEY);

    await web3.eth.sendSignedTransaction(signedTx.rawTransaction, function(error, hash) {
      if (!error) {
        console.log("🎉 The hash of your transaction is: ", hash, "\n Check Alchemy's Mempool to view the status of your transaction!");
      } else {
        console.log("❗Something went wrong while submitting your transaction:", error)
      }
     });

}
}

