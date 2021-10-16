require("dotenv");
const Web3 = require("web3");

//all addresses hardcoded for mumbai
const hostJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol/ISuperfluid.json")
const hostABI = hostJSON.abi;
const hostAddress = "0xEB796bdb90fFA0f28255275e16936D25d3418603";

const cfaJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json")
const cfaABI = cfaJSON.abi;
const cfaAddress = "0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873";

const tradeableCashflowJSON = require("../artifacts/contracts/TradeableCashflow.sol/TradeableCashflow.json");
const tradeableCashflowABI = tradeableCashflowJSON.abi; 

  //temporarily hardcode contract address 
  const deployedTradeableCashflow = require("../deployments/polytest/TradeableCashflow.json");
  const tradeableCashflowAddress = deployedTradeableCashflow.address;
  
//read flowData
async function main() {

const web3 = new Web3(new Web3.providers.HttpProvider(process.env.MUMBAI_ALCHEMY_URL));

  //create contract instances for each of these
  const host = new web3.eth.Contract(hostABI, hostAddress);
  const cfa = new web3.eth.Contract(cfaABI, cfaAddress);
  const tradeableCashflow = new web3.eth.Contract(tradeableCashflowABI, tradeableCashflowAddress);
  const fDAIx = "0x5D8B4C2554aeB7e86F387B4d6c00Ac33499Ed01f"


  //get data
  const decodedContext = await tradeableCashflow.methods.uData().call();
  const decodedUserData = web3.eth.abi.decodeParameter('string', decodedContext.userData);
  console.log(decodedContext)
  console.log(decodedUserData)
  
  //get jail info
  const jailed = await host.methods.getAppManifest(tradeableCashflowAddress).call()
  console.log(jailed)
  const isJailed = await host.methods.isAppJailed(tradeableCashflowAddress).call();
  console.log(`is jailed: ${isJailed}`);

  const flowInfo = await cfa.methods.getFlow(fDAIx, tradeableCashflowAddress, "0x00471Eaad87b91f49b5614D452bd0444499c1bd9").call();
  const outFlowRate = Number(flowInfo.flowRate);
  console.log(`Outflow Rate: ${outFlowRate}`);

  const netFlow = await cfa.methods.getNetFlow(fDAIx, tradeableCashflowAddress).call();
  console.log(`Net flow: ${netFlow}`);

  const inFlowRate = Number(netFlow) + outFlowRate;
  console.log(`Inflow rate: ${inFlowRate}`)
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });