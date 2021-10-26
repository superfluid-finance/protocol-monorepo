const { ethers } = require("hardhat");
const { use, expect } = require("chai");
const Web3 = require('web3')
const { solidity } = require("ethereum-waffle");
const web3 = new Web3(new Web3.providers.HttpProvider(process.env.MUMBAI_ALCHEMY_URL));
const flowHelpers = require("./flowHelpers");
const fDAIx = "0x5D8B4C2554aeB7e86F387B4d6c00Ac33499Ed01f";
const deployedTradeableCashflow = require("../deployments/polytest/TradeableCashflow.json");
const tradeableCashflowAddress = deployedTradeableCashflow.address;


// use(solidity);

describe("NFT Billboard Testin", function () {
  
  
  let hostJSON;
  let hostABI;
  let hostAddress;
  let host;

  let cfaJSON;
  let cfaABI;
  let cfaAddress;
  let cfa;
  
  let tradeableCashflowJSON;
  let tradeableCashflowABI;
  let _sender;

  beforeEach(async () => {
      //all addresses hardcoded for mumbai
    hostJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol/ISuperfluid.json")
    hostABI = hostJSON.abi;
    hostAddress = "0xEB796bdb90fFA0f28255275e16936D25d3418603";

    cfaJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol/IConstantFlowAgreementV1.json")
    cfaABI = cfaJSON.abi;
    cfaAddress = "0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873";

    tradeableCashflowJSON = require("../artifacts/contracts/TradeableCashflow.sol/TradeableCashflow.json");
    tradeableCashflowABI = tradeableCashflowJSON.abi; 

    //temporarily hardcode contract address and sender address
    //need to manually enter contract address and sender address here
    _sender = "0x9421FE8eCcAfad76C3A9Ec8f9779fAfA05A836B3";

    //create contract instances for each of these
    host = new web3.eth.Contract(hostABI, hostAddress);
    cfa = new web3.eth.Contract(cfaABI, cfaAddress);
    tradeableCashflow = new web3.eth.Contract(tradeableCashflowABI, tradeableCashflowAddress);
    
  })

  xit("should create a flow properly", async () => {
    // await flowHelpers.createFlow(fDAIx, tradeableCashflowAddress, "3858024691358", "HODL BTC", _sender);

    const flowInfo = await cfa.methods.getFlow(fDAIx, tradeableCashflowAddress, "0x00471Eaad87b91f49b5614D452bd0444499c1bd9").call();
    const outFlowRate = Number(flowInfo.flowRate);
    console.log(`Outflow Rate: ${outFlowRate}`);

    const netFlow = await cfa.methods.getNetFlow(fDAIx, tradeableCashflowAddress).call();
    console.log(`Net flow: ${netFlow}`);

    const inFlowRate = Number(netFlow) + outFlowRate;
    console.log(`Inflow rate: ${inFlowRate}`)

    expect(1).to.equal(1);

  })

  it("should update a flow properly", async () => {
    await flowHelpers.updateFlow(fDAIx, tradeableCashflowAddress, "5858024691358", "HODL ETH", _sender);

    const flowInfo = await cfa.methods.getFlow(fDAIx, tradeableCashflowAddress, "0x00471Eaad87b91f49b5614D452bd0444499c1bd9").call();
    const outFlowRate = Number(flowInfo.flowRate);
    console.log(`Outflow Rate: ${outFlowRate}`);

    const netFlow = await cfa.methods.getNetFlow(fDAIx, tradeableCashflowAddress).call();
    console.log(`Net flow: ${netFlow}`);

    const inFlowRate = Number(netFlow) + outFlowRate;
    console.log(`Inflow rate: ${inFlowRate}`)

    const context = await tradeableCashflow.methods.uData().call();
    const decodedUserData = web3.eth.abi.decodeParameter('string', context.userData);
    
    expect(decodedUserData).to.equal("HODL ETH");
    expect(outFlowRate).to.equal("5858024691358");
    expect(netFlow).to.equal(0)

  })

  it("should delete a flow properly", async () => {

  })


});