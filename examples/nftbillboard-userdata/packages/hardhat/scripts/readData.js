const ethers = require("ethers");
const { Framework } = require("@superfluid-finance/sdk-core");
const { defaultAbiCoder } = require("ethers/lib/utils");
require("dotenv").config();
const tradeableCashflowABI = require("../artifacts/contracts/TradeableCashflow.sol/TradeableCashflow.json").abi;

const tcfAddress = "0xdd28Bff0530681892A4b5d73ae7c97f2c7418fA7"; //NOTE: this must be changed to reflect the actual live contract address

//used for getting jailed status of app
const hostABI = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol/ISuperfluid.json").abi

//read flowData
async function main() {

  //NOTE: this is currently for usage on GOERLI
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);

  const sf = await Framework.create({
    chainId: (await customHttpProvider.getNetwork()).chainId,
    provider: customHttpProvider
  });

  const sender = sf.createSigner({
    privateKey:
      process.env.PRIVATE_KEY,
    provider: customHttpProvider
  });

  const fDAIx = await sf.loadSuperToken("fDAIx");

  const host = new ethers.Contract(sf.settings.config.hostAddress, hostABI, customHttpProvider);

  const tradeableCashflow = new ethers.Contract(tcfAddress, tradeableCashflowABI, customHttpProvider);

  //get data
  const decodedContext = await tradeableCashflow.uData();
  console.log("Full decoded Ctx: ", decodedContext)

  const decodedUserData = defaultAbiCoder.decode(['string'], decodedContext.userData);
  console.log("User data value: ", decodedUserData[0])

  //get jail info - is this app jailed? Learn more at docs.superfluid.finance 
  const isJailed = await host.isAppJailed(tcfAddress);
  console.log(`is jailed: ${isJailed}`);

  const flowInfo = await sf.cfaV1.getFlow({
    superToken: fDAIx.address, 
    sender: sender.address,
    receiver: tcfAddress,
    providerOrSigner: customHttpProvider
  });

  const netFlow = await sf.cfaV1.getNetFlow({
    superToken: fDAIx.address,
    account: tcfAddress,
    providerOrSigner: customHttpProvider
  })

  const outFlowRate = Number(flowInfo.flowRate);
  console.log(`Outflow Rate: ${outFlowRate}`);

  const inFlowRate = Number(netFlow) + outFlowRate;
  console.log(`Inflow rate: ${inFlowRate}`)

  console.log(`Net flow: ${netFlow}`);

}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });