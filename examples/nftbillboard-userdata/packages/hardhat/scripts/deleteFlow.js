const ethers = require("ethers");
const { Framework } = require("@superfluid-finance/sdk-core");
const { AbiCoder, defaultAbiCoder } = require("ethers/lib/utils");
require("dotenv").config();

const tcfAddress = "0xdd28Bff0530681892A4b5d73ae7c97f2c7418fA7"; //NOTE: this must be changed to reflect the actual live contract address

async function main() {

  //NOTE: this is currently for usage on GOERLI
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);

  const sf = await Framework.create({
    chainId: (await customHttpProvider.getNetwork()).chainId,
    provider: customHttpProvider,
    customSubgraphQueriesEndpoint: "",
    dataMode: "WEB3_ONLY"
  });

  const sender = sf.createSigner({
    privateKey:
      process.env.PRIVATE_KEY,
    provider: customHttpProvider
  });


  const daix = await sf.loadSuperToken("fDAIx");
    
  const deleteFlowOperation = sf.cfaV1.deleteFlow({
      sender: sender.address,
      receiver: tcfAddress,
      superToken: daix.address,
      userData: `0x` // no message on deletion
  });

  console.log('running delete flow script and sending message...');

  await deleteFlowOperation.exec(sender).then(console.log);
  
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });