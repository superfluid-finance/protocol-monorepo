const ethers = require("ethers");
const { Framework } = require("@superfluid-finance/sdk-core");
const BudgetNFTJSON = require("../artifacts/contracts/BudgetNFT.sol/BudgetNFT.json");
const BudgetNFTABI = BudgetNFTJSON.abi;
require("dotenv").config();

async function main() {

  //NOTE: this is set as the goerli url, but can be changed to reflect your RPC URL and network of choice
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);

  const network = await customHttpProvider.getNetwork();

  //NOTE: you'll need to input your own deployedBudgetNFT address here
  const deployedBudgetNFTAddress = "";

  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider,
    customSubgraphQueriesEndpoint: "",
    dataMode: "WEB3_ONLY"
  });

  const funder = sf.createSigner({
    privateKey:
      process.env.PRIVATE_KEY,
    provider: customHttpProvider
  });

  //NOTE - this is DAIx on goerli - you can change this token to suit your network and desired token address
  const daix = await sf.loadSuperToken("fDAIx");

  console.log('running fund contract script...')

  //Note: add your own custom amount here
  const daixTransferOperation = daix.transfer({receiver: deployedBudgetNFTAddress, amount: "1000000000000000000000"});
  try {
    await daixTransferOperation.exec(funder).then(function (tx) {
        console.log(
            ` Your operation was successful. Tx Hash: ${tx.hash} `
        )
    })
  }
  catch (err) {
      console.log(err)
  }
  
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
