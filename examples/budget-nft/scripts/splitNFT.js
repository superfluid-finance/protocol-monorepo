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
  const budgetNFT = new ethers.Contract(deployedBudgetNFTAddress, BudgetNFTABI, customHttpProvider)

  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider
  });

  //NOTE: splitor needs to be the owner of the NFT
  const splitor = sf.createSigner({
    privateKey:
      process.env.PRIVATE_KEY2,
    provider: customHttpProvider
  });

  console.log('running split NFT script...');

  //NOTE: ensure that you have your own token id and flow rate here. This flow rate is ~$1.50 per month 
  try {
    await budgetNFT.connect(splitor).splitStream(1, "578703703703").then(function (tx) {
        console.log(`Your transaction was successful. Txn hash: ${tx.hash}`)
    })
  } catch (err) {
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