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

  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider,
    customSubgraphQueriesEndpoint: "",
    dataMode: "WEB3_ONLY"
  });

  const deployer = sf.createSigner({
    privateKey:
      process.env.PRIVATE_KEY,
    provider: customHttpProvider
  });

  //NOTE - this is DAIx on goerli - you can change this token to suit your network and desired token address
  const daix = await sf.loadSuperToken("fDAIx");

  console.log('running deploy script...')
  // We get the contract to deploy
  const BudgetNFT = await hre.ethers.getContractFactory("BudgetNFT");
  const budgetNFT = await BudgetNFT.connect(deployer).deploy(
      "Budget NFT",
      "BNFT",
      sf.settings.config.hostAddress,
      daix.address
  );
  
  await budgetNFT.deployed();

  //NOTE: you will need this address to run other scripts, so we recommend getting it from the console
  console.log("BudgetNFT.sol deployed to:", budgetNFT.address);
  
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });