const hre = require("hardhat");
const { Framework } = require("@superfluid-finance/sdk-core");

async function main() {

  //// Applying best practices and using Superfluid Framework to get deployment info

  // Setting up network object - this is set as the goerli url, but can be changed to reflect your RPC URL and network of choice
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);
  const network = await customHttpProvider.getNetwork();

  // Setting up the out Framework object with Goerli (knows it's Goerli when we pass in network.chainId)
  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider,
    customSubgraphQueriesEndpoint: "",
    dataMode: "WEB3_ONLY"
  });

  // Getting the Goerli fDAIx Super Token object from the Framework object
  // This is fDAIx on goerli - you can change this token to suit your network and desired token address
  const daix = await sf.loadSuperToken("fDAIx");

  //// Actually deploying

  // We get the contract to deploy to Gorli Testnet
  const TokenSpreader = await hre.ethers.getContractFactory("TokenSpreader");
  const tokenSpreader = await TokenSpreader.deploy(
    sf.settings.config.hostAddress, // Getting the Goerli Host contract address from the Framework object
    daix.address                  
  );

  await tokenSpreader.deployed();

  console.log("Token Spreader deployed to:", tokenSpreader.address);
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});

// Deploy: npx hardhat run scripts/deploy.js --network goerli

// Verify: npx hardhat verify --network goerli --constructor-args arguments-tokenspreader.js [contractaddress]