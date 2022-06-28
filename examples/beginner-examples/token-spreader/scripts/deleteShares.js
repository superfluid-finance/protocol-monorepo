const hre = require("hardhat");
const ethers = hre.ethers;
const { Framework } = require("@superfluid-finance/sdk-core");
const TokenSpreaderJSON = require("../artifacts/contracts/TokenSpreader.sol/TokenSpreader.json");
const TokenSpreaderABI = TokenSpreaderJSON.abi;
require("dotenv").config();

const deployedTokenSpreaderAddress = process.env.TOKENSPREADER_ADDRESS;  // INPUT YOUR OWN DEPLOYED TOKENSPREADER ADDRESS IN .ENV FILE

async function main() {

  // Get signer objects from private key as potential receivers of unit from gainShare
  let alice;
  let bob;
  let carol;
  let mallory;
  [alice, bob, carol, mallory] = await ethers.getSigners();
  
  let shareDeleter = alice; // SELECT FROM ALICE, BOB, CAROL, OR MALLORY at your discretion

  // Setting up network object - this is set as the goerli url, but can be changed to reflect your RPC URL and network of choice
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);
  const network = await customHttpProvider.getNetwork();

  // Getting tokenSpreader contract object
  const tokenSpreader = new ethers.Contract(deployedTokenSpreaderAddress, TokenSpreaderABI, customHttpProvider)

  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider,
    customSubgraphQueriesEndpoint: "",
    dataMode: "WEB3_ONLY"
  });

  // Getting the Goerli fDAIx Super Token object from the Framework object
  // This is fDAIx on goerli - you can change this token to suit your network and desired token address
  const daix = await sf.loadSuperToken("fDAIx");

  console.log('Running deleteShare() script...');

  // View units that shareDeleter has
  const unitsHeld = (await sf.idaV1.getSubscription({
    superToken: daix.address,
    publisher: tokenSpreader.address,
    indexId: await tokenSpreader.INDEX_ID(),
    subscriber: shareDeleter.address,
    providerOrSigner: alice
  })).units

  console.log(`Original ${shareDeleter.address} units held: ${unitsHeld}`);

  // If the shareLose has units, then delete them. If not, do nothing
  if(unitsHeld != 0) {

    // Removes all shares from shareDeleter
    const gainShareTx = await tokenSpreader.connect(alice).deleteShares(shareDeleter.address);
    await gainShareTx.wait();

      // View shares that shareDeleter has (should be none)
    console.log(`New ${shareDeleter.address} units held:`, 
      (await sf.idaV1.getSubscription({
        superToken: daix.address,
        publisher: tokenSpreader.address,
        indexId: await tokenSpreader.INDEX_ID(),
        subscriber: shareDeleter.address,
        providerOrSigner: alice
      })).units
    );

  } else {

    console.log("Already has zero units, can't further reduce.");

  }

};

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
});