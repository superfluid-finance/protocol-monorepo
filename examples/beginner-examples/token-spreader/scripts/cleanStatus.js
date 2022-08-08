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
  let userList = [alice, bob, carol, mallory];
  
  // Setting up network object - this is set as the goerli url, but can be changed to reflect your RPC URL and network of choice
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);
  const network = await customHttpProvider.getNetwork();

  // Getting tokenSpreader contract object
  const tokenSpreader = new ethers.Contract(deployedTokenSpreaderAddress, TokenSpreaderABI, customHttpProvider)

  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider
  });

  // Getting the Goerli fDAIx Super Token object from the Framework object
  // This is fDAIx on goerli - you can change this token to suit your network and desired token address
  const daix = await sf.loadSuperToken("fDAIx");

  console.log('Running cleanStatus script...\n');

  for (let i = 0; i < userList.length; i++) {
    
    // Getting rid of units held
    let unitsHeld = (await sf.idaV1.getSubscription({
        superToken: daix.address,
        publisher: tokenSpreader.address,
        indexId: await tokenSpreader.INDEX_ID(),
        subscriber: userList[i].address,
        providerOrSigner: alice
    })).units

    if(unitsHeld != 0) {

        const gainShareTx = await tokenSpreader.connect(alice).deleteShares(userList[i].address);
        await gainShareTx.wait();
    
    }

    // Getting rid of Spreader Tokens


    let transferOperations = daix.transfer({
        receiver: "0x63bfb2118771bd0da7A6936667A7BB705A06c1bA", // some random address because we can't transfer Super Tokens to zero address
        amount: await daix.balanceOf({account: userList[i].address, providerOrSigner: alice}),
    });
    await transferOperations.exec(userList[i]);

  }

  console.log("User units and balances all zeroed out!");

}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
});