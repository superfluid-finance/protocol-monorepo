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

  console.log('Running viewStatus script...\n');

  console.log('TokenSpreader spreaderToken Balance:', (await daix.balanceOf({account:tokenSpreader.address, providerOrSigner: alice})));

  // Get outstanding units of tokenSpreader's IDA index
  const indexDataTokenSpreader = await sf.idaV1.getIndex({
      superToken: daix.address,
      publisher: tokenSpreader.address,
      indexId: await tokenSpreader.INDEX_ID(),
      providerOrSigner: alice
  });

  console.log( 'TokenSpreader Units Approved:', indexDataTokenSpreader.totalUnitsApproved );
  console.log( 'TokenSpreader Units Pending:', indexDataTokenSpreader.totalUnitsPending,"\n" );

  // Row format: address | units held | super token balance

  console.log(" Address \t|\t Units Held \t|\t Spreader Token Balance\n");

  for (let i = 0; i < userList.length; i++) {
    
    row = `${ (userList[i].address).substring(0,7) }...\t`;
    
    let unitsHeld = (await sf.idaV1.getSubscription({
        superToken: daix.address,
        publisher: tokenSpreader.address,
        indexId: await tokenSpreader.INDEX_ID(),
        subscriber: userList[i].address,
        providerOrSigner: alice
    })).units
    row += `|\t  ${unitsHeld}  \t\t`;

    let spreaderTokenBalance = await daix.balanceOf({account:userList[i].address, providerOrSigner: alice});
    row += `|\t${spreaderTokenBalance}`;

    console.log(row);
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