const hre = require("hardhat");
const ethers = hre.ethers;
const { Framework } = require("@superfluid-finance/sdk-core");
const TokenSpreaderJSON = require("../artifacts/contracts/TokenSpreader.sol/TokenSpreader.json");
const TokenSpreaderABI = TokenSpreaderJSON.abi;
require("dotenv").config();

const deployedTokenSpreaderAddress = process.env.TOKENSPREADER_ADDRESS;  // INPUT YOUR OWN DEPLOYED TOKENSPREADER ADDRESS IN .ENV FILE

async function main() {

  // Get signer object to use when calling functions
  let alice;
  [alice] = await ethers.getSigners();

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

  console.log('Running distribute() script...');

  // Get spreader token object and print out balance of it held by TokenSpreader (fDAIx)
  // const spreaderToken = new ethers.Contract( await tokenSpreader.spreaderToken() , SuperTokenABI , customHttpProvider);
  console.log('Original TokenSpreader spreaderToken Balance:', (await daix.balanceOf({account:tokenSpreader.address, providerOrSigner: alice})));

  // Get outstanding units of tokenSpreader's IDA index
  const indexDataTokenSpreader = await sf.idaV1.getIndex({
      superToken: daix.address,
      publisher: tokenSpreader.address,
      indexId: await tokenSpreader.INDEX_ID(),
      providerOrSigner: alice
  });

  console.log( 'TokenSpreader Units Approved:', indexDataTokenSpreader.totalUnitsApproved );
  console.log( 'TokenSpreader Units Pending:', indexDataTokenSpreader.totalUnitsPending );
  
  const totalUnitsOutstanding = parseInt(indexDataTokenSpreader.totalUnitsApproved) + parseInt(indexDataTokenSpreader.totalUnitsPending);
  if (totalUnitsOutstanding != 0) {

    const distributeTx = await tokenSpreader.connect(alice).distribute()
    await distributeTx.wait();
    console.log('Distributed successfully!')

  } else {

    console.log( 'DID NOT DISTRIBUTE: the sum of Units Approved and Pending is zero and the distribute will fail!');
  
  }



  console.log('New TokenSpreader spreaderToken Balance:', (await daix.balanceOf({account:tokenSpreader.address, providerOrSigner: alice})));
  
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
});