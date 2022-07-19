const hre = require("hardhat");
const ethers = hre.ethers;
const { Framework } = require("@superfluid-finance/sdk-core");
const TokenSpreaderJSON = require("../artifacts/contracts/TokenSpreader.sol/TokenSpreader.json");
const TokenSpreaderABI = TokenSpreaderJSON.abi;
const SuperTokenJSON = require("../artifacts/@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol/ISuperToken.json")
const SuperTokenABI = SuperTokenJSON.abi;
const TestTokenJSON = require("@superfluid-finance/ethereum-contracts/build/contracts/TestToken.json")
const TestTokenABI = TestTokenJSON.abi; // fDAI is modded to include a public mint function that this ABI includes for us to use

require("dotenv").config();

const deployedTokenSpreaderAddress = process.env.TOKENSPREADER_ADDRESS;  // INPUT YOUR OWN DEPLOYED TOKENSPREADER ADDRESS IN .ENV FILE
const transferInAmount = ethers.utils.parseEther("1000"); // INPUT AMOUNT OF SPREADERTOKENS YOU'D LIKE TO TRANSFER INTO TOKENSPREADER HERE

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
  
  console.log("Providing TokenSpreader with fDAIx...")

  try {

    console.log( 'Original TokenSpreader SpreaderToken Balance:', await daix.balanceOf({account: tokenSpreader.address, providerOrSigner: alice }));

    // Mint alice transferInAmount of fDAI
    const fdai = new ethers.Contract(await daix.underlyingToken.address, TestTokenABI, customHttpProvider);
    await fdai.connect(alice).mint(alice.address, transferInAmount);

    // Alice approves fDAIx contract to spend fDAI
    await fdai.connect(alice).approve(daix.address, transferInAmount.add(transferInAmount));

    // Alice upgrades fDAI to fDAIx
    const daiXUpgradeOperation = daix.upgrade({
      amount: transferInAmount
    });
    const upgradeTx = await daiXUpgradeOperation.exec(alice);
    await upgradeTx.wait();

    // Transfer fDAIx into the tokenSpreader
    const daiXTransferOperation = daix.transfer({
      receiver: tokenSpreader.address,
      amount: transferInAmount
    });
    const transferTx = await daiXTransferOperation.exec(alice);
    await transferTx.wait(); // Waiting for the transfer transaction to mine so we can correctly view the balance change below

    console.log( 'New TokenSpreader SpreaderToken Balance:', await daix.balanceOf({account: tokenSpreader.address, providerOrSigner: alice }));

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