const hre = require("hardhat");
const { Framework } = require("@superfluid-finance/sdk-core");
const { ethers, providers } = require("ethers");
require("dotenv");
// const TradeableCashflow = require("../artifacts/contracts/TradeableCashflow.sol/TradeableCashflow.json");
// const TradeableCashflowABI = TradeableCashflow.abi;

async function main() {

  const url = `${process.env.KOVAN_RPC_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);

  const sf = await Framework.create({
    chainId: 42,
    provider: customHttpProvider
  });
  const signer = sf.createSigner({
    privateKey:
      process.env.DEPLOYER_PRIVATE_KEY,
    provider: customHttpProvider
  });

  const daix = await sf.loadSuperToken("fDAIx");

  const deleteFlowOperation = sf.cfaV1.deleteFlow({
      sender: signer.address,
      receiver: "0x1114a6880EF8cE89Cf108830eaE2f3D66a4951ca", //tradeable cashflow address
      superToken: daix.address,
  });

  const txn = await deleteFlowOperation.exec(signer);

  const receipt = await txn.wait();

  console.log(receipt);
  
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });