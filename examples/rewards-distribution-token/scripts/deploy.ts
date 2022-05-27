import { ethers } from 'hardhat';
let { artifacts } = require("hardhat");
import { Framework } from '@superfluid-finance/sdk-core';
import * as dotenv from "dotenv";
import { DividendRightsToken } from "../typechain-types/contracts";


dotenv.config();

let dividendRightsToken: DividendRightsToken;

async function main() {

  const provider = new ethers.providers.JsonRpcProvider(process.env.RPC_URL);
  await provider.getNetwork(); // wait for this to ensure network info is available
  console.log(`Connected to network ${provider.network.name} with chainId ${provider.network.chainId}`);

  const sf = await Framework.create({
    chainId: provider.network.chainId,
    provider
  });

  const hostAddr = sf.host.contract.address;  
  const daix = await sf.loadSuperToken("fDAIx");

  console.log(`Superfluid: host address ${hostAddr}, daix address ${daix.address}`);

  const signer = (await ethers.getSigners())[0];
  const signerBalance = await provider.getBalance(signer.address);

  console.log(`Using signer ${signer.address} with native token balance: ${signerBalance.toString()}`);

  const drtContractFactory = await ethers.getContractFactory(
      "DividendRightsToken",
      signer
  );
  
  console.log("Deploying...");
  const dividendRightsToken = await drtContractFactory.deploy(
    "Dividend Rights Token",
    "DRT",
    daix.address,
    hostAddr
  );
  const drt = await dividendRightsToken.deployed();
  console.log("Dividend Rights Token deployed to:", drt.address);

  console.log("You can verify the contract with:");
  console.log(`npx hardhat verify ${drt.address} "Dividend Rights Token" "DRT" ${daix.address} ${hostAddr}`);
}

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
});