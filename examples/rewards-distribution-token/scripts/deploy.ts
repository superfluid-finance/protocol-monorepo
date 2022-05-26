import { ethers } from 'hardhat'
let { artifacts } = require("hardhat");
import { Framework } from '@superfluid-finance/sdk-core';
import * as dotenv from "dotenv";
import { DividendRightsToken } from "../typechain-types/contracts";


dotenv.config();

let dividendRightsToken: DividendRightsToken;

async function main() {

  const provider = new ethers.providers.JsonRpcProvider(process.env.RPC_URL)
  const sf = await Framework.create({
     networkName: "goerli",
     provider
    });

  const daix = await sf.loadSuperToken("fDAIx");

  const signers = await ethers.getSigners();

  const drtContractFactory = await ethers.getContractFactory(
      "DividendRightsToken",
      signers[0]
  );
  
  const dividendRightsToken = await drtContractFactory.deploy(
    "Dividend Rights Token",
    "DRT",
    daix.address,
    "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9"
);
const drt = await dividendRightsToken.deployed();
console.log("Dividend Rights Token Address:", drt.address);
  
}

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
})