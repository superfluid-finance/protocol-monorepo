import { ethers } from 'hardhat'
let { artifacts } = require("hardhat");
import { Framework } from '@superfluid-finance/sdk-core';
import { network } from "../hardhat.config";
import * as dotenv from "dotenv";

dotenv.config();

const DividendRightsToken = artifacts.require("DividendRightsToken");

async function main() {

  const provider = new ethers.providers.JsonRpcProvider(process.env.RPC_URL)
  const sf = await Framework.create({
     networkName: network,
     provider,
     dataMode: "WEB3_ONLY",
     resolverAddress: process.env.RESOLVER_ADDRESS, // this is how you get the resolver address
     protocolReleaseVersion: "test",
    });

  const daix = await sf.loadSuperToken("fDAIx");

  const signers = await ethers.getSigners();
  const DividendRightsToken = await ethers.getContractFactory("DividendRightsToken", signers[0])

  const dividendrightstoken =  await DividendRightsToken.deploy(
    "Dividend Rights Token",
        "DRT",
        daix.address,
        sf.settings.config.hostAddress
  );

  await dividendrightstoken.deployed();

  console.log("Contract deployed to:", dividendrightstoken.address);
  
}

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
})