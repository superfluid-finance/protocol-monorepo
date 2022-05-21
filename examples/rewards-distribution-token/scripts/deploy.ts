import { ethers } from 'hardhat'
let { artifacts } = require("hardhat");
import { DividendRightsToken__factory  } from "../typechain-types";

const DividendRightsToken = artifacts.require("DividendRightsToken");

async function main() {
  const signers = await ethers.getSigners();
  const DividendRightsToken__factory = await ethers.getContractFactory("DividendRightsToken__factory")

  const dividendrightstoken =  await DividendRightsToken.deploy();

  await dividendrightstoken.deployed();

  console.log("Contract deployed to:", dividendrightstoken.address);
  
}

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
})