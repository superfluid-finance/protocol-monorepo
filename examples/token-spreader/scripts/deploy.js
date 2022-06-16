const hre = require("hardhat");

const gorliHostAddress = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
const gorliFDAIXAddress = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00"; 

async function main() {

  // We get the contract to deploy to Gorli Testnet
  const TokenSpreader = await hre.ethers.getContractFactory("TokenSpreader");
  const tokenSpreader = await TokenSpreader.deploy(
    gorliHostAddress,
    gorliFDAIXAddress
  );

  await tokenSpreader.deployed();

  console.log("Token Spreader deployed to:", tokenSpreader.address);
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});

// Deploy: npx hardhat run scripts/deploy.js --network goerli

// Verify: npx hardhat verify --network goerli --constructor-args arguments-tokenspreader.js [contractaddress]