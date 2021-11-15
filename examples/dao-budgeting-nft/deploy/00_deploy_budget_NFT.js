//rinkeby addresses - change if using a different network
require("ethers");

//kovan addresses
const host = '0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3';
const cfa = '0xECa8056809e7e8db04A8fF6e4E82cD889a46FE2F';
const fDAIx = '0xe3CB950Cb164a31C66e32c320A800D477019DCFF';

module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;

  const { deployer } = await getNamedAccounts();
  console.log(deployer);

  const nft = await deploy("BudgetNFT", {
    from: deployer,
    args: ["BudgetNFT Example", 'SFBudget', host, cfa, fDAIx],
    log: true,
  })
  await nft.deployTransaction?.wait()
  nftAddress = nft.address
  console.log("BudgetNFT address:", nftAddress)

};
module.exports.tags = ["BudgetNFT"];
