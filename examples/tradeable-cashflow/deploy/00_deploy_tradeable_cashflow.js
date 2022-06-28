require("@nomiclabs/hardhat-ethers");

//kovan addresses - change if using a different network
const host = '0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3';
const fDAIx = '0xe3cb950cb164a31c66e32c320a800d477019dcff';

//your address here...
const owner = "0x5966aa11c794893774a382d9a19743B8be6BFFd1";

//to deploy, run yarn hardhat deploy --network kovan

module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;

  const { deployer } = await getNamedAccounts();
  console.log(deployer);

  await deploy("TradeableCashflow", {
    from: deployer,
    args: [owner, 'Tradeable Cashflow', 'TCF', host, fDAIx],
    log: true,
  })
};
module.exports.tags = ["TradeableCashflow"];
