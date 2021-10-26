// deploy/00_deploy_your_contract.js

//const { ethers } = require("hardhat");

//mumbai
const host = '0xEB796bdb90fFA0f28255275e16936D25d3418603';
const cfa = '0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873';
const fDAIx = '0x5D8B4C2554aeB7e86F387B4d6c00Ac33499Ed01f';
const owner = '0x00471Eaad87b91f49b5614D452bd0444499c1bd9';
// const acct1 = "0x5966aa11c794893774a382d9a19743B8be6BFFd1";
// const acct2 = "0x9421FE8eCcAfad76C3A9Ec8f9779fAfA05A836B3";

module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;
    //change this 'deployer' to HD wallet provider and send from a different account


    // await hre.network.provider.request({
    //   method: "hardhat_impersonateAccount",
    //   params: [acct1],
    // });

    // await hre.network.provider.request({
    //   method: "hardhat_impersonateAccount",
    //   params: [acct2],
    // });


  const { deployer } = await getNamedAccounts();
  console.log(deployer);

  // await deploy("YourContract", {
  //   // Learn more about args here: https://www.npmjs.com/package/hardhat-deploy#deploymentsdeploy
  //   from: deployer,
  //   //args: [ "Hello", ethers.utils.parseEther("1.5") ],
  //   log: true,
  // });

  /*
    // Getting a previously deployed contract
    const YourContract = await ethers.getContract("YourContract", deployer);
    await YourContract.setPurpose("Hello");
  
    To take ownership of yourContract using the ownable library uncomment next line and add the 
    address you want to be the owner. 
    // yourContract.transferOwnership(YOUR_ADDRESS_HERE);

    //const yourContract = await ethers.getContractAt('YourContract', "0xaAC799eC2d00C013f1F11c37E654e59B0429DF6A") //<-- if you want to instantiate a version of a contract at a specific address!
  */

  /*
  //If you want to send value to an address from the deployer
  const deployerWallet = ethers.provider.getSigner()
  await deployerWallet.sendTransaction({
    to: "0x34aA3F359A9D614239015126635CE7732c18fDF3",
    value: ethers.utils.parseEther("0.001")
  })
  */

  /*
  //If you want to send some ETH to a contract on deploy (make your constructor payable!)
  const yourContract = await deploy("YourContract", [], {
  value: ethers.utils.parseEther("0.05")
  });
  */

  /*
  //If you want to link a library into your contract:
  // reference: https://github.com/austintgriffith/scaffold-eth/blob/using-libraries-example/packages/hardhat/scripts/deploy.js#L19
  const yourContract = await deploy("YourContract", [], {}, {
   LibraryName: **LibraryAddress**
  });
  */
};
module.exports.tags = ["YourContract"];
