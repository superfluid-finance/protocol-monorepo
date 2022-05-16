const { ethers } = require("hardhat");
const { assert, expect } = require("chai");
const { Framework } = require("@superfluid-finance/sdk-core");
const SuperfluidFrameworkDeployer = require("@superfluid-finance/ethereum-contracts/build/contracts/SuperfluidFrameworkDeployer.json");

const one = ethers.utils.parseEther("1").toString();

    // Deploying signer
let deployer, 
    // Alice signer
    alice,
    // Bob signer
    bob,
    // Superfluid deployer contract
    superfluidFrameworkDeployer,
    // Superfluid sdk-core framework instance
    sf,
    // Underlying ERC20 of `inToken`
    inUnderlyingToken,
    // Underlying ERC20 of `outToken`
    outUnderlyingToken,
    // Super token to stream in
    inToken,
    // Super token to distribute out
    outToken;

before(async function () {
    [deployer, alice, bob] = await ethers.getSigners();
    superfluidFrameworkDeployer = await new ethers.ContractFactory(
        SuperfluidFrameworkDeployer.abi,
        SuperfluidFrameworkDeployer.bytecode,
        deployer
    ).deploy();

    console.log(await superfluidFrameworkDeployer.getFramework());
});

describe('smoke', async function () {
    it('smoke', async function() {
        assert(true);
    })
})
