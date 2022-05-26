// This DOES NOT deploy contracts with an upgradeable proxy, therefore these are not suitable for production deployment.
const { ethers } = require('hardhat')

const Resolver = require('@superfluid-finance/ethereum-contracts/build/contracts/Resolver.json')
const TestGovernance = require('@superfluid-finance/ethereum-contracts/build/contracts/TestGovernance.json')
const SuperfluidLoader = require('@superfluid-finance/ethereum-contracts/build/contracts/SuperfluidLoader.json')
const Superfluid = require('@superfluid-finance/ethereum-contracts/build/contracts/Superfluid.json')
const ConstantFlowAgreementV1 = require('@superfluid-finance/ethereum-contracts/build/contracts/ConstantFlowAgreementV1.json')
const SlotsBitmapLibrary = require('@superfluid-finance/ethereum-contracts/build/contracts/SlotsBitmapLibrary.json')
const SuperTokenFactoryHelper = require('@superfluid-finance/ethereum-contracts/build/contracts/SuperTokenFactoryHelper.json')
const SuperTokenFactory = require('@superfluid-finance/ethereum-contracts/build/contracts/SuperTokenFactory.json')

// const contract = await deploy(Contract.abi, Contract.bytecode, signer, [arg0, arg1])
async function deploy(abi, bytecode, signer, constructorArgs = []) {
	const factory = new ethers.ContractFactory(abi, bytecode, signer)
	return await factory.deploy(...constructorArgs)
}

async function deployWithLibs(name, signer, constructorArgs, libraries) {
	const factory = await ethers.getContractFactory(name, { signer, libraries })
	return await factory.deploy(...constructorArgs)
}

const technicallyNotZeroAddress = '0x0000000000000000000000000000000000000001'

async function deploySuperfluid(deployer) {
	// STEP 1
	// Deploy ERC1820 Registry
  	// use `yarn add hardhat-erc1820` and add `require('hardhat-erc1820');` to the hardhat config.
	// done by `hardhat-erc1820` plugin because bonk that nonsense

	// STEP 2
	// Deploy Resolver
	const resolver = await deploy(Resolver.abi, Resolver.bytecode, deployer, [])

	// STEP 3
	// Deploy Governance
	const governance = await deploy(TestGovernance.abi, TestGovernance.bytecode, deployer, [])

	// STEP 4
	// Register Governance with Resolver
	await resolver.connect(deployer).set('TestGovernance.test', governance.address)

	// STEP 5
	// Deploy Superfluid Loader
	const superfluidLoader = await deploy(
		SuperfluidLoader.abi,
		SuperfluidLoader.bytecode,
		deployer,
		[resolver.address]
	)

	// STEP 6
	// Register Loader with Resolver
	await resolver.connect(deployer).set('SuperfluidLoader-v1', superfluidLoader.address)

	// STEP 7
	// Deploy and initialize Superfluid contract
	const host = await deploy(Superfluid.abi, Superfluid.bytecode, deployer, [true, false])
	await host.initialize(governance.address)

	// STEP 8
	// Register Superfluid with Resolver
	await resolver.connect(deployer).set('Superfluid.test', host.address)

	// STEP 9
	// Initialize Governance
	await governance.connect(deployer).initialize(
		host.address,
		technicallyNotZeroAddress, // liquidation rewardAddress
		14400, // 4 hours
		900, // 15 minutes
		[]
	)

	// STEP 10
	// Deploy ConstantFlowAgreementV1
	const cfa = await deploy(
		ConstantFlowAgreementV1.abi,
		ConstantFlowAgreementV1.bytecode,
		deployer,
		[host.address]
	)

	// STEP 11
	// Register ConstantFlowAgreementV1 agreement class with Governance
	await governance.connect(deployer).registerAgreementClass(host.address, cfa.address)

	// STEP 12
	// Deploy SlotsBitmapLibrary
	const slotsBitmapLibrary = await deploy(
		SlotsBitmapLibrary.abi,
		SlotsBitmapLibrary.bytecode,
		deployer,
		[]
	)

	// STEP 13
	// Deploy InstantDistributionAgreementV1
	const ida = await deployWithLibs('InstantDistributionAgreementV1', deployer, [host.address], {
		SlotsBitmapLibrary: slotsBitmapLibrary.address
	})

	// STEP 14
	// Register InstantDistributionAgreementV1 agreement class with Governance
	await governance.connect(deployer).registerAgreementClass(host.address, ida.address)

	// STEP 15
	// Deploy SuperTokenFactoryHelper library
	const superTokenFactoryHelper = await deploy(
		SuperTokenFactoryHelper.abi,
		SuperTokenFactoryHelper.bytecode,
		deployer,
		[]
	)

	// STEP 16
	// Deploy SuperTokenFactory
	const superTokenFactory = await deploy(
		SuperTokenFactory.abi,
		SuperTokenFactory.bytecode,
		deployer,
		[host.address, superTokenFactoryHelper.address]
	)

	// STEP 17
	// 'Upgrade', but instead of upgrading, we're actually registering the SuperTokenFactory with
	// the Superfluid host contract. @superfluid-finance wen better deploy and upgrade scripps D:
	await governance
		.connect(deployer)
		.updateContracts(host.address, ethers.constants.AddressZero, [], superTokenFactory.address)

	// ??? -> PROFIT
	console.log('Superfluid Protocol successfully deployed\n')

	return resolver.address
}

module.exports = deploySuperfluid
