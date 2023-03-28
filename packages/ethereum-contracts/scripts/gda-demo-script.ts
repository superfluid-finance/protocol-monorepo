import {ethers} from "hardhat";
import {IGeneralDistributionAgreementV1__factory} from "../typechain-types";
import {deployContractsAndToken} from "../dev-scripts/deploy-contracts-and-token";

// @note THIS IS A TEMPORARY SCRIPT SOLELY FOR THE DEMO

// this script is just used for demo testing
// it is to prevent me from having to manually do a bunch of stuff prior
// to the actual testing
async function main() {
    const {deployer, tokenDeploymentOutput} = await deployContractsAndToken();
    const signers = await ethers.getSigners();
    const contractAddresses = await deployer.getFramework();

    const superToken = await ethers.getContractAt(
        "SuperToken",
        tokenDeploymentOutput.wrapperSuperTokenData.wrapperSuperTokenAddress
    );
    const underlyingAddress = await superToken.getUnderlyingToken();
    const testToken = await ethers.getContractAt(
        "TestToken",
        underlyingAddress
    );
    // mint test tokens
    const INITIAL_AMOUNT = ethers.utils.parseEther("100000000");
    await testToken.mint(signers[0].address, INITIAL_AMOUNT);

    // approve test token spend by super token
    await testToken
        .connect(signers[0])
        .approve(superToken.address, INITIAL_AMOUNT);

    // upgrade all minted test tokens to super tokens
    await superToken.connect(signers[0]).upgrade(INITIAL_AMOUNT);

    // deploy GDA deployer contract (for ease of updating member units)
    const gdaDeployerFactory = await ethers.getContractFactory(
        "GDADeployer",
        signers[0]
    );
    // deploy gda deployer contract
    const gdaDeployer = await gdaDeployerFactory.deploy(
        contractAddresses.gda,
        superToken.address
    );
    await gdaDeployer.deployed();

    // get pool address
    const createPoolTxn = await gdaDeployer.connect(signers[0]).createPool();
    const createPoolReceipt = await createPoolTxn.wait();
    console.log(createPoolReceipt.events);
    const poolAddress = createPoolReceipt.events?.find(
        (e) => e.event === "PoolCreated"
    )?.args?.pool;
    console.log(poolAddress);

    // update member units for the pool
    await gdaDeployer.updateMemberUnits(
        signers.slice(1, 4).map((s) => s.address),
        [
            ethers.utils.parseEther("1"),
            ethers.utils.parseEther("1"),
            ethers.utils.parseEther("1"),
        ]
    );

    // distribute flow
    const hostContract = await ethers.getContractAt(
        "Superfluid",
        "0x3Ca8f9C04c7e3E1624Ac2008F92f6F366A869444"
    );
    const connectPoolCalldata =
        IGeneralDistributionAgreementV1__factory.createInterface().encodeFunctionData(
            "distributeFlow",
            ["0x6564e0C9e0F60572997864F195a63d94654f107F", "0xBFDcfA8EAEa4E638DB69ED718259DB49a35EC081", ethers.utils.parseEther("10"), "0x"]
        );
    const gm = await hostContract
        .connect(signers[0])
        .callAgreement("0xCdF0E532AB8eb9a12da5Cae3B6aE5370fAACD028", connectPoolCalldata, "0x");
    console.log(gm);
    const henlo = await gm.wait();
    console.log(henlo);
}

// try both:
// have everyone connect, then distribute flow
// distribute flow, then ask everyone to connect to the pool.

main()
    .then(() => process.exit(0))
    .catch((error) => {
        
        console.error(error);
        process.exit(1);
    });
