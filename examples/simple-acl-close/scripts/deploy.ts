import { ethers } from "hardhat";

async function main() {
    const simpleACLCloseResolverFactory = await ethers.getContractFactory(
        "SimpleACLCloseResolver",
    );
    const simpleACLCloseResolver = await simpleACLCloseResolverFactory.deploy(
        process.env.END_TIME,
        process.env.GAS_LIMIT,
        process.env.CFA_ADDRESS || "",
        process.env.SUPERTOKEN_ADDRESS || "",
        process.env.SENDER_ADDRESS || "",
        process.env.RECEIVER_ADDRESS || "",
    );

    const deployed = await simpleACLCloseResolver.deployed();

    // NOTE: strange issue where address property doesn't exist on contract type
    console.log("SimpleACLCloseResolver Address:", (deployed as any).address);
}

main()
    .then(() => process.exit(0))
    .catch(err => {
        console.error(err);
        process.exit(1);
    });
