import hre, { ethers } from "hardhat";
import { verifyContract } from "./verify";
import { Contract } from "ethers";

async function main() {
    try {
        const simpleACLCloseResolverFactory = await ethers.getContractFactory(
            "SimpleACLCloseResolver",
        );

        console.log("Deploying SimpleACLCloseResolver Contract");
        const simpleACLCloseDeployTxn =
            await simpleACLCloseResolverFactory.deploy(
                process.env.END_TIME || "",
                process.env.CFA_ADDRESS || "",
                process.env.SUPERTOKEN_ADDRESS || "",
                process.env.SENDER_ADDRESS || "",
                process.env.RECEIVER_ADDRESS || "",
            );

        const typedContract = simpleACLCloseDeployTxn as unknown as Contract;
        console.log("CONTRACT DEPLOYED AT:", typedContract.address);

        console.log("Awaiting 6 confirmations...");
        await typedContract.deployTransaction.wait(6);

        // NOTE: strange issue where address property doesn't exist on contract type
        console.log("SimpleACLCloseResolver Address:", typedContract.address);

        // programmatically verify the contract in production
        // https://hardhat.org/plugins/nomiclabs-hardhat-etherscan.html#using-programmatically
        if (
            process.env.SKIP_VERIFICATION ||
            (hre.network.name !== "hardhat" && hre.network.name !== "localhost")
        ) {
            await verifyContract(hre, typedContract.address || "", [
                process.env.END_TIME || "",
                process.env.CFA_ADDRESS || "",
                process.env.SUPERTOKEN_ADDRESS || "",
                process.env.SENDER_ADDRESS || "",
                process.env.RECEIVER_ADDRESS || "",
            ]);
        }
    } catch (err) {
        console.error(err);
    }
}

main()
    .then(() => process.exit(0))
    .catch(err => {
        console.error(err);
        process.exit(1);
    });
