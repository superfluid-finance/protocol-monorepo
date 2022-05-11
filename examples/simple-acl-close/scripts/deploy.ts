import hre, { ethers } from "hardhat";
import { verifyContract } from "./verify";

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

        console.log("CONTRACT DEPLOYED AT:", simpleACLCloseDeployTxn.address);

        console.log("Awaiting 6 confirmations before verification...");
        await simpleACLCloseDeployTxn.deployTransaction.wait(6);

        console.log(
            "SimpleACLCloseResolver Address:",
            simpleACLCloseDeployTxn.address,
        );

        // programmatically verify the contract in production
        // https://hardhat.org/plugins/nomiclabs-hardhat-etherscan.html#using-programmatically
        if (
            process.env.SKIP_VERIFICATION ||
            (hre.network.name !== "hardhat" && hre.network.name !== "localhost")
        ) {
            await verifyContract(hre, simpleACLCloseDeployTxn.address || "", [
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
