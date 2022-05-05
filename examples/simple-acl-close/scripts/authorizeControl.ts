import { ethers } from "hardhat";
import { Framework } from "@superfluid-finance/sdk-core";

const authorizeControl = async () => {
    try {
        console.log("START SCRIPT");
        // SENDER_ADDRESS is the sender that is authorizing full control
        const ethersSigner = await ethers.getSigner(
            process.env.SENDER_ADDRESS || "",
        );
        const provider = ethers.provider;

        console.log("Create Superfluid Framework with SDK-Core");
        const sf = await Framework.create({
            chainId: Number(process.env.CHAIN_ID),
            provider: provider,
        });

        // create a signer using the ethersSigner
        const signer = sf.createSigner({
            signer: ethersSigner,
        });

        // create updateFlowOperatorPermissions for delete operation
        const op = sf.cfaV1.updateFlowOperatorPermissions({
            superToken: process.env.SUPERTOKEN_ADDRESS || "",

            // this is the Gelato Ops address for the network you deploy this on:
            // see https://docs.gelato.network/resources/contract-addresses for a list of addresses
            flowOperator: process.env.FLOW_OPERATOR_ADDRESS || "",
            permissions: 4, // delete only
            flowRateAllowance: "0",
        });

        console.log(
            "Execute Authorize Flow Operator with Full Control Operation...",
        );
        const txn = await op.exec(signer);

        console.log("Transaction broadcasted, waiting...");
        const receipt = await txn.wait();

        console.log("Transaction has been mined.");
        console.log("Transaction Receipt:", receipt);
    } catch (err) {
        console.error(err);
    }
};

(async () => {
    await authorizeControl();
})();
