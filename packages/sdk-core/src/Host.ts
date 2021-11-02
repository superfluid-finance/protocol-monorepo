import { ethers } from "ethers";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { handleError } from "./errorHelper";
import Operation from "./Operation";
import { Superfluid } from "./typechain";

export default class Host {
    hostContract: Superfluid;

    constructor(hostAddress: string) {
        this.hostContract = new ethers.Contract(
            hostAddress,
            SuperfluidABI
        ) as Superfluid;
    }

    populateTransactionAndReturnOperation = async (
        agreementAddress: string,
        callData: string,
        userData: string | undefined
    ) => {
        try {
            const txn =
                await this.hostContract.populateTransaction.callAgreement(
                    agreementAddress,
                    callData,
                    userData || "0x"
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };
}
