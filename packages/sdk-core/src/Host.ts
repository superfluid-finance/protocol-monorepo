import { ethers } from "ethers";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { handleError } from "./errorHelper";
import Operation from "./Operation";
import { Superfluid } from "./typechain";

/**
 * @dev Host Helper Class
 * @description A helper class which can be used as a standalone class to populate call agreement transactions.
 */
export default class Host {
    hostContract: Superfluid;

    constructor(hostAddress: string) {
        this.hostContract = new ethers.Contract(
            hostAddress,
            SuperfluidABI
        ) as Superfluid;
    }

    populateCallAgreementTxnAndReturnOperation = async (
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
            return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
        } catch (err) {
            return handleError(
                "POPULATE_TRANSACTION",
                "There was an error populating the transaction",
                JSON.stringify(err)
            );
        }
    };
}
