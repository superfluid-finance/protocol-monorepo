import { ethers } from "ethers";

import Operation from "./Operation";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
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

    /**
     * @dev Creates an Operation of the `callAgreement` function on the host contract.
     * @param agreementAddress the agreement address (cfa or ida address)
     * @param callData the encoded callData for the function
     * @param userData any additional user data
     * @returns {Operation} an `Operation` class
     */
    populateCallAgreementTxnAndReturnOperation = (
        agreementAddress: string,
        callData: string,
        userData: string | undefined
    ): Operation => {
        const txn = this.hostContract.populateTransaction.callAgreement(
            agreementAddress,
            callData,
            userData || "0x"
        );
        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    };
}
