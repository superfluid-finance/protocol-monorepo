import { ethers, Overrides } from "ethers";

import Operation from "./Operation";
import { Superfluid, Superfluid__factory } from "./typechain-types";

/**
 * Host Helper Class
 * @description A helper class which can be used as a standalone class to populate call agreement transactions.
 */
export default class Host {
    contract: Superfluid;

    constructor(hostAddress: string) {
        this.contract = new ethers.Contract(
            hostAddress,
            Superfluid__factory.abi
        ) as Superfluid;
    }

    /**
     * Creates an Operation of the `callAgreement` function on the host contract.
     * @param agreementAddress the agreement address (cfa or ida address)
     * @param callData the encoded callData for the function
     * @param userData any additional user data
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} an `Operation` class
     */
    callAgreement = (
        agreementAddress: string,
        callData: string,
        userData: string | undefined,
        overrides?: Overrides & { from?: string }
    ): Operation => {
        const txn = this.contract.populateTransaction.callAgreement(
            agreementAddress,
            callData,
            userData || "0x",
            overrides || {}
        );
        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    };

    /**
     * Creates an Operation of the `callAppAction` function on the host contract.
     * @param app the address of the Super App you are calling
     * @param callData the encoded callData for the function
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} an `Operation` class
     */
    callAppAction = (
        app: string,
        callData: string,
        overrides?: Overrides & { from?: string }
    ): Operation => {
        const txn = this.contract.populateTransaction.callAppAction(
            app,
            callData,
            overrides || {}
        );
        return new Operation(txn, "CALL_APP_ACTION");
    };
}
