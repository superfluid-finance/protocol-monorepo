import { ethers } from "ethers";

import Operation from "./Operation";

export default class SuperfluidAgreement {
    /**
     * Returns the desired Operation based on shouldUseCallAgreement.
     * @param shouldUseCallAgreement whether or not to use host.callAgreement
     * @param callAgreementOperation the host.callAgreement created Operation
     * @param forwarderPopulatedTransactionPromise the populated forwarder transaction promise
     */
    _getCallAgreementOperation = (
        callAgreementOperation: Operation,
        forwarderPopulatedTransactionPromise?: Promise<ethers.PopulatedTransaction>,
        shouldUseCallAgreement?: boolean
    ) => {
        return shouldUseCallAgreement
            ? callAgreementOperation
            : new Operation(
                  callAgreementOperation.populateTransactionPromise,
                  callAgreementOperation.type,
                  forwarderPopulatedTransactionPromise
              );
    };
}
