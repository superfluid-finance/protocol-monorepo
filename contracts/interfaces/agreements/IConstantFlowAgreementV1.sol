// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import "../superfluid/ISuperToken.sol";
import "../superfluid/ISuperAgreement.sol";


/**
 * @dev Superfluid's constant flow agreement interface
 *
 * @author Superfluid
 */
abstract contract IConstantFlowAgreementV1 is ISuperAgreement {

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    }

    /**
     * @dev Create a flow betwen sender and receiver.
     * @param token Super token address.
     * @param receiver Flow receiver address.
     * @param flowRate New flow rate in amount per second.
     *
     * NOTE:
     * - A deposit is taken as safety margin for the solvency agents.
     * - A extra gas fee may be taken to pay for solvency agent liquidations.
     */
    function createFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes calldata ctx
    )
        external
        virtual
        returns(bytes memory newCtx);

    /**
     * @dev Update the flow rate between sender and receiver.
     * @param token Super token address.
     * @param receiver Flow receiver address.
     * @param flowRate New flow rate in amount per second.
     *
     * NOTE:
     * - Only the flow sender may update the flow rate.
     * - Even if the flow rate is zero, the flow is not deleted
     * from the system.
     * - Deposit amount will be adjusted accordingly.
     * - No new gas fee is charged.
     */
    function updateFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes calldata ctx
    )
        external
        virtual
        returns(bytes memory newCtx);


    /**
     * @dev Get the flow data between `sender` and `receiver`.
     * @param token Super token address.
     * @param sender Flow receiver.
     * @param receiver Flow sender.
     * @return timestamp Timestamp of when the flow is updated.
     * @return flowRate The flow rate.
     * @return deposit The amount of deposit the flow.
     * @return owedDeposit The amount of owed deposit of the flow.
     */
    function getFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        virtual
        returns (
            uint256 timestamp,
            int96 flowRate,
            uint256 deposit,
            uint256 owedDeposit
        );

    /**
     * @dev Get flow data using agreement ID
     * @param token Super token address.
     * @param agreementId The agreement ID.
     * @return timestamp Timestamp of when the flow is updated.
     * @return flowRate The flow rate.
     * @return deposit The amount of deposit the flow.
     * @return owedDeposit The amount of owed deposit of the flow.
     */
    function getFlowByID(
       ISuperToken token,
       bytes32 agreementId
    )
        external
        view
        virtual
        returns (
            uint256 timestamp,
            int96 flowRate,
            uint256 deposit,
            uint256 owedDeposit
        );

    /**
     * @dev Get the net flow rate of the account
     * @param token Super token address.
     * @param account Account for the query.
     * @return flowRate Flow rate.
     */
    function getNetFlow(
       ISuperToken token,
       address account)
       external
       view
       virtual
       returns (int96 flowRate);

    /**
     * @dev Delete the flow between sender and receiver
     * @param token Super token address.
     * @param ctx Context bytes.
     * @param receiver Flow receiver address.
     *
     * NOTE:
     * - Both flow sender and receiver may delete the flow.
     * - If Sender account is insolvent or in critical state, a solvency agent may
     *   also terminate the agreement.
     * - Gas fee may be returned to the sender.
     */
    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver,
        bytes calldata ctx
    )
        external
        virtual
        returns(bytes memory newCtx);

     /**
      * @dev Flow updated event.
      * @param token Super token address.
      * @param sender Flow sender address.
      * @param receiver Flow recipient address.
      * @param flowRate Flow rate in amount per second for this flow.
      * @param flowRate Total flow rate in amount per second for the sender.
      * @param flowRate Total flow rate in amount per second for the receiver.
      */
     event FlowUpdated(
         ISuperToken indexed token,
         address indexed sender,
         address indexed receiver,
         int96 flowRate,
         int256 totalSenderFlowRate,
         int256 totalReceiverFlowRate
     );

}
