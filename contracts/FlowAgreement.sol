pragma solidity 0.5.17;

import "./ISuperAgreement.sol";

/**
 * @title Superfluid's flow agreement
 * @notice 
 * @author Superfluid
 */
contract FlowAgreement is ISuperAgreement {

    enum FlowRateType {
        FLOW_PER_BLOCK,
        FLOW_PER_DAY,
        FLOW_PER_MONTH,
        FLOW_PER_YEAR
    }

    function balanceOf(bytes calldata state) external pure
        returns (uint256 amount) {
        return 0;
    }

    function composeState(
        bytes calldata currentState,
        bytes calldata additionalState) external pure
        returns (bytes memory newState) {
        newState = additionalState;
    }

}
