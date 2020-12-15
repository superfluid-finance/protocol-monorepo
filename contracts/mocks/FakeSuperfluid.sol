// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import { CallUtils } from "../utils/CallUtils.sol";


contract FakeSuperfluid {
    function callAgreement(
        address agreement,
        bytes calldata callData
    )
        external
    {
        bool success;
        bytes memory returnedData;
        // solhint-disable-next-line avoid-low-level-calls
        (success, returnedData) = agreement.call(callData);
        assert(!success);
        revert(CallUtils.getRevertMsg(returnedData));
    }
}
