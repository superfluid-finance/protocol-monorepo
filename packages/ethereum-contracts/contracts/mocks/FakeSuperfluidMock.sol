// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { CallUtils } from "../utils/CallUtils.sol";


contract FakeSuperfluidMock {

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
