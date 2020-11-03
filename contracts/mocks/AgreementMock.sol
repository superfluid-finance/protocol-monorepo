// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";
import { AgreementBase } from "../agreements/AgreementBase.sol";


contract AgreementMock is AgreementBase {

    uint256 constant private _REAL_TIME_BALANCE_SLOT_ID = 65552025;

    // using immutable, otherweise the proxy contract would see zero instead
    bytes32 immutable private _type;
    uint immutable private _version;

    constructor(bytes32 t, uint v) {
        _type = t;
        _version = v;
    }

    function version() external view returns (uint) { return _version; }

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external override view returns (bytes32) {
        return _type;
    }

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
       ISuperfluidToken token,
       address account,
       uint256 /* time */
    )
       external view override
       returns (
           int256 dynamicBalance,
           uint256 deposit,
           uint256 owedDeposit
       )
    {
        bytes32[] memory slotData = token.getAgreementStateSlot(
            address(this), account, _REAL_TIME_BALANCE_SLOT_ID, 3);
        return (
            int256(slotData[0]),
            uint256(slotData[1]),
            uint256(slotData[2])
        );
    }

    /**
     * Real-time balance mockings
     */

    function setRealtimeBalanceFor(
        ISuperfluidToken token,
        address account,
        int256 dynamicBalance,
        uint256 deposit,
        uint256 owedDeposit
    ) external {
        bytes32[] memory slotData = new bytes32[](3);
        slotData[0] = bytes32(uint256(dynamicBalance));
        slotData[1] = bytes32(uint256(deposit));
        slotData[2] = bytes32(uint256(owedDeposit));
        token.updateAgreementStateSlot(account, _REAL_TIME_BALANCE_SLOT_ID, slotData);
    }

    /**
     * Agreement client mockings
     */

    function createAgreementFor(
        ISuperfluidToken token,
        bytes32 id,
        bytes32[] calldata data
        ) external {
        token.createAgreement(id, data);
    }

    function updateAgreementDataFor(
        ISuperfluidToken token,
        bytes32 id,
        bytes32[] calldata data
        ) external {
        token.updateAgreementData(id, data);
    }

    function terminateAgreementFor(
        ISuperfluidToken token,
        bytes32 id,
        uint dataLength
        ) external {
        token.terminateAgreement(id, dataLength);
    }

    function updateAgreementStateSlotFor(
        ISuperfluidToken token,
        address account,
        uint256 slotId,
        bytes32[] calldata slotData
        ) external {
        token.updateAgreementStateSlot(account, slotId, slotData);
    }

    function settleBalanceFor(
        ISuperfluidToken token,
        address account,
        int256 delta
        ) external {
        token.settleBalance(account, delta);
    }

    function liquidateAgreementFor(
        ISuperfluidToken token,
        bytes32 id,
        address liquidator,
        address penaltyAccount,
        uint256 rewardAmount,
        uint256 bailoutAmount
    ) external {
        token.liquidateAgreement(id, liquidator, penaltyAccount, rewardAmount, bailoutAmount);
    }

}
