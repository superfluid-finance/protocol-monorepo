// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";
import { AgreementBase } from "../agreements/AgreementBase.sol";


contract AgreementMock is AgreementBase {

    // using immutable, otherweise the proxy contract would see zero instead
    bytes32 immutable private _agreementType;
    uint immutable private _version;

    constructor(bytes32 agreementType, uint version) {
        _agreementType = agreementType;
        _version = version;
    }

    function version() external view returns (uint) { return _version; }

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external override view returns (bytes32) {
        return _agreementType;
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
        bytes32[] memory slotData = token.getAgreementStateSlot(address(this), account, 0, 3);
        return (
            int256(slotData[0]),
            uint256(slotData[1]),
            uint256(slotData[2])
        );
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
        address liquidator,
        bytes32 id,
        address account,
        uint256 singleDeposit,
        uint256 totalDeposit
    ) external {
        token.liquidateAgreement(liquidator, id, account, singleDeposit, totalDeposit);
    }

}
