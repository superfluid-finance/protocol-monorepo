// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma experimental ABIEncoderV2;

import {
    Superfluid,
    ISuperApp
} from "../superfluid/Superfluid.sol";

import { CallUtils } from "../utils/CallUtils.sol";


contract SuperfluidMock is Superfluid {


    constructor(bool nonUpgradable)
        Superfluid(nonUpgradable)
    // solhint-disable-next-line no-empty-blocks
    {
    }

    /**
     * Upgradability
     */

    function validateStorageLayout()
        external pure
    {
        uint256 slot;
        uint256 offset;

        assembly { slot:= _gov.slot offset := _gov.offset }
        require (slot == 0 && offset == 2, "_gov changed location");

        assembly { slot:= _agreementClasses.slot offset := _agreementClasses.offset }
        require (slot == 1 && offset == 0, "_agreementClasses changed location");

        assembly { slot:= _agreementClassIndices.slot offset := _agreementClassIndices.offset }
        require (slot == 2 && offset == 0, "_agreementClassIndices changed location");

        assembly { slot:= _superTokenFactory.slot offset := _superTokenFactory.offset }
        require (slot == 3 && offset == 0, "_superTokenFactory changed location");

        assembly { slot:= _appManifests.slot offset := _appManifests.offset }
        require (slot == 4 && offset == 0, "_appManifests changed location");

        assembly { slot:= _compositeApps.slot offset := _compositeApps.offset }
        require (slot == 5 && offset == 0, "_compositeApps changed location");

        assembly { slot:= _ctxStamp.slot offset := _ctxStamp.offset }
        require (slot == 6 && offset == 0, "_ctxStamp changed location");
    }

    function ctxFunc1(uint256 n, bytes calldata ctx)
        external pure
        returns (uint256, bytes memory)
    {
        return (n, ctx);
    }

    // same ABI to afterAgreementCreated
    function ctxFunc2(
        address superToken,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx)
        external pure
        returns (address, address, bytes32, bytes memory, bytes memory, bytes memory)
    {
        return (superToken, agreementClass, agreementId, agreementData, cbdata, ctx);
    }

    function testCtxFuncX(bytes calldata dataWithPlaceHolderCtx, bytes calldata ctx)
        external view
        returns (bytes memory returnedData)
    {
        bytes memory data = _replacePlaceholderCtx(dataWithPlaceHolderCtx, ctx);
        bool success;
        (success, returnedData) = address(this).staticcall(data);
        if (success) return returnedData;
        else revert(CallUtils.getRevertMsg(returnedData));
    }

    function testIsValidAbiEncodedBytes() external pure {
        require(CallUtils.isValidAbiEncodedBytes(abi.encode(new bytes(0))), "0");
        require(CallUtils.isValidAbiEncodedBytes(abi.encode(new bytes(1))), "1");
        require(CallUtils.isValidAbiEncodedBytes(abi.encode(new bytes(32))), "32");
        require(CallUtils.isValidAbiEncodedBytes(abi.encode(new bytes(33))), "33");
    }

    function jailApp(ISuperApp app)
        external
    {
        _jailApp(app, 0);
    }

}
