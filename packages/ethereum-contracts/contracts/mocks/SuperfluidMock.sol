// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import {
    Superfluid,
    ISuperApp
} from "../superfluid/Superfluid.sol";

import { CallUtils } from "../libs/CallUtils.sol";


contract SuperfluidUpgradabilityTester is Superfluid {

    constructor() Superfluid(false, false)
    // solhint-disable-next-line no-empty-blocks
    {
    }

    // @dev Make sure the storage layout never change over the course of the development
    function validateStorageLayout()
        external pure
    {
        uint256 slot;
        uint256 offset;

        assembly { slot:= _gov.slot offset := _gov.offset }
        require(slot == 0 && offset == 2, "_gov changed location");

        assembly { slot:= _agreementClasses.slot offset := _agreementClasses.offset }
        require(slot == 1 && offset == 0, "_agreementClasses changed location");

        assembly { slot:= _agreementClassIndices.slot offset := _agreementClassIndices.offset }
        require(slot == 2 && offset == 0, "_agreementClassIndices changed location");

        assembly { slot:= _superTokenFactory.slot offset := _superTokenFactory.offset }
        require(slot == 3 && offset == 0, "_superTokenFactory changed location");

        assembly { slot:= _appManifests.slot offset := _appManifests.offset }
        require(slot == 4 && offset == 0, "_appManifests changed location");

        assembly { slot:= _compositeApps.slot offset := _compositeApps.offset }
        require(slot == 5 && offset == 0, "_compositeApps changed location");

        assembly { slot:= _ctxStamp.slot offset := _ctxStamp.offset }
        require(slot == 6 && offset == 0, "_ctxStamp changed location");

        assembly { slot:= _appKeysUsedDeprecated.slot offset := _appKeysUsedDeprecated.offset }
        require(slot == 7 && offset == 0, "_appKeysUsedDeprecated changed location");
    }

    // @dev Make sure the context struct layout never change over the course of the development
    function validateContextStructLayout()
        external pure
    {
        // context.appLevel
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 0)), 42) }
            require(context.appLevel == 42, "appLevel changed location");
        }
        // context.callType
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 1)), 42) }
            require(context.callType == 42, "callType changed location");
        }
        // context.timestamp
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 2)), 42) }
            require(context.timestamp == 42, "timestamp changed location");
        }
        // context.msgSender
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 3)), 42) }
            require(context.msgSender == address(42), "msgSender changed location");
        }
        // context.agreementSelector
        {
            Context memory context;
            // be aware of the bytes4 endianness
            assembly { mstore(add(context, mul(32, 4)), shl(224, 0xdeadbeef)) }
            require(context.agreementSelector == bytes4(uint32(0xdeadbeef)), "agreementSelector changed location");
        }
        // context.userData
        {
            Context memory context;
            context.userData = new bytes(42);
            uint256 dataOffset;
            assembly { dataOffset := mload(add(context, mul(32, 5))) }
            require(dataOffset != 0, "userData offset is zero");
            uint256 dataLen;
            assembly { dataLen := mload(dataOffset) }
            require(dataLen == 42, "userData changed location");
        }
        // context.appAllowanceGranted
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 6)), 42) }
            require(context.appAllowanceGranted == 42, "appAllowanceGranted changed location");
        }
        // context.appAllowanceGranted
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 7)), 42) }
            require(context.appAllowanceWanted == 42, "appAllowanceWanted changed location");
        }
        // context.appAllowanceGranted
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 8)), 42) }
            require(context.appAllowanceUsed == 42, "appAllowanceUsed changed location");
        }
        // context.appAllowanceGranted
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 9)), 42) }
            require(context.appAddress == address(42), "appAddress changed location");
        }
        // context.appAllowanceToken
        {
            Context memory context;
            assembly { mstore(add(context, mul(32, 10)), 42) }
            require(address(context.appAllowanceToken) == address(42), "appAllowanceToken changed location");
        }
    }
}

contract SuperfluidMock is Superfluid {


    constructor(bool nonUpgradable, bool appWhiteListingEnabled)
        Superfluid(nonUpgradable, appWhiteListingEnabled)
    // solhint-disable-next-line no-empty-blocks
    {
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
        else CallUtils.revertFromReturnedData(returnedData);
    }

    function jailApp(ISuperApp app)
        external
    {
        _jailApp(app, 0);
    }

}
