// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { SuperTokenMock } from "./SuperTokenMock.sol";
import {
    SuperTokenFactoryBase,
    ISuperfluid
} from "../superfluid/SuperTokenFactory.sol";

contract SuperTokenFactoryStorageLayoutTester is SuperTokenFactoryBase {

    constructor(
        ISuperfluid host
    )
        SuperTokenFactoryBase(host)
        // solhint-disable-next-line no-empty-blocks
    {
    }

    // @dev Make sure the storage layout never change over the course of the development
    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        // Initializable bool _initialized and bool _initialized

        assembly { slot:= _superTokenLogic.slot offset := _superTokenLogic.offset }
        require (slot == 0 && offset == 2, "_superTokenLogic changed location");
    }

    // dummy impl
    function createSuperTokenLogic(ISuperfluid)
        external pure override
        returns (address)
    {
        return address(0);
    }
}

// spliting this off because the contract is getting bigger
contract SuperTokenFactoryMockHelper {
    function create(ISuperfluid host, uint256 waterMark)
        external
        returns (address logic)
    {
        SuperTokenMock superToken = new SuperTokenMock(host, waterMark);
        return address(superToken);
    }
}

contract SuperTokenFactoryMock is SuperTokenFactoryBase
{
    SuperTokenFactoryMockHelper immutable private _helper;

    constructor(
        ISuperfluid host,
        SuperTokenFactoryMockHelper helper
    )
        SuperTokenFactoryBase(host)
    {
        _helper = helper;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        return _helper.create(host, 0);
    }

    function initializeCanonicalWrapperSuperTokens(
        InitializeData[] calldata _data
    ) external override {
        address nativeAssetSuperTokenAddress = _canonicalWrapperSuperTokens[
            address(0)
        ];

        // hardcoded address has permission to set this list
        // This is the first address in hardhat node
        if (msg.sender != 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266) revert("");

        // once the list has been set, it cannot be reset
        // @note this means that we must set the 0 address
        if (nativeAssetSuperTokenAddress != address(0)) revert("");

        // initialize mapping
        for (uint256 i = 0; i < _data.length; i++) {
            _canonicalWrapperSuperTokens[_data[i].underlyingToken] = _data[i]
                .superToken;
        }
    }
}

contract SuperTokenFactoryMock42 is SuperTokenFactoryBase
{

    SuperTokenFactoryMockHelper immutable private _helper;

    constructor(
        ISuperfluid host,
        SuperTokenFactoryMockHelper helper
    )
        SuperTokenFactoryBase(host)
    {
        _helper = helper;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        return _helper.create(host, 42);
    }

}
