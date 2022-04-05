// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

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
