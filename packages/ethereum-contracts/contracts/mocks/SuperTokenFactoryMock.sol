// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

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
contract SuperTokenMockFactory {

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
    SuperTokenMockFactory immutable private _f;

    constructor(
        ISuperfluid host,
        SuperTokenMockFactory f
    )
        SuperTokenFactoryBase(host)
    {
        _f = f;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        return _f.create(host, 0);
    }

}

contract SuperTokenFactoryMock42 is SuperTokenFactoryBase
{

    SuperTokenMockFactory immutable private _f;

    constructor(
        ISuperfluid host,
        SuperTokenMockFactory f
    )
        SuperTokenFactoryBase(host)
    {
        _f = f;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        return _f.create(host, 42);
    }

}
