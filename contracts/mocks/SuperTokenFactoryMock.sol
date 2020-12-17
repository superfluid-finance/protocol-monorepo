// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import { SuperTokenMock } from "./SuperTokenMock.sol";
import { SuperTokenFactoryBase } from "../superfluid/SuperTokenFactory.sol";


contract SuperTokenFactoryMock is SuperTokenFactoryBase
{

    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        assembly { slot:= _host.slot offset := _host.offset }
        require (slot == 0 && offset == 2, "_host changed location");

        assembly { slot:= _superTokenLogic.slot offset := _superTokenLogic.offset }
        require (slot == 1 && offset == 0, "_superTokenLogic changed location");
    }

    function createSuperTokenLogic()
        external override
        returns (address logic)
    {
        SuperTokenMock superToken = new SuperTokenMock(0);
        return address(superToken);
    }

}

contract SuperTokenFactory42Mock is SuperTokenFactoryBase
{

    function createSuperTokenLogic()
        external override
        returns (address logic)
    {
        SuperTokenMock superToken = new SuperTokenMock(42);
        return address(superToken);
    }

}
