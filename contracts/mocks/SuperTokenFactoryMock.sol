// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import { SuperTokenMock } from "./SuperTokenMock.sol";
import { SuperTokenFactoryBase } from "../superfluid/SuperTokenFactory.sol";


contract SuperTokenFactoryMock is SuperTokenFactoryBase
{

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
