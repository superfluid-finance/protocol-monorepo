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
        return address(new SuperTokenMock());
    }

}
