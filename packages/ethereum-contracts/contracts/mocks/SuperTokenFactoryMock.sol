// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { SuperTokenMock } from "./SuperTokenMock.sol";
import {
    SuperTokenFactoryBase,
    ISuperfluid
} from "../superfluid/SuperTokenFactory.sol";


contract SuperTokenFactoryMock is SuperTokenFactoryBase
{

    constructor(
        ISuperfluid host
    )
        SuperTokenFactoryBase(host)
        // solhint-disable-next-line no-empty-blocks
    {
    }

    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        // Initializable bool _initialized and bool _initialized

        assembly { slot:= _superTokenLogic.slot offset := _superTokenLogic.offset }
        require (slot == 0 && offset == 2, "_superTokenLogic changed location");
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        SuperTokenMock superToken = new SuperTokenMock(host, 0);
        return address(superToken);
    }

}

contract SuperTokenFactory42Mock is SuperTokenFactoryBase
{

    constructor(
        ISuperfluid host
    )
        SuperTokenFactoryBase(host)
        // solhint-disable-next-line no-empty-blocks
    {
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        SuperTokenMock superToken = new SuperTokenMock(host, 42);
        return address(superToken);
    }

}
