// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { SuperTokenMock } from "./SuperTokenMock.sol";
import {
    ISuperfluid,
    ISuperToken,
    SuperToken,
    SuperTokenFactoryBase
} from "../superfluid/SuperTokenFactory.sol";

contract SuperTokenFactoryStorageLayoutTester is SuperTokenFactoryBase {
    constructor(
        ISuperfluid host,
        ISuperToken superTokenLogic
    )
        SuperTokenFactoryBase(host, superTokenLogic)
    // solhint-disable-next-line no-empty-blocks
    {

    }

    // @dev Make sure the storage layout never change over the course of the development
    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        // Initializable bool _initialized and bool _initialized

        assembly { slot:= _superTokenLogicDeprecated.slot offset := _superTokenLogicDeprecated.offset }
        require (slot == 0 && offset == 2, "_superTokenLogicDeprecated changed location");

        assembly { slot := _canonicalWrapperSuperTokens.slot offset := _canonicalWrapperSuperTokens.offset }
        require(slot == 1 && offset == 0, "_canonicalWrapperSuperTokens changed location");

        assembly { slot := _constantOutflowNFTLogic.slot offset := _constantOutflowNFTLogic.offset }
        require(slot == 2 && offset == 0, "_constantOutflowNFTLogic changed location");

        assembly { slot := _constantInflowNFTLogic.slot offset := _constantInflowNFTLogic.offset }
        require(slot == 3 && offset == 0, "_constantInflowNFTLogic changed location");

        assembly { slot := _poolAdminNFTLogic.slot offset := _poolAdminNFTLogic.offset }
        require(slot == 4 && offset == 0, "_poolAdminNFTLogic changed location");

        assembly { slot := _poolMemberNFTLogic.slot offset := _poolMemberNFTLogic.offset }
        require(slot == 5 && offset == 0, "_poolMemberNFTLogic changed location");
    }
}

contract SuperTokenFactoryUpdateLogicContractsTester is SuperTokenFactoryBase {
    uint256 public newVariable;

    constructor(
        ISuperfluid host,
        ISuperToken superTokenLogic
    )
        SuperTokenFactoryBase(host, superTokenLogic)
    // solhint-disable-next-line no-empty-blocks
    {

    }
}

contract SuperTokenFactoryMock is SuperTokenFactoryBase {
    constructor(
        ISuperfluid host,
        ISuperToken superTokenLogic
    )
        SuperTokenFactoryBase(host, superTokenLogic)
    // solhint-disable-next-line no-empty-blocks
    {

    }
}

contract SuperTokenFactoryMock42 is SuperTokenFactoryBase {
    constructor(
        ISuperfluid host,
        ISuperToken superTokenLogic
    )
        SuperTokenFactoryBase(host, superTokenLogic)
    // solhint-disable-next-line no-empty-blocks
    {

    }
}
