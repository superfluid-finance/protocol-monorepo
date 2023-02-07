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

    // dummy impl
    function createSuperTokenLogic(ISuperfluid)
        external pure override
        returns (address)
    {
        return address(0);
    }
    function createConstantOutflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
    }
    function createConstantInflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
    }
}

contract SuperTokenFactoryUpdateLogicContractsTester is SuperTokenFactoryBase {
    uint256 public newVariable;

    constructor(
        ISuperfluid host
    )
        SuperTokenFactoryBase(host)
        // solhint-disable-next-line no-empty-blocks
    {
    }
    event UpdateLogicContractsCalled();

    function updateLogicContracts() external override {
        newVariable = 69;
    }

    // dummy impl
    function createSuperTokenLogic(ISuperfluid)
        external pure override
        returns (address)
    {
        return address(0);
    }
    function createConstantOutflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
    }
    function createConstantInflowNFTLogic()
        external override
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
    function createConstantOutflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
    }
    function createConstantInflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
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
    function createConstantOutflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
    }
    function createConstantInflowNFTLogic()
        external override
        returns (address)
    {
        return address(0);
    }

}
