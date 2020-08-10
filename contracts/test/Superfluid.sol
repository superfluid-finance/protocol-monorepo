// SPDX-License-Identifier: MIT
/* solhint-disable*/
pragma solidity >=0.6.0;

import { Ownable } from "../interface/Ownable.sol";
import { ISuperfluid } from "../interface/ISuperfluid.sol";
import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";

contract Superfluid is Ownable, ISuperfluid {

    mapping(address => bool) private _jail;
    mapping(address => address[]) private _app;
    //mapping(address => mapping(address => bool) _allowedAppConnections;
    mapping(address => address[]) private _appModules;

    function isJailed(address appAddr) external view override returns(bool) {
        return _jail[appAddr];
    }

    function setAppConnection(address appModule) external override {
        _appModules[msg.sender].push(appModule);
    }

    function setWhiteList(address module) external override {
        _app[msg.sender].push(module);
    }

    function isWhiteListed(address sender, address appAddr) external view override returns(bool) {
        for(uint256 i = 0; i < _app[sender].length; i++) {
            if(_app[sender][i] == appAddr) {
                return true;
            }
        }

        return false;
    }

    function callWithContext(
        address appAddr,
        uint64 gasReservation,
        bytes4 selector,
        bytes32 id

    )
        external
        override
        returns(bool)
    {
        //Build context data
        bytes memory ctx = "";

        //Call app
        _callCallback(appAddr, msg.sender, gasReservation, selector, id, ctx);
    }

    function _callCallback(
        address appAddr,
        address agreementClass,
        uint64 gasReservation,
        bytes4 selector,
        bytes32 id,
        bytes memory ctx
    )
        private
        returns(bool)
    {
        uint256 gasLeft = gasleft();
        uint256 gasBudget = gasLeft - gasReservation;
        bytes memory data = abi.encodeWithSelector(selector, agreementClass, id, ctx);
        (bool success, bytes memory returnedData) =
            appAddr.call{gas: gasBudget}(data);
        if (!success) {
            gasLeft = gasleft();
            if (gasLeft < gasReservation) {
                // this is out of gas, but the call may still fail if more gas is provied
                // and this is okay, because there can be incentive to jail the app by providing
                // more gas
            } else {
                // go to jail
                _jail[appAddr] = true;
                return false;
            }
        }

        return true;
    }
}
