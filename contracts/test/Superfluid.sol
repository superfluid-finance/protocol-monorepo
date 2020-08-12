// SPDX-License-Identifier: MIT
/* solhint-disable*/
pragma solidity >=0.6.0;

import { Ownable } from "../interface/Ownable.sol";
import { ISuperfluid } from "../interface/ISuperfluid.sol";
import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";
import { ISuperApp } from "../interface/ISuperApp.sol";
import { AppHelper } from "../interface/AppHelper.sol";

contract Superfluid is Ownable, ISuperfluid {
    mapping(address => address[]) private _app;
    //mapping(address => mapping(address => bool) _allowedAppConnections;
    mapping(address => address[]) private _appModules;
    mapping(bytes32 => address) private _ctxStamps;
    mapping(address => uint256) private _appConfigs;

    function isJailed(address appAddr) external view override returns(bool) {
        return (_appConfigs[appAddr] & AppHelper.JAIL) != AppHelper.JAIL;
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

    function registerSuperApp(uint256 configWord) external override {
        _appConfigs[msg.sender] = configWord;
    }

    function getConfig(address superApp) external view override returns(uint256) {
        return _appConfigs[superApp];
    }

    function callWithContext(
        bytes calldata ctx,
        address callAddr,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory)
    {
        require(_ctxStamps[keccak256(abi.encodePacked(ctx))] != address(0), "SF: Invalid Context");
        delete _ctxStamps[keccak256(abi.encodePacked(ctx))];
        (
            uint8 level,
            address appAddr,
            uint64 gasReservation
        ) = _decode(ctx);

        bytes memory newCtx = _encode(level++, appAddr, gasReservation);
        _ctxStamps[keccak256(abi.encodePacked(newCtx))] = appAddr;
        _callCallback(callAddr, gasReservation, selector, data, newCtx);
        return newCtx;
    }

    function callBuildContext(
        address callAddr,
        uint64 gasReservation,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory)
    {
        //Build context data
        bytes memory ctx = _encode(0, msg.sender, gasReservation);
        bytes32 stamp = keccak256(abi.encodePacked(ctx));
        require(_ctxStamps[stamp] == address(0), "SF: Context exist");
        _ctxStamps[keccak256(abi.encodePacked(ctx))] = msg.sender;

        //Call app
        _callCallback(callAddr, gasReservation, selector, data, ctx);
        delete _ctxStamps[stamp];
        return ctx;
    }

    function _getAppLevel(address appAddr) internal returns(uint8) {
        if(_appConfigs[appAddr] | AppHelper.TYPE_APP_FINAL == AppHelper.TYPE_APP_FINAL) {
            return 1;
        }

        return 2;
    }


    function _callCallback(
        address callAddr,
        uint64 gasReservation,
        bytes4 selector,
        bytes memory data,
        bytes memory ctx
    )
        private
        returns(bool)
    {
        uint256 gasLeft = gasleft();
        uint256 gasBudget = gasLeft - gasReservation;
        (bool success, bytes memory returnedData) =
            callAddr.call{gas: gasBudget}(data);
        if (!success) {
            gasLeft = gasleft();
            if (gasLeft < gasReservation) {
                // this is out of gas, but the call may still fail if more gas is provied
                // and this is okay, because there can be incentive to jail the app by providing
                // more gas
                revert("SF: Send more gas");
            } else {
                _appConfigs[callAddr] |= AppHelper.JAIL;
                return false;
            }
        }

        return true;
    }

    function _encode(
        uint8 level,
        address appAddr,
        uint64 gasReservation
    )
        internal
        returns(bytes memory)
    {
        return abi.encode(level, appAddr, gasReservation);
    }

    function _decode(
        bytes memory ctx
    )
        internal
        returns(uint8, address, uint64)
    {
        return abi.decode(ctx, (uint8, address, uint64));
    }

}
