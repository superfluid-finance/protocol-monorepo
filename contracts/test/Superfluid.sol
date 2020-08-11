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
    mapping(bytes32 => address) private _stamps;

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

    function callWithContext(bytes calldata ctx) external override returns(bool) {
        bytes32 stamp = keccak256(abi.encodePacked(ctx));
        require(_stamps[stamp] != address(0), "SF: Context don't exist");
        (
            address appAddr,
            address agreementClass,
            uint64 gasReservation,
            bytes4 selector,
            bytes32 id
        ) = _decode(ctx);
        _callCallback(appAddr, agreementClass, gasReservation, selector, id, ctx);
    }

    function callBuildContext(
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
        bytes memory ctx = _encode(appAddr, msg.sender, gasReservation, selector, id);
        bytes32 stamp = keccak256(abi.encodePacked(ctx));
        require(_stamps[stamp] == address(0), "SF: Context exist");
        _stamps[keccak256(abi.encodePacked(ctx))] = msg.sender;

        //Call app
        _callCallback(appAddr, msg.sender, gasReservation, selector, id, ctx);
        delete _stamps[stamp];
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

    function _encode(
        address appAddr,
        address agreementClass,
        uint64 gasReservation,
        bytes4 selector,
        bytes32 id
    )
        internal
        returns(bytes memory)
    {
        return abi.encode(appAddr, agreementClass, gasReservation, selector, id);
    }

    function _decode(
        bytes memory ctx
    )
        internal
        returns(address, address, uint64, bytes4, bytes32)
    {
        return abi.decode(ctx, (address, address, uint64, bytes4, bytes32));
    }

}
