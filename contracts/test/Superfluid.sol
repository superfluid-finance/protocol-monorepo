// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;
pragma experimental ABIEncoderV2;

import { Ownable } from "../interface/Ownable.sol";
import { ISuperfluid } from "../interface/ISuperfluid.sol";
import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";
import { ISuperApp } from "../interface/ISuperApp.sol";
import { AppHelper } from "../interface/AppHelper.sol";
import { ContextLibrary } from "../interface/ContextLibrary.sol";

contract Superfluid is Ownable, ISuperfluid {
    event Jail(address app);

    mapping(address => address) private _composedApp;
    mapping(address => AppManifest) private _appConfigs;
    bytes32 private _ctxStamp;
    uint64 constant private _GAS_RESERVATION = 5000;

    /**
     * @notice Message sender declares it as a super app.
     * @param configWord The super app manifest configuration
     */
    function registerApp(
        uint256 configWord
    )
        external
        override
    {
        require(_appConfigs[msg.sender].configWord == 0 , "can't change app manifest");
        require(configWord > 0, "Invalid configuration");
        _appConfigs[msg.sender] = AppManifest(configWord);
    }

    function getAppManifest(
        address app
    )
    external
    view
    override
    returns (
        bool exist,
        uint256 configWord
    )
    {
        AppManifest memory manifest = _appConfigs[app];
        return ((manifest.configWord > 0), manifest.configWord);
    }

    function isAppJailed(
        address app
    )
        public
        view
        override
        returns(bool)
    {
        return (_appConfigs[app].configWord & AppHelper.JAIL) != AppHelper.JAIL;
    }

    /**
     * @notice White-list the target app for app composition for the source app `msg.sender`
     * @param targetApp The taget super app address
     */
    function allowCompositeApp(
        address targetApp
    )
        external
        override
    {
        require(_composedApp[msg.sender] == address(0), "Can't change target composite App");
        _composedApp[msg.sender] = targetApp;
    }

    function isCompositeAppAllowed(
        address app,
        address targetApp
    )
        external
        view
        override
        returns (bool)
    {
        return _composedApp[app] == targetApp;
    }

    //Split the callback in the two functions so they can have different rules and returns data formats
    function callAppBefore(
        bytes calldata ctx,
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory newCtx, bytes memory cbdata)
    {
        require(!_isSuperApp(app), "SF: target is not superApp");
        bytes32 stamp = ContextLibrary.createStamp(ctx);
        if(!ContextLibrary.validateContext(ctx, stamp)) {
            //JAIL
            emit Jail(app);
        }
        _ctxStamp = stamp;

        //TODO: _callCallback
        (bool success, bytes memory returnedData) = _callCallback(ctx, app, selector, data);
        _ctxStamp = 0;
        if(success) {
            (newCtx, cbdata) = ContextLibrary.splitReturnedData(returnedData);
            if(!ContextLibrary.validateContext(newCtx, _ctxStamp)) {
                //JAIL
                //Change return context
                emit Jail(app);
            }
        }

        return ("", returnedData);
    }

    function callAppAfter(
        bytes calldata ctx,
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory newCtx)
    {
        require(!_isSuperApp(app), "SF: target is not superApp");
        if(isAppJailed(app) || !_uniqueContext()) {
            _appConfigs[msg.sender].configWord |= AppHelper.JAIL;
        }

        _ctxStamp = ContextLibrary.createStamp(ctx);
        (bool success, bytes memory returnedData) = _callCallback(ctx, app, selector, data);
        _ctxStamp = 0;
        if(success) {
            (newCtx, ) = ContextLibrary.splitReturnedData(returnedData);
            if(!ContextLibrary.validateContext(newCtx, _ctxStamp)) {
                //JAIL
                //Change return context
                emit Jail(app);
            }
        }
        return returnedData;
    }

    function callAppAction(
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory returnedData)
    {
        require(!_isSuperApp(app), "SF: target is not superApp");
        if(isAppJailed(app) || !_uniqueContext()) {
            _appConfigs[msg.sender].configWord |= AppHelper.JAIL;
        }

        //Build context data
        //TODO: Where we get the gas reservation?

        (bytes memory ctx, bytes32 stamp) = ContextLibrary.encodeContext(0, msg.sender, _GAS_RESERVATION);
        _ctxStamp = stamp;
        (, returnedData) = _callCallback(ctx, app, selector, data);
        _ctxStamp = 0;
    }

    function callAgreement(
        bytes calldata ctx,
        address agreementClass,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory newCtx, bytes memory cbdata)
    {
        require(_isValidContext(ctx), "SF: Context Invalid");

        //Call app
        (bool sucess, bytes memory returnedData) = _callExternal(ctx, agreementClass, selector, data);
        if(sucess) {
            return ContextLibrary.splitReturnedData(returnedData);
        }
        revert("Returned data not encoded correctly");
    }

    function callAgreement(
        address agreementClass,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory newCtx)
    {
        //TODO: sender has to be App? If not we can jail it
        if(!_uniqueContext()) {
            _appConfigs[msg.sender].configWord |= AppHelper.JAIL;
        }
        //Build context data
        (bytes memory ctx, bytes32 stamp) = ContextLibrary.encodeContext(0, msg.sender, _GAS_RESERVATION);
        _ctxStamp = stamp;
        _callExternal(ctx, agreementClass, selector, data);
        return ctx;
    }

    function callAppAction(
        bytes calldata ctx,
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        override
        returns(bytes memory newCtx)
    {
        require(_isValidContext(ctx), "SF: Context Invalid");
        (
            uint8 level,
            address appAddr,
            uint64 gasReservation
        ) = ContextLibrary.decodeContext(ctx);

        level++;
        require(_checkAppCallStact(msg.sender, level), "SF: App Call Stack too deep");
        (newCtx, _ctxStamp) = ContextLibrary.encodeContext(level, appAddr, gasReservation);
        //_ctxStamp = stamp;
        _callCallback(newCtx, app, selector, data);
        level--;
        (newCtx, ) = ContextLibrary.encodeContext(level, appAddr, _GAS_RESERVATION);
    }


    /* solhint-disable-next-line */
    function chargeGasFee(uint fee) external override {
        ///TODO
    }

    /* Basic Law Rules */

    function _isCallerAgreement() internal view returns(bool) {
        return true;
    }

    function _uniqueContext() internal view returns(bool) {
        return _ctxStamp.length == 0;
    }

    function _isValidContext(bytes memory ctx) internal view returns(bool) {
        return keccak256(abi.encodePacked(ctx)) == _ctxStamp;
    }

    function _isSuperApp(address app) internal view returns(bool) {
        return _appConfigs[app].configWord > 0;
    }

    function _checkAppCallStact(address appAddr, uint8 currentAppLevel) internal returns(bool) {
        uint8 appLevel = _getAppLevel(appAddr);
        if(appLevel == 1 && currentAppLevel > 1) {
            return false;
        }
        if(appLevel == 2 && currentAppLevel > 2) {
            return false;
        }
        return true;
    }

    function _getAppLevel(address appAddr) internal returns(uint8) {
        if(_appConfigs[appAddr].configWord | AppHelper.TYPE_APP_FINAL == AppHelper.TYPE_APP_FINAL) {
            return 1;
        }
        return 2;
    }

    function _callExternal(
        bytes memory ctx,
        address app,
        bytes4 selector,
        bytes memory data
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        /* solhint-disable-next-line avoid-low-level-calls*/
        return app.call(abi.encodeWithSelector(selector, ctx, data));
    }

    function _callCallback(
        bytes memory ctx,
        address app,
        bytes4 selector,
        bytes memory data
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        uint256 gasBudget = gasleft() - _GAS_RESERVATION;
        (success, returnedData) =
        /* solhint-disable-next-line avoid-low-level-calls*/
            app.call{gas: gasBudget}(abi.encodeWithSelector(selector, ctx, data));
             if (!success) {
                 if (gasleft() < _GAS_RESERVATION) {
                     // this is out of gas, but the call may still fail if more gas is provied
                     // and this is okay, because there can be incentive to jail the app by providing
                     // more gas
                     revert("SF: Send more gas");
                 } else {
                     _appConfigs[app].configWord |= AppHelper.JAIL;
                 }
             }
    }
}
