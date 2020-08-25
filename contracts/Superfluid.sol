// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;
pragma experimental ABIEncoderV2;

import { Ownable } from "./interface/Ownable.sol";
import { ISuperfluid } from "./interface/ISuperfluid.sol";
import { ISuperfluidGovernance } from "./interface/ISuperfluidGovernance.sol";
import { ISuperApp } from "./interface/ISuperApp.sol";
import { AppHelper } from "./interface/AppHelper.sol";
import { ContextLibrary } from "./interface/ContextLibrary.sol";

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
    //TODO : msg.sender should be only SuperAgreement
    function callAppBefore(
        address app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        require(_isSuperApp(app), "SF: target is not superApp");

        //TODO: _callCallback
        (bool success, bytes memory returnedData) = _callCallback(app, data);
        cbdata = abi.decode(returnedData, (bytes));
        if(success) {
            //(newCtx, cbdata) = ContextLibrary.splitReturnedData(returnedData);
            //TODO Change counter gas measurement
            newCtx = ctx;
            /*
            if(!ContextLibrary.validateContext(newCtx, _ctxStamp)) {
                //JAIL
                //Change return context
                emit Jail(app);
            }
            */
        }
    }

    function callAppAfter(
        address app,
        bytes calldata data,
        bytes calldata /*ctx*/
    )
        external
        override
        returns(bytes memory newCtx)
    {
        require(_isSuperApp(app), "SF: target is not superApp");
        if(isAppJailed(app) || !_uniqueContext()) {
            _appConfigs[msg.sender].configWord |= AppHelper.JAIL;
        }

        (bool success, bytes memory returnedData) = _callCallback(app, data);
        newCtx = abi.decode(returnedData, (bytes));
        if(success) {
            if(!ContextLibrary.validate(newCtx, _ctxStamp)) { //JAIL
                //Change return context
                emit Jail(app);
            }
        } else {
            revert("SF: Insuccessful external call");
        }
       return "";
        //return returnedData;
    }

    function callAppAction(
        address app,
        bytes calldata data
    )
        external
        override
        returns(bytes memory returnedData)
    {
        require(_isSuperApp(app), "SF: target is not superApp");
        if(isAppJailed(app) || !_uniqueContext()) {
            _appConfigs[msg.sender].configWord |= AppHelper.JAIL;
        }

        //Build context data
        //TODO: Where we get the gas reservation?
        bool success;

        bytes memory ctx;
        (ctx, _ctxStamp) = ContextLibrary.encode(ContextLibrary.Context(0, msg.sender, _GAS_RESERVATION));
        (success, returnedData) = _callExternal(app, data, ctx);
        if(!success) {
            revert(string(returnedData));
        }
        _ctxStamp = 0;
    }

    function callAgreementWithContext(
        address agreementClass,
        bytes calldata data,
        bytes calldata ctx
    ) external
        override
        returns(bytes memory newCtx, bytes memory returnedData)
    {
        require(ContextLibrary.validate(ctx, _ctxStamp), "SF: Agreement Context Invalid");
        ContextLibrary.Context memory stcCtx = ContextLibrary.decode(ctx);
        address oldSender = stcCtx.msgSender;
        stcCtx.msgSender = msg.sender;
        (newCtx, _ctxStamp) = ContextLibrary.encode(stcCtx);

        //Call app
        bool success;
        (success, returnedData) = _callExternal(agreementClass, data, newCtx);
        if(success) {
            // TODO update context
            stcCtx.msgSender = oldSender;
            (newCtx, _ctxStamp) = ContextLibrary.encode(stcCtx);
        } else {
            revert("SF: call agreement failed");
        }
    }

    function callAgreement(
        address agreementClass,
        bytes calldata data
    )
        external
        override
        returns(bytes memory newCtx, bytes memory returnedData)
    {
        //TODO: sender has to be App? If not we can jail it
        /*
        if(!_uniqueContext()) {
            _appConfigs[msg.sender].configWord |= AppHelper.JAIL;
        }
        */
        //Build context data
        require(_ctxStamp == 0, "Stamp is not clean");
        bytes memory ctx;
        (ctx, _ctxStamp) = ContextLibrary.encode(ContextLibrary.Context(0, msg.sender, _GAS_RESERVATION));
        bool success;
        (success, returnedData) = _callExternal(agreementClass, data, ctx);
        if (success) {
            _ctxStamp = 0;
        } else {
            revert("SF: call agreement failed");
        }
    }

    function callAppActionWithContext(
        address app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        require(ContextLibrary.validate(ctx, _ctxStamp), "SF: Action Context Invalid");
        ContextLibrary.Context memory stcCtx = ContextLibrary.decode(ctx);

        stcCtx.level++;
        require(_checkAppCallStact(msg.sender, stcCtx.level), "SF: App Call Stack too deep");
        (newCtx, _ctxStamp) = ContextLibrary.encode(stcCtx);
        _callExternal(app, data, newCtx);
        stcCtx.level--;
        (newCtx, _ctxStamp) = ContextLibrary.encode(stcCtx);
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

    /*
    function _isValidContext(bytes memory ctx) internal view returns(bool) {
        return keccak256(abi.encodePacked(ctx)) == _ctxStamp;
    }
    */

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
        address app,
        bytes memory data,
        bytes memory ctx
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        /* solhint-disable-next-line avoid-low-level-calls */
        (success, returnedData) = app.call(
            ContextLibrary.replaceContext(data, ctx)
        );

        if(!success) {
            revert(string(returnedData));
        }
    }

    function _callCallback(
        address app,
        bytes memory data
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        //uint256 gasBudget = gasleft() - _GAS_RESERVATION;
        /* solhint-disable-next-line avoid-low-level-calls*/
        (success, returnedData) = app.call(data);
         if (!success) {
             if (gasleft() < _GAS_RESERVATION) {
                 // this is out of gas, but the call may still fail if more gas is provied
                 // and this is okay, because there can be incentive to jail the app by providing
                 // more gas
                 revert("SF: Send more gas");
             } else {
                revert(string(returnedData));
                 //_appConfigs[app].configWord |= AppHelper.JAIL;
             }
         }
    }
}
