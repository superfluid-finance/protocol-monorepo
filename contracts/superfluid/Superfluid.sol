// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;
pragma experimental ABIEncoderV2;

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperAgreement,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions,
    ISuperfluidToken,
    ISuperToken,
    ISuperTokenFactory,
    IERC20
} from "../interfaces/superfluid/ISuperfluid.sol";

import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/SafeCast.sol";


contract Superfluid is
    UUPSProxiable,
    ISuperfluid
{

    using SafeMath for uint256;
    using SafeCast for uint256;
    using SignedSafeMath for int256;

    //
    // the context that only needed for the next callback level
    //
    struct ExtCallContext {
        // callback level
        uint8 cbLevel;
        // type of call
        uint8 callType;
        // the system timestsamp
        uint256 timestamp;
        // The intended message sender for the call
        address msgSender;
        // For callbacks it is used to know which agreement function selector is called
        bytes4 agreementSelector;
        // User provided data for app callbacks
        bytes userData;
    }

    //
    // the context that needed by the app
    //
    struct AppContext {
        // app allowance granted
        uint256 allowanceGranted;
        // app allowance wanted by the app callback
        uint256 allowanceWanted;
        // app allowance used, allowing negative values over a callback session
        int256 allowanceUsed;
    }

    struct FullContext {
        ExtCallContext extCall;
        AppContext app;
    }

    struct AppManifest {
        uint256 configWord;
    }

    // solhint-disable-next-line var-name-mixedcase
    bool immutable private _NON_UPGRADABLE_DEPLOYMENT;

    // solhint-disable-next-line var-name-mixedcase
    uint64 immutable private _GAS_RESERVATION = 5000;

    /* WARNING: NEVER RE-ORDER VARIABLES! Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev Governance contract
    ISuperfluidGovernance internal _gov;

    /// @dev Agreement list indexed by agreement index minus one
    ISuperAgreement[] internal _agreementClasses;
    /// @dev Mapping between agreement type to agreement index (starting from 1)
    mapping (bytes32 => uint) internal _agreementClassIndices;

    /// @dev Super token
    ISuperTokenFactory internal _superTokenFactory;

    /// @dev App manifests
    mapping(ISuperApp => AppManifest) internal _appManifests;
    /// @dev Composite app white-listing: source app => (target app => isAllowed)
    mapping(ISuperApp => mapping(ISuperApp => bool)) internal _compositeApps;
    /// @dev Ctx stamp of the current transaction, it should always be cleared to
    ///      zero before transaction finishes
    bytes32 internal _ctxStamp;

    constructor(bool nonUpgradable) {
        _NON_UPGRADABLE_DEPLOYMENT = nonUpgradable;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // UUPSProxiable
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    function initialize(
        ISuperfluidGovernance gov
    )
        external
        initializer // OpenZeppelin Initializable
    {
        _gov = gov;
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.Superfluid.implementation");
    }

    function updateCode(address newAddress) external override onlyGovernance {
        return _updateCodeAddress(newAddress);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Governance
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    function getGovernance() external view override returns (ISuperfluidGovernance) {
        return _gov;
    }

    function replaceGovernance(ISuperfluidGovernance newGov) external override onlyGovernance {
        _gov = newGov;
    }

    /**************************************************************************
     * Agreement Whitelisting
     *************************************************************************/

    function registerAgreementClass(ISuperAgreement agreementClassLogic) external onlyGovernance override {
        bytes32 agreementType = agreementClassLogic.agreementType();
        require(_agreementClassIndices[agreementType] == 0,
            "SF: agreement class already registered");
        require(_agreementClasses.length < 256,
            "SF: support up to 256 agreement classes");
        ISuperAgreement agreementClass;
        if (!_NON_UPGRADABLE_DEPLOYMENT) {
            // initialize the proxy
            UUPSProxy proxy = new UUPSProxy();
            proxy.initializeProxy(address(agreementClassLogic));
            agreementClass = ISuperAgreement(address(proxy));
        } else {
            agreementClass = ISuperAgreement(address(agreementClassLogic));
        }
        agreementClass.initialize();
        // register the agreement proxy
        _agreementClasses.push((agreementClass));
        _agreementClassIndices[agreementType] = _agreementClasses.length;
    }

    function updateAgreementClass(ISuperAgreement agreementClassLogic) external onlyGovernance override {
        require(!_NON_UPGRADABLE_DEPLOYMENT, "SF: non upgradable");
        bytes32 agreementType = agreementClassLogic.agreementType();
        uint idx = _agreementClassIndices[agreementType];
        require(idx != 0, "SF: agreement class not registered");
        UUPSProxiable proxiable = UUPSProxiable(address(_agreementClasses[idx - 1]));
        proxiable.updateCode(address(agreementClassLogic));
    }

    function isAgreementTypeListed(bytes32 agreementType)
        external view override
        returns (bool yes)
    {
        uint idx = _agreementClassIndices[agreementType];
        return idx != 0;
    }

    function isAgreementClassListed(ISuperAgreement agreementClass)
        public view override
        returns (bool yes)
    {
        bytes32 agreementType = agreementClass.agreementType();
        uint idx = _agreementClassIndices[agreementType];
        // it should also be the same agreement class proxy address
        return idx != 0 && _agreementClasses[idx - 1] == agreementClass;
    }

    function getAgreementClass(bytes32 agreementType)
        external view override
        returns(ISuperAgreement agreementClass)
    {
        uint idx = _agreementClassIndices[agreementType];
        require(idx != 0, "SF: agreement class not registered");
        return ISuperAgreement(_agreementClasses[idx - 1]);
    }

    function mapAgreementClasses(uint256 bitmap)
        external view override
        returns (ISuperAgreement[] memory agreementClasses) {
        uint i;
        uint n;
        // create memory output using the counted size
        agreementClasses = new ISuperAgreement[](_agreementClasses.length);
        // add to the output
        n = 0;
        for (i = 0; i < _agreementClasses.length; ++i) {
            if ((bitmap & (1 << i)) > 0) {
                agreementClasses[n++] = _agreementClasses[i];
            }
        }
        // resize memory arrays
        assembly { mstore(agreementClasses, n) }
    }

    function addToAgreementClassesBitmap(uint256 bitmap, bytes32 agreementType)
        external view override
        returns (uint256 newBitmap)
    {
        uint idx = _agreementClassIndices[agreementType];
        require(idx != 0, "SF: agreement class not registered");
        return bitmap | (1 << (idx - 1));
    }

    function removeFromAgreementClassesBitmap(uint256 bitmap, bytes32 agreementType)
        external view override
        returns (uint256 newBitmap)
    {
        uint idx = _agreementClassIndices[agreementType];
        require(idx != 0, "SF: agreement class not registered");
        return bitmap & ~(1 << (idx - 1));
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Super Token Factory
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    function getSuperTokenFactory()
        external view override
        returns (ISuperTokenFactory factory)
    {
        return _superTokenFactory;
    }

    function getSuperTokenFactoryLogic()
        external view override
        returns (address logic)
    {
        if (address(_superTokenFactory) == address(0)) return address(0);
        if (_NON_UPGRADABLE_DEPLOYMENT) return address(_superTokenFactory);
        else return UUPSProxiable(address(_superTokenFactory)).getCodeAddress();
    }

    function updateSuperTokenFactory(ISuperTokenFactory newFactory)
        external override
        onlyGovernance
    {
        if (address(_superTokenFactory) == address(0)) {
            if (!_NON_UPGRADABLE_DEPLOYMENT) {
                // initialize the proxy
                UUPSProxy proxy = new UUPSProxy();
                proxy.initializeProxy(address(newFactory));
                _superTokenFactory = ISuperTokenFactory(address(proxy));
            } else {
                _superTokenFactory = newFactory;
            }
            _superTokenFactory.initialize();
        } else {
            require(!_NON_UPGRADABLE_DEPLOYMENT, "SF: non upgradable");
            UUPSProxiable(address(_superTokenFactory)).updateCode(address(newFactory));
        }
    }

    function updateSuperTokenLogic(ISuperToken token)
        external override
        onlyGovernance
    {
        // assuming it's uups proxiable
        UUPSProxiable(address(token))
            .updateCode(address(_superTokenFactory.getSuperTokenLogic()));
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // App Registry
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    function registerApp(
        uint256 configWord
    )
        external override
    {
        ISuperApp app = ISuperApp(msg.sender);
        {
            uint256 cs;
            // solhint-disable-next-line no-inline-assembly
            assembly { cs := extcodesize(app) }
            require(cs == 0, "SF: app registration only in constructor");
        }
        require(
            SuperAppDefinitions.getAppLevel(configWord) > 0 &&
            (configWord & SuperAppDefinitions.APP_JAIL_BIT) == 0,
            "SF: invalid config word");
        require(_appManifests[ISuperApp(msg.sender)].configWord == 0 , "SF: app already registered");
        _appManifests[ISuperApp(msg.sender)] = AppManifest(configWord);
    }

    function isApp(ISuperApp app) public view override returns(bool) {
        return _appManifests[app].configWord > 0;
    }

    function getAppLevel(ISuperApp appAddr) public override view returns(uint8) {
        return SuperAppDefinitions.getAppLevel(_appManifests[appAddr].configWord);
    }

    function getAppManifest(
        ISuperApp app
    )
        external view override
        returns (
            bool isSuperApp,
            bool isJailed,
            uint256 noopMask
        )
    {
        AppManifest memory manifest = _appManifests[app];
        isSuperApp = (manifest.configWord > 0);
        if (isSuperApp) {
            isJailed = SuperAppDefinitions.isAppJailed(manifest.configWord);
            noopMask = manifest.configWord & SuperAppDefinitions.AGREEMENT_CALLBACK_NOOP_BITMASKS;
        }
    }

    function isAppJailed(
        ISuperApp app
    )
        public view override
        returns(bool)
    {
        return SuperAppDefinitions.isAppJailed(_appManifests[app].configWord);
    }

    function allowCompositeApp(
        ISuperApp targetApp
    )
        external override
    {
        require(isApp(ISuperApp(msg.sender)), "SF: sender is not an app");
        require(isApp(targetApp), "SF: target is not an app");
        _compositeApps[ISuperApp(msg.sender)][targetApp] = true;
    }

    function isCompositeAppAllowed(
        ISuperApp app,
        ISuperApp targetApp
    )
        external view override
        returns (bool)
    {
        return _compositeApps[app][targetApp];
    }

    /**************************************************************************
     * Agreement Framework
     *************************************************************************/

    function callAppBeforeCallback(
        ISuperApp app,
        bytes calldata callData,
        bool isTermination,
        bytes calldata ctx
    )
        external override
        onlyAgreement
        isAppActive(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory cbdata)
    {
        (bool success, bytes memory returnedData) = _callCallback(app, true, callData, ctx);
        if (success) {
            cbdata = abi.decode(returnedData, (bytes));
        } else {
            if (!isTermination) {
               revert(_getRevertMsg(returnedData));
            } else {
                emit Jail(app, SuperAppDefinitions.APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK);
            }
        }
    }

    function callAppAfterCallback(
        ISuperApp app,
        bytes calldata callData,
        bool isTermination,
        bytes calldata ctx
    )
        external override
        onlyAgreement
        isAppActive(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory newCtx)
    {
        (bool success, bytes memory returnedData) = _callCallback(app, false, callData, ctx);
        if (success) {
            newCtx = abi.decode(returnedData, (bytes));
            if(!_isCtxValid(newCtx)) {
                // TODO: APP_JAIL_BIT if callback changes ctx and Change return context
                if (!isTermination) {
                    revert("SF: APP_RULE_CTX_IS_READONLY");
                } else {
                    emit Jail(app, SuperAppDefinitions.APP_RULE_CTX_IS_READONLY);
                }
            }
        } else {
            if (!isTermination) {
                revert(_getRevertMsg(returnedData));
            } else {
                emit Jail(app, SuperAppDefinitions.APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK);
            }
        }
    }

    function appCallbackPush(
        bytes calldata ctx,
        uint256 allowanceGranted,
        int256 allowanceUsed
    )
        external override
        onlyAgreement
        returns (bytes memory appCtx)
    {
        FullContext memory appContext = _decodeFullContext(ctx);
        appContext.extCall.cbLevel++;
        appContext.extCall.callType = ContextDefinitions.CALL_INFO_CALL_TYPE_APP_CALLBACK;
        appContext.app.allowanceGranted = allowanceGranted;
        appContext.app.allowanceWanted = 0;
        appContext.app.allowanceUsed = allowanceUsed;
        appCtx = _updateContext(appContext);
    }

    function appCallbackPop(
        bytes calldata ctx,
        int256 allowanceUsedDelta
    )
        external override
        onlyAgreement
        returns (bytes memory newCtx)
    {
        FullContext memory context = _decodeFullContext(ctx);
        context.app.allowanceUsed = context.app.allowanceUsed.add(allowanceUsedDelta);
        newCtx = _updateContext(context);
    }

    function ctxUseAllowance(
        bytes calldata ctx,
        uint256 allowanceWantedMore,
        int256 allowanceUsedDelta
    )
        external override
        onlyAgreement
        returns (bytes memory newCtx)
    {
        FullContext memory context = _decodeFullContext(ctx);

        context.app.allowanceWanted = context.app.allowanceWanted.add(allowanceWantedMore);
        context.app.allowanceUsed = context.app.allowanceUsed.add(allowanceUsedDelta);

        newCtx = _updateContext(context);
    }

    /**************************************************************************
    * Contextless Call Proxies
    *************************************************************************/

    function callAgreement(
        ISuperAgreement agreementClass,
        bytes memory callData,
        bytes calldata userData
    )
        public override
        cleanCtx
        isAgreement(agreementClass)
        returns(bytes memory returnedData)
    {
        //Build context data
        bytes memory ctx;
        // beaware of the endiness
        bytes4 agreementSelector = _functionPrefix(callData);
        ctx = _updateContext(FullContext({
            extCall: ExtCallContext({
                cbLevel: 0,
                callType: ContextDefinitions.CALL_INFO_CALL_TYPE_AGREEMENT,
                /* solhint-disable-next-line not-rely-on-time */
                timestamp: block.timestamp,
                msgSender: msg.sender,
                agreementSelector: agreementSelector,
                userData: userData
            }),
            app: AppContext({
                allowanceGranted: 0,
                allowanceWanted: 0,
                allowanceUsed: 0
            })
        }));
        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(agreementClass), callData, ctx);
        if (!success) {
            revert(_getRevertMsg(returnedData));
        }
        // clear the stamp
        _ctxStamp = 0;
    }

    function callAppAction(
        ISuperApp app,
        bytes memory callData
    )
        public override
        cleanCtx
        isAppActive(app)
        returns(bytes memory returnedData)
    {
        //Build context data
        //TODO: Where we get the gas reservation?
        bool success;

        bytes memory ctx;
        ctx = _updateContext(FullContext({
            extCall: ExtCallContext({
                cbLevel: 0,
                callType: ContextDefinitions.CALL_INFO_CALL_TYPE_APP_ACTION,
                /* solhint-disable-next-line not-rely-on-time */
                timestamp: block.timestamp,
                msgSender: msg.sender,
                agreementSelector: 0,
                userData: ""
            }),
            app: AppContext({
                allowanceGranted: 0,
                allowanceWanted: 0,
                allowanceUsed: 0
            })
        }));
        (success, returnedData) = _callExternalWithReplacedCtx(address(app), callData, ctx);
        if (success) {
            ctx = abi.decode(returnedData, (bytes));
            require(_isCtxValid(ctx), "SF: APP_RULE_CTX_IS_READONLY");
        } else {
            revert(_getRevertMsg(returnedData));
        }
        // clear the stamp
        _ctxStamp = 0;
    }

    /**************************************************************************
     * Contextual Call Proxies
     *************************************************************************/

    function callAgreementWithContext(
        ISuperAgreement agreementClass,
        bytes calldata callData,
        bytes calldata userData,
        bytes calldata ctx
    )
        external override
        validCtx(ctx)
        isAgreement(agreementClass)
        returns (bytes memory newCtx, bytes memory returnedData)
    {
        FullContext memory context = _decodeFullContext(ctx);
        address oldSender = context.extCall.msgSender;

        context.extCall.msgSender = msg.sender;
        //context.extCall.agreementSelector =;
        context.extCall.userData = userData;
        newCtx = _updateContext(context);

        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(agreementClass), callData, newCtx);
        if (success) {
            (newCtx) = abi.decode(returnedData, (bytes));
            assert(_isCtxValid(newCtx));
            // back to old msg.sender
            context = _decodeFullContext(newCtx);
            context.extCall.msgSender = oldSender;
            newCtx = _updateContext(context);
        } else {
            revert(_getRevertMsg(returnedData));
        }
    }

    function callAppActionWithContext(
        ISuperApp app,
        bytes calldata callData,
        bytes calldata ctx
    )
        external override
        validCtx(ctx)
        isAppActive(app)
        returns(bytes memory newCtx)
    {
        FullContext memory context = _decodeFullContext(ctx);
        address oldSender = context.extCall.msgSender;

        // FIXME max app level check
        context.extCall.msgSender = msg.sender;
        newCtx = _updateContext(context);

        (bool success, bytes memory returnedData) = _callExternalWithReplacedCtx(address(app), callData, newCtx);
        if (success) {
            (newCtx) = abi.decode(returnedData, (bytes));
            require(_isCtxValid(newCtx), "SF: APP_RULE_CTX_IS_READONLY");
            // back to old msg.sender
            context = _decodeFullContext(newCtx);
            context.extCall.msgSender = oldSender;
            newCtx = _updateContext(context);
        } else {
            revert(_getRevertMsg(returnedData));
        }
    }

    function decodeCtx(bytes calldata ctx)
        external pure override
        returns (
            uint256 callInfo,
            uint256 timestamp,
            address msgSender,
            bytes4 agreementSelector,
            bytes memory userData,
            uint256 appAllowanceGranted,
            uint256 appAllowanceWanted,
            int256 appAllowanceUsed
        )
    {
        FullContext memory context = _decodeFullContext(ctx);
        callInfo = ContextDefinitions.encodeCallInfo(context.extCall.cbLevel, context.extCall.callType);
        timestamp = context.extCall.timestamp;
        msgSender = context.extCall.msgSender;
        agreementSelector = context.extCall.agreementSelector;
        userData = context.extCall.userData;
        appAllowanceGranted = context.app.allowanceGranted;
        appAllowanceWanted = context.app.allowanceWanted;
        appAllowanceUsed = context.app.allowanceUsed;
    }

    function isCtxValid(bytes calldata ctx)
        external view override
        returns (bool)
    {
        return _isCtxValid(ctx);
    }

    /**************************************************************************
    * Batch call
    **************************************************************************/

    function batchCall(
       Operation[] memory operations
    )
       external override
    {
        for(uint256 i = 0; i < operations.length; i++) {
            OperationType opType = operations[i].opType;
            /*  */ if (opType == OperationType.Approve) {
                (address spender, uint256 amount) =
                    abi.decode(operations[i].data, (address, uint256));
                ISuperToken(operations[i].target).operationApprove(
                    msg.sender,
                    spender,
                    amount);
            } else if (opType == OperationType.TransferFrom) {
                (address sender, address receiver, uint256 amount) =
                    abi.decode(operations[i].data, (address, address, uint256));
                ISuperToken(operations[i].target).operationTransferFrom(
                    msg.sender,
                    sender,
                    receiver,
                    amount);
            } else if (opType == OperationType.Upgrade) {
                ISuperToken(operations[i].target).operationUpgrade(
                    msg.sender,
                    abi.decode(operations[i].data, (uint256)));
            } else if (opType == OperationType.Downgrade) {
                ISuperToken(operations[i].target).operationDowngrade(
                    msg.sender,
                    abi.decode(operations[i].data, (uint256)));
            } else if (opType == OperationType.CallAgreement) {
                (bytes memory callData, bytes memory userData) = abi.decode(operations[i].data, (bytes, bytes));
                this.callAgreement(
                    ISuperAgreement(operations[i].target),
                    callData,
                    userData);
            } else if (opType == OperationType.CallApp) {
                this.callAppAction(
                    ISuperApp(operations[i].target),
                    operations[i].data);
            } else {
               revert("SF: unknown operation type");
            }
        }
    }

    /**************************************************************************
     * Internal
     *************************************************************************/

    function _decodeFullContext(bytes memory ctx)
        private pure
        returns (FullContext memory context)
    {
        uint256 callInfo;
        uint256 allowanceIO;
        (
            callInfo,
            context.extCall.timestamp,
            context.extCall.msgSender,
            context.extCall.agreementSelector,
            context.extCall.userData,
            allowanceIO,
            context.app.allowanceUsed
        ) = abi.decode(ctx, (uint256, uint256, address, bytes4, bytes, uint256, int256));
        (context.extCall.cbLevel, context.extCall.callType) = ContextDefinitions.decodeCallInfo(callInfo);
        context.app.allowanceGranted = allowanceIO & type(uint128).max;
        context.app.allowanceWanted = allowanceIO >> 128;
    }

    function _updateContext(FullContext memory context)
        private
        returns (bytes memory ctx)
    {
        uint256 callInfo = ContextDefinitions.encodeCallInfo(context.extCall.cbLevel, context.extCall.callType);
        uint256 allowanceIO =
            context.app.allowanceGranted.toUint128() |
            (uint256(context.app.allowanceWanted.toUint128()) << 128);
        ctx = abi.encode(
            callInfo,
            context.extCall.timestamp,
            context.extCall.msgSender,
            context.extCall.agreementSelector,
            context.extCall.userData,
            allowanceIO,
            context.app.allowanceUsed
        );
        _ctxStamp = keccak256(ctx);
    }

    function _isCtxValid(bytes memory ctx) private view returns (bool) {
        return ctx.length != 0 && keccak256(ctx) == _ctxStamp;
    }

    function _callExternalWithReplacedCtx(
        address target,
        bytes memory callData,
        bytes memory ctx
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        // STEP 1 : replace placeholder ctx with actual ctx
        callData = _replacePlaceholderCtx(callData, ctx);

        // STEP 2: Call external with replaced context
        // FIXME make sure existence of target due to EVM rule
        /* solhint-disable-next-line avoid-low-level-calls */
        (success, returnedData) = target.call(callData);
    }

    function _callCallback(
        ISuperApp app,
        bool isStaticall,
        bytes memory callData,
        bytes memory ctx
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        callData = _replacePlaceholderCtx(callData, ctx);

        //uint256 gasBudget = gasleft() - _GAS_RESERVATION;

        if (isStaticall) {
            /* solhint-disable-next-line avoid-low-level-calls*/
            (success, returnedData) = address(app).staticcall(callData);
        } else {
            /* solhint-disable-next-line avoid-low-level-calls*/
            (success, returnedData) = address(app).call(callData);
        }

         if (!success) {
             if (gasleft() < _GAS_RESERVATION) {
                 // this is out of gas, but the call may still fail if more gas is provied
                 // and this is okay, because there can be incentive to jail the app by providing
                 // more gas
                 revert("SF: need more more gas");
             }
         }
    }

    /**
     * @dev Replace the placeholder ctx with the actual ctx
     */
    function _replacePlaceholderCtx(bytes memory data, bytes memory ctx)
        internal pure
        returns (bytes memory dataWithCtx)
    {
        // 1.a ctx needs to be padded to align with 32 bytes bundary
        uint256 paddedLength = (ctx.length / 32 + 1) * 32;
        uint256 dataLen = data.length;

        // double check if the ctx is a placeholder ctx
        {
            uint256 placeHolderCtxLength;
            // solhint-disable-next-line no-inline-assembly
            assembly { placeHolderCtxLength := mload(add(data, dataLen)) }
            require(placeHolderCtxLength == 0, "SF: placerholder ctx should have zero length");
        }

        // 1.b remove the placeholder ctx
        // solhint-disable-next-line no-inline-assembly
        assembly { mstore(data, sub(dataLen, 0x20)) }

        // 1.c pack data with the replacement ctx
        return abi.encodePacked(
            data,
            // bytes with padded length
            uint256(ctx.length),
            ctx, new bytes(paddedLength - ctx.length) // ctx padding
        );
    }

    /// @dev Get the revert message from a call
    /// @notice This is needed in order to get the human-readable revert message from a call
    /// @param res Response of the call
    /// @return Revert message string
    function _getRevertMsg(bytes memory res) internal pure returns (string memory) {
        // If the _res length is less than 68, then the transaction failed silently (without a revert message)
        if (res.length < 68) return "SF: target reverted";
        // solhint-disable-next-line no-inline-assembly
        assembly {
            // Slice the sighash.
            res := add(res, 0x04)
        }
        return abi.decode(res, (string)); // All that remains is the revert string
    }

    /**
    * @notice Helper method to parse data and extract the method signature.
    *
    * Copied from: https://github.com/argentlabs/argent-contracts/
    * blob/master/contracts/modules/common/Utils.sol#L54-L60
    */
    function _functionPrefix(bytes memory data) internal pure returns (bytes4 prefix) {
        require(data.length >= 4, "SF: invalid functionPrefix");
        // solhint-disable-next-line no-inline-assembly
        assembly {
            prefix := mload(add(data, 0x20))
        }
    }

    modifier validCtx(bytes memory ctx) {
        require(_isCtxValid(ctx), "SF: APP_RULE_CTX_IS_READONLY");
        _;
    }
    modifier cleanCtx() {
        require(_ctxStamp == 0, "SF: APP_RULE_CTX_IS_NOT_CLEAN");
        _;
    }

    modifier isAgreement(ISuperAgreement agreementClass) {
        require(isAgreementClassListed(agreementClass), "SF: only listed agreeement allowed");
        _;
    }

    modifier onlyGovernance() {
        require(msg.sender == address(_gov), "SF: only governance allowed");
        _;
    }

    modifier onlyAgreement() {
        require(isAgreementClassListed(ISuperAgreement(msg.sender)), "SF: sender is not listed agreeement");
        _;
    }

    modifier isAppActive(ISuperApp app) {
        uint256 w = _appManifests[app].configWord;
        require(w > 0, "SF: not a super app");
        require(!SuperAppDefinitions.isAppJailed(w), "SF: app is jailed");
        _;
    }
}
