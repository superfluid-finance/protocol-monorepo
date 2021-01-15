// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
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
    BatchOperation,
    ISuperfluidToken,
    ISuperToken,
    ISuperTokenFactory,
    IERC20
} from "../interfaces/superfluid/ISuperfluid.sol";

import { CallUtils } from "../utils/CallUtils.sol";

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

    struct AppManifest {
        uint256 configWord;
    }

    // solhint-disable-next-line var-name-mixedcase
    bool immutable public NON_UPGRADABLE_DEPLOYMENT;

    /**
     * @dev Maximum number of level of apps can be composed together
     *
     * NOTE:
     * - TODO Composite app feature is currently disabled. Hence app cannot
     *   will not be able to call other app.
     */
    // solhint-disable-next-line var-name-mixedcase
    uint immutable public MAX_APP_LEVEL = 1;

    // solhint-disable-next-line var-name-mixedcase
    uint64 immutable public CALLBACK_GAS_LIMIT = 3000000;

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
        NON_UPGRADABLE_DEPLOYMENT = nonUpgradable;
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
        require(!NON_UPGRADABLE_DEPLOYMENT, "SF: non upgradable");
        require(!Superfluid(newAddress).NON_UPGRADABLE_DEPLOYMENT(), "SF: cannot downgrade to non upgradable");
        _updateCodeAddress(newAddress);
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
        if (!NON_UPGRADABLE_DEPLOYMENT) {
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
        require(!NON_UPGRADABLE_DEPLOYMENT, "SF: non upgradable");
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
        if (NON_UPGRADABLE_DEPLOYMENT) return address(_superTokenFactory);
        else return UUPSProxiable(address(_superTokenFactory)).getCodeAddress();
    }

    function updateSuperTokenFactory(ISuperTokenFactory newFactory)
        external override
        onlyGovernance
    {
        if (address(_superTokenFactory) == address(0)) {
            if (!NON_UPGRADABLE_DEPLOYMENT) {
                // initialize the proxy
                UUPSProxy proxy = new UUPSProxy();
                proxy.initializeProxy(address(newFactory));
                _superTokenFactory = ISuperTokenFactory(address(proxy));
            } else {
                _superTokenFactory = newFactory;
            }
            _superTokenFactory.initialize();
        } else {
            require(!NON_UPGRADABLE_DEPLOYMENT, "SF: non upgradable");
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
        ISuperApp sourceApp = ISuperApp(msg.sender);
        require(isApp(sourceApp), "SF: sender is not an app");
        require(isApp(targetApp), "SF: target is not an app");
        require(getAppLevel(sourceApp) > getAppLevel(targetApp), "SF: source app should have higher app level");
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
        (bool success, bytes memory returnedData) = _callCallback(app, true, isTermination, callData, ctx);
        if (success) {
            if (CallUtils.isValidAbiEncodedBytes(returnedData)) {
                cbdata = abi.decode(returnedData, (bytes));
            } else {
                if (!isTermination) {
                    revert("SF: APP_RULE_CTX_IS_MALFORMATED");
                } else {
                    _jailApp(app, SuperAppDefinitions.APP_RULE_CTX_IS_MALFORMATED);
                }
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
        (bool success, bytes memory returnedData) = _callCallback(app, false, isTermination, callData, ctx);
        if (success) {
            // the non static callback should not return empty ctx
            if (CallUtils.isValidAbiEncodedBytes(returnedData)) {
                newCtx = abi.decode(returnedData, (bytes));
                if (!_isCtxValid(newCtx)) {
                    if (!isTermination) {
                        revert("SF: APP_RULE_CTX_IS_READONLY");
                    } else {
                        newCtx = ctx;
                        _jailApp(app, SuperAppDefinitions.APP_RULE_CTX_IS_READONLY);
                    }
                }
            } else {
                if (!isTermination) {
                    revert("SF: APP_RULE_CTX_IS_MALFORMATED");
                } else {
                    newCtx = ctx;
                    _jailApp(app, SuperAppDefinitions.APP_RULE_CTX_IS_MALFORMATED);
                }
            }
        } else {
            newCtx = ctx;
        }
    }

    function appCallbackPush(
        bytes calldata ctx,
        ISuperApp app,
        uint256 appAllowanceGranted,
        int256 appAllowanceUsed
    )
        external override
        onlyAgreement
        returns (bytes memory appCtx)
    {
        Context memory context = decodeCtx(ctx);
        if (isApp(ISuperApp(context.msgSender))) {
            require(!isAppJailed(app),
                "SF: APP_RULE_COMPOSITE_APP_IS_JAILED");
            require(_compositeApps[ISuperApp(context.msgSender)][app],
                "SF: APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED");
        }
        context.appLevel++;
        context.callType = ContextDefinitions.CALL_INFO_CALL_TYPE_APP_CALLBACK;
        context.appAllowanceGranted = appAllowanceGranted;
        context.appAllowanceWanted = 0;
        context.appAllowanceUsed = appAllowanceUsed;
        appCtx = _updateContext(context);
    }

    function appCallbackPop(
        bytes calldata ctx,
        int256 allowanceUsedDelta
    )
        external override
        onlyAgreement
        returns (bytes memory newCtx)
    {
        Context memory context = decodeCtx(ctx);
        context.appAllowanceUsed = context.appAllowanceUsed.add(allowanceUsedDelta);
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
        Context memory context = decodeCtx(ctx);

        context.appAllowanceWanted = context.appAllowanceWanted.add(allowanceWantedMore);
        context.appAllowanceUsed = context.appAllowanceUsed.add(allowanceUsedDelta);

        newCtx = _updateContext(context);
    }

    function jailApp(
        bytes calldata ctx,
        ISuperApp app,
        uint256 reason
    )
        external override
        onlyAgreement
        returns (bytes memory newCtx)
    {
        _jailApp(app, reason);
        return ctx;
    }

    /**************************************************************************
    * Contextless Call Proxies
    *************************************************************************/

    function callAgreement(
        ISuperAgreement agreementClass,
        bytes memory callData,
        bytes memory userData
    )
        public override
        cleanCtx
        isAgreement(agreementClass)
        returns(bytes memory returnedData)
    {
        // beaware of the endiness
        bytes4 agreementSelector = CallUtils.parseSelector(callData);

        //Build context data
        bytes memory  ctx = _updateContext(Context({
            appLevel: isApp(ISuperApp(msg.sender)) ? 1 : 0,
            callType: ContextDefinitions.CALL_INFO_CALL_TYPE_AGREEMENT,
            /* solhint-disable-next-line not-rely-on-time */
            timestamp: block.timestamp,
            msgSender: msg.sender,
            agreementSelector: agreementSelector,
            userData: userData,
            appAllowanceGranted: 0,
            appAllowanceWanted: 0,
            appAllowanceUsed: 0
        }));
        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(agreementClass), callData, ctx);
        if (!success) {
            revert(CallUtils.getRevertMsg(returnedData));
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
        bytes memory ctx = _updateContext(Context({
            appLevel: isApp(ISuperApp(msg.sender)) ? 1 : 0,
            callType: ContextDefinitions.CALL_INFO_CALL_TYPE_APP_ACTION,
            /* solhint-disable-next-line not-rely-on-time */
            timestamp: block.timestamp,
            msgSender: msg.sender,
            agreementSelector: 0,
            userData: "",
            appAllowanceGranted: 0,
            appAllowanceWanted: 0,
            appAllowanceUsed: 0
        }));
        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(app), callData, ctx);
        if (success) {
            ctx = abi.decode(returnedData, (bytes));
            require(_isCtxValid(ctx), "SF: APP_RULE_CTX_IS_READONLY");
        } else {
            revert(CallUtils.getRevertMsg(returnedData));
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
        Context memory context = decodeCtx(ctx);
        address oldSender = context.msgSender;

        context.msgSender = msg.sender;
        //context.agreementSelector =;
        context.userData = userData;
        newCtx = _updateContext(context);

        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(agreementClass), callData, newCtx);
        if (success) {
            (newCtx) = abi.decode(returnedData, (bytes));
            assert(_isCtxValid(newCtx));
            // back to old msg.sender
            context = decodeCtx(newCtx);
            context.msgSender = oldSender;
            newCtx = _updateContext(context);
        } else {
            revert(CallUtils.getRevertMsg(returnedData));
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
        Context memory context = decodeCtx(ctx);
        address oldSender = context.msgSender;

        context.msgSender = msg.sender;
        newCtx = _updateContext(context);

        (bool success, bytes memory returnedData) = _callExternalWithReplacedCtx(address(app), callData, newCtx);
        if (success) {
            (newCtx) = abi.decode(returnedData, (bytes));
            require(_isCtxValid(newCtx), "SF: APP_RULE_CTX_IS_READONLY");
            // back to old msg.sender
            context = decodeCtx(newCtx);
            context.msgSender = oldSender;
            newCtx = _updateContext(context);
        } else {
            revert(CallUtils.getRevertMsg(returnedData));
        }
    }

    function decodeCtx(bytes memory ctx)
        public pure override
        returns (Context memory context)
    {
        uint256 callInfo;
        uint256 allowanceIO;
        (
            callInfo,
            context.timestamp,
            context.msgSender,
            context.agreementSelector,
            context.userData,
            allowanceIO,
            context.appAllowanceUsed
        ) = abi.decode(ctx, (uint256, uint256, address, bytes4, bytes, uint256, int256));
        (context.appLevel, context.callType) = ContextDefinitions.decodeCallInfo(callInfo);
        context.appAllowanceGranted = allowanceIO & type(uint128).max;
        context.appAllowanceWanted = allowanceIO >> 128;
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
            uint32 opeartionType = operations[i].opeartionType;
            if (opeartionType == BatchOperation.OPERATION_TYPE_ERC20_APPROVE) {
                (address spender, uint256 amount) =
                    abi.decode(operations[i].data, (address, uint256));
                ISuperToken(operations[i].target).operationApprove(
                    msg.sender,
                    spender,
                    amount);
            } else if (opeartionType == BatchOperation.OPERATION_TYPE_ERC20_TRANSFER_FROM) {
                (address sender, address receiver, uint256 amount) =
                    abi.decode(operations[i].data, (address, address, uint256));
                ISuperToken(operations[i].target).operationTransferFrom(
                    msg.sender,
                    sender,
                    receiver,
                    amount);
            } else if (opeartionType == BatchOperation.OPERATION_TYPE_SUPERTOKEN_UPGRADE) {
                ISuperToken(operations[i].target).operationUpgrade(
                    msg.sender,
                    abi.decode(operations[i].data, (uint256)));
            } else if (opeartionType == BatchOperation.OPERATION_TYPE_SUPERTOKEN_DOWNGRADE) {
                ISuperToken(operations[i].target).operationDowngrade(
                    msg.sender,
                    abi.decode(operations[i].data, (uint256)));
            } else if (opeartionType == BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT) {
                (bytes memory callData, bytes memory userData) = abi.decode(operations[i].data, (bytes, bytes));
                callAgreement(
                    ISuperAgreement(operations[i].target),
                    callData,
                    userData);
            } else if (opeartionType == BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_APP_ACTION) {
                callAppAction(
                    ISuperApp(operations[i].target),
                    operations[i].data);
            } else {
               revert("SF: unknown batch call operation type");
            }
        }
    }

    /**************************************************************************
    * Internal
    **************************************************************************/

    function _jailApp(ISuperApp app, uint256 reason)
        internal
    {
        if ((_appManifests[app].configWord & SuperAppDefinitions.APP_JAIL_BIT) == 0) {
            _appManifests[app].configWord |= SuperAppDefinitions.APP_JAIL_BIT;
            emit Jail(app, reason);
        }
    }

    function _updateContext(Context memory context)
        private
        returns (bytes memory ctx)
    {
        require(context.appLevel <= MAX_APP_LEVEL, "SF: APP_RULE_MAX_APP_LEVEL_REACHED");
        uint256 callInfo = ContextDefinitions.encodeCallInfo(context.appLevel, context.callType);
        uint256 allowanceIO =
            context.appAllowanceGranted.toUint128() |
            (uint256(context.appAllowanceWanted.toUint128()) << 128);
        ctx = abi.encode(
            callInfo,
            context.timestamp,
            context.msgSender,
            context.agreementSelector,
            context.userData,
            allowanceIO,
            context.appAllowanceUsed
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
        assert(target != address(0));

        // STEP 1 : replace placeholder ctx with actual ctx
        callData = _replacePlaceholderCtx(callData, ctx);

        // STEP 2: Call external with replaced context
        // FIXME make sure existence of target due to EVM rule
        /* solhint-disable-next-line avoid-low-level-calls */
        (success, returnedData) = target.call(callData);

        if (success) {
            require(returnedData.length > 0, "SF: APP_RULE_CTX_IS_EMPTY");
        }
    }

    function _callCallback(
        ISuperApp app,
        bool isStaticall,
        bool isTermination,
        bytes memory callData,
        bytes memory ctx
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        assert(address(app) != address(0));

        callData = _replacePlaceholderCtx(callData, ctx);

        uint256 gasLeftBefore = gasleft();
        if (isStaticall) {
            /* solhint-disable-next-line avoid-low-level-calls*/
            (success, returnedData) = address(app).staticcall{ gas: CALLBACK_GAS_LIMIT }(callData);
        } else {
            /* solhint-disable-next-line avoid-low-level-calls*/
            (success, returnedData) = address(app).call{ gas: CALLBACK_GAS_LIMIT }(callData);
        }

        if (!success) {
            // "/ 63" is a magic to avoid out of gas attack. See https://ronan.eth.link/blog/ethereum-gas-dangers/.
            // A callback may use this to block the APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK jail rule.
            if (gasleft() > gasLeftBefore / 63) {
                if (!isTermination) {
                    revert(CallUtils.getRevertMsg(returnedData));
                } else {
                    //revert(CallUtils.getRevertMsg(returnedData)); { }
                    _jailApp(app, SuperAppDefinitions.APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK);
                }
            } else {
                // For legit out of gas issue, the call may still fail if more gas is provied
                // and this is okay, because there can be incentive to jail the app by providing
                // more gas.
                revert("SF: need more gas");
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
            ctx, new bytes(CallUtils.padLength32(ctx.length) - ctx.length) // ctx padding
        );
    }

    modifier validCtx(bytes memory ctx) {
        require(_isCtxValid(ctx), "SF: APP_RULE_CTX_IS_NOT_VALID");
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
