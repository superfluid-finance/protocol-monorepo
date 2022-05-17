// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";

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
    SuperfluidGovernanceConfigs,
    ISuperfluidToken,
    ISuperToken,
    ISuperTokenFactory,
    IERC20
} from "../interfaces/superfluid/ISuperfluid.sol";

import { CallUtils } from "../libs/CallUtils.sol";
import { BaseRelayRecipient } from "../libs/BaseRelayRecipient.sol";


/// FIXME Lots of reverts in here - can put custom errors

/**
 * @dev The Superfluid host implementation.
 *
 * NOTE:
 * - Please read ISuperfluid for implementation notes.
 * - For some deeper technical notes, please visit protocol-monorepo wiki area.
 *
 * @author Superfluid
 */
contract Superfluid is
    UUPSProxiable,
    ISuperfluid,
    BaseRelayRecipient
{

    using SafeCast for uint256;

    struct AppManifest {
        uint256 configWord;
    }

    // solhint-disable-next-line var-name-mixedcase
    bool immutable public NON_UPGRADABLE_DEPLOYMENT;

    // solhint-disable-next-line var-name-mixedcase
    bool immutable public APP_WHITE_LISTING_ENABLED;

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
    /// @dev if app whitelisting is enabled, this is to make sure the keys are used only once
    mapping(bytes32 => bool) internal _appKeysUsedDeprecated;

    constructor(bool nonUpgradable, bool appWhiteListingEnabled) {
        NON_UPGRADABLE_DEPLOYMENT = nonUpgradable;
        APP_WHITE_LISTING_ENABLED = appWhiteListingEnabled;
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
    // Time
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    function getNow() public view  returns (uint256) {
        // solhint-disable-next-line not-rely-on-time
        return block.timestamp;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Governance
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    function getGovernance() external view override returns (ISuperfluidGovernance) {
        return _gov;
    }

    function replaceGovernance(ISuperfluidGovernance newGov) external override onlyGovernance {
        emit GovernanceReplaced(_gov, newGov);
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
        // register the agreement proxy
        _agreementClasses.push((agreementClass));
        _agreementClassIndices[agreementType] = _agreementClasses.length;
        emit AgreementClassRegistered(agreementType, address(agreementClassLogic));
    }

    function updateAgreementClass(ISuperAgreement agreementClassLogic) external onlyGovernance override {
        require(!NON_UPGRADABLE_DEPLOYMENT, "SF: non upgradable");
        bytes32 agreementType = agreementClassLogic.agreementType();
        uint idx = _agreementClassIndices[agreementType];
        require(idx != 0, "SF: agreement class not registered");
        UUPSProxiable proxiable = UUPSProxiable(address(_agreementClasses[idx - 1]));
        proxiable.updateCode(address(agreementClassLogic));
        emit AgreementClassUpdated(agreementType, address(agreementClassLogic));
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
        emit SuperTokenFactoryUpdated(_superTokenFactory);
    }

    function updateSuperTokenLogic(ISuperToken token)
        external override
        onlyGovernance
    {
        address code = address(_superTokenFactory.getSuperTokenLogic());
        // assuming it's uups proxiable
        UUPSProxiable(address(token)).updateCode(code);
        emit SuperTokenLogicUpdated(token, code);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // App Registry
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    function registerApp(
        uint256 configWord
    )
        external override
    {
        // check if whitelisting required
        if (APP_WHITE_LISTING_ENABLED) {
            revert("SF: app registration requires permission");
        }
        _registerApp(configWord, ISuperApp(msg.sender), true);
    }

    function registerAppWithKey(uint256 configWord, string calldata registrationKey)
        external override
    {
        bytes32 configKey = SuperfluidGovernanceConfigs.getAppRegistrationConfigKey(
            // solhint-disable-next-line avoid-tx-origin
            tx.origin,
            registrationKey
        );
        // check if the key is valid and not expired
        require(
            _gov.getConfigAsUint256(
                this,
                ISuperfluidToken(address(0)),
                configKey
            // solhint-disable-next-line not-rely-on-time
            ) >= block.timestamp,
            "SF: invalid or expired registration key"
        );
        _registerApp(configWord, ISuperApp(msg.sender), true);
    }

    function registerAppByFactory(
        ISuperApp app,
        uint256 configWord
    )
        external override
    {
        // msg sender must be a contract
        {
            uint256 cs;
            // solhint-disable-next-line no-inline-assembly
            assembly { cs := extcodesize(caller()) }
            require(cs > 0, "SF: factory must be a contract");
        }

        if (APP_WHITE_LISTING_ENABLED) {
            // check if msg sender is authorized to register
            bytes32 configKey = SuperfluidGovernanceConfigs.getAppFactoryConfigKey(msg.sender);
            bool isAuthorizedAppFactory = _gov.getConfigAsUint256(
                this,
                ISuperfluidToken(address(0)),
                configKey) == 1;

            require(isAuthorizedAppFactory, "SF: authorized factory required");
        }
        _registerApp(configWord, app, false);
    }

    function _registerApp(uint256 configWord, ISuperApp app, bool checkIfInAppConstructor) private
    {
        // solhint-disable-next-line avoid-tx-origin
        require(msg.sender != tx.origin, "SF: APP_RULE_NO_REGISTRATION_FOR_EOA");

        if (checkIfInAppConstructor) {
            uint256 cs;
            // solhint-disable-next-line no-inline-assembly
            assembly { cs := extcodesize(app) }
            require(cs == 0, "SF: APP_RULE_REGISTRATION_ONLY_IN_CONSTRUCTOR");
        }
        require(
            SuperAppDefinitions.isConfigWordClean(configWord) &&
            SuperAppDefinitions.getAppLevel(configWord) > 0 &&
            (configWord & SuperAppDefinitions.APP_JAIL_BIT) == 0,
            "SF: invalid config word");
        require(_appManifests[ISuperApp(app)].configWord == 0 , "SF: app already registered");
        _appManifests[ISuperApp(app)] = AppManifest(configWord);
        emit AppRegistered(app);
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
        assertValidCtx(ctx)
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
        assertValidCtx(ctx)
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
        int256 appAllowanceUsed,
        ISuperfluidToken appAllowanceToken
    )
        external override
        onlyAgreement
        assertValidCtx(ctx)
        returns (bytes memory appCtx)
    {
        Context memory context = decodeCtx(ctx);
        if (isApp(ISuperApp(context.msgSender))) {
            require(_compositeApps[ISuperApp(context.msgSender)][app],
                "SF: APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED");
        }
        context.appLevel++;
        context.callType = ContextDefinitions.CALL_INFO_CALL_TYPE_APP_CALLBACK;
        context.appAllowanceGranted = appAllowanceGranted;
        context.appAllowanceWanted = 0;
        context.appAllowanceUsed = appAllowanceUsed;
        context.appAddress = address(app);
        context.appAllowanceToken = appAllowanceToken;
        appCtx = _updateContext(context);
    }

    function appCallbackPop(
        bytes calldata ctx,
        int256 appAllowanceUsedDelta
    )
        external override
        onlyAgreement
        returns (bytes memory newCtx)
    {
        Context memory context = decodeCtx(ctx);
        context.appAllowanceUsed = context.appAllowanceUsed + appAllowanceUsedDelta;
        newCtx = _updateContext(context);
    }

    function ctxUseAllowance(
        bytes calldata ctx,
        uint256 appAllowanceWantedMore,
        int256 appAllowanceUsedDelta
    )
        external override
        onlyAgreement
        assertValidCtx(ctx)
        returns (bytes memory newCtx)
    {
        Context memory context = decodeCtx(ctx);

        context.appAllowanceWanted = context.appAllowanceWanted + appAllowanceWantedMore;
        context.appAllowanceUsed = context.appAllowanceUsed + appAllowanceUsedDelta;

        newCtx = _updateContext(context);
    }

    function jailApp(
        bytes calldata ctx,
        ISuperApp app,
        uint256 reason
    )
        external override
        onlyAgreement
        assertValidCtx(ctx)
        returns (bytes memory newCtx)
    {
        _jailApp(app, reason);
        return ctx;
    }

    /**************************************************************************
    * Contextless Call Proxies
    *************************************************************************/

    function _callAgreement(
        address msgSender,
        ISuperAgreement agreementClass,
        bytes memory callData,
        bytes memory userData
    )
        internal
        cleanCtx
        isAgreement(agreementClass)
        returns(bytes memory returnedData)
    {
        // beaware of the endiness
        bytes4 agreementSelector = CallUtils.parseSelector(callData);

        //Build context data
        bytes memory  ctx = _updateContext(Context({
            appLevel: isApp(ISuperApp(msgSender)) ? 1 : 0,
            callType: ContextDefinitions.CALL_INFO_CALL_TYPE_AGREEMENT,
            timestamp: getNow(),
            msgSender: msgSender,
            agreementSelector: agreementSelector,
            userData: userData,
            appAllowanceGranted: 0,
            appAllowanceWanted: 0,
            appAllowanceUsed: 0,
            appAddress: address(0),
            appAllowanceToken: ISuperfluidToken(address(0))
        }));
        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(agreementClass), callData, ctx);
        if (!success) {
            CallUtils.revertFromReturnedData(returnedData);
        }
        // clear the stamp
        _ctxStamp = 0;
    }

    function callAgreement(
        ISuperAgreement agreementClass,
        bytes memory callData,
        bytes memory userData
    )
        external override
        returns(bytes memory returnedData)
    {
        return _callAgreement(msg.sender, agreementClass, callData, userData);
    }

    function _callAppAction(
        address msgSender,
        ISuperApp app,
        bytes memory callData
    )
        internal
        cleanCtx
        isAppActive(app)
        isValidAppAction(callData)
        returns(bytes memory returnedData)
    {
        //Build context data
        bytes memory ctx = _updateContext(Context({
            appLevel: isApp(ISuperApp(msgSender)) ? 1 : 0,
            callType: ContextDefinitions.CALL_INFO_CALL_TYPE_APP_ACTION,
            timestamp: getNow(),
            msgSender: msgSender,
            agreementSelector: 0,
            userData: "",
            appAllowanceGranted: 0,
            appAllowanceWanted: 0,
            appAllowanceUsed: 0,
            appAddress: address(app),
            appAllowanceToken: ISuperfluidToken(address(0))
        }));
        bool success;
        (success, returnedData) = _callExternalWithReplacedCtx(address(app), callData, ctx);
        if (success) {
            ctx = abi.decode(returnedData, (bytes));
            require(_isCtxValid(ctx), "SF: APP_RULE_CTX_IS_READONLY");
        } else {
            CallUtils.revertFromReturnedData(returnedData);
        }
        // clear the stamp
        _ctxStamp = 0;
    }

    function callAppAction(
        ISuperApp app,
        bytes memory callData
    )
        external override // NOTE: modifiers are called in _callAppAction
        returns(bytes memory returnedData)
    {
        return _callAppAction(msg.sender, app, callData);
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
        requireValidCtx(ctx)
        isAgreement(agreementClass)
        returns (bytes memory newCtx, bytes memory returnedData)
    {
        Context memory context = decodeCtx(ctx);
        require(context.appAddress == msg.sender,  "SF: callAgreementWithContext from wrong address");

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
            CallUtils.revertFromReturnedData(returnedData);
        }
    }

    function callAppActionWithContext(
        ISuperApp app,
        bytes calldata callData,
        bytes calldata ctx
    )
        external override
        requireValidCtx(ctx)
        isAppActive(app)
        isValidAppAction(callData)
        returns(bytes memory newCtx)
    {
        Context memory context = decodeCtx(ctx);
        require(context.appAddress == msg.sender,  "SF: callAppActionWithContext from wrong address");

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
            CallUtils.revertFromReturnedData(returnedData);
        }
    }

    function decodeCtx(bytes memory ctx)
        public pure override
        returns (Context memory context)
    {
        return _decodeCtx(ctx);
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

    function _batchCall(
        address msgSender,
        Operation[] memory operations
    )
       internal
    {
        for(uint256 i = 0; i < operations.length; i++) {
            uint32 operationType = operations[i].operationType;
            if (operationType == BatchOperation.OPERATION_TYPE_ERC20_APPROVE) {
                (address spender, uint256 amount) =
                    abi.decode(operations[i].data, (address, uint256));
                ISuperToken(operations[i].target).operationApprove(
                    msgSender,
                    spender,
                    amount);
            } else if (operationType == BatchOperation.OPERATION_TYPE_ERC20_TRANSFER_FROM) {
                (address sender, address receiver, uint256 amount) =
                    abi.decode(operations[i].data, (address, address, uint256));
                ISuperToken(operations[i].target).operationTransferFrom(
                    msgSender,
                    sender,
                    receiver,
                    amount);
            } else if (operationType == BatchOperation.OPERATION_TYPE_SUPERTOKEN_UPGRADE) {
                ISuperToken(operations[i].target).operationUpgrade(
                    msgSender,
                    abi.decode(operations[i].data, (uint256)));
            } else if (operationType == BatchOperation.OPERATION_TYPE_SUPERTOKEN_DOWNGRADE) {
                ISuperToken(operations[i].target).operationDowngrade(
                    msgSender,
                    abi.decode(operations[i].data, (uint256)));
            } else if (operationType == BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT) {
                (bytes memory callData, bytes memory userData) = abi.decode(operations[i].data, (bytes, bytes));
                _callAgreement(
                    msgSender,
                    ISuperAgreement(operations[i].target),
                    callData,
                    userData);
            } else if (operationType == BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_APP_ACTION) {
                _callAppAction(
                    msgSender,
                    ISuperApp(operations[i].target),
                    operations[i].data);
            } else {
               revert("SF: unknown batch call operation type");
            }
        }
    }

    /// @dev ISuperfluid.batchCall implementation
    function batchCall(
       Operation[] memory operations
    )
       external override
    {
        _batchCall(msg.sender, operations);
    }

    /// @dev ISuperfluid.forwardBatchCall implementation
    function forwardBatchCall(Operation[] memory operations)
        external override
    {
        _batchCall(_getTransactionSigner(), operations);
    }

    /// @dev BaseRelayRecipient.isTrustedForwarder implementation
    function isTrustedForwarder(address forwarder)
        public view override
        returns(bool)
    {
        return _gov.getConfigAsUint256(
            this,
            ISuperfluidToken(address(0)),
            SuperfluidGovernanceConfigs.getTrustedForwarderConfigKey(forwarder)
        ) != 0;
    }

    /// @dev IRelayRecipient.isTrustedForwarder implementation
    function versionRecipient()
        external override pure
        returns (string memory)
    {
        return "v1";
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
        // NOTE: nested encoding done due to stack too deep error when decoding in _decodeCtx
        ctx = abi.encode(
            abi.encode(
                callInfo,
                context.timestamp,
                context.msgSender,
                context.agreementSelector,
                context.userData
            ),
            abi.encode(
                allowanceIO,
                context.appAllowanceUsed,
                context.appAddress,
                context.appAllowanceToken
            )
        );
        _ctxStamp = keccak256(ctx);
    }

    function _decodeCtx(bytes memory ctx)
        private pure
        returns (Context memory context)
    {
        bytes memory ctx1;
        bytes memory ctx2;
        (ctx1, ctx2) = abi.decode(ctx, (bytes, bytes));
        {
            uint256 callInfo;
            (
                callInfo,
                context.timestamp,
                context.msgSender,
                context.agreementSelector,
                context.userData
            ) = abi.decode(ctx1, (
                uint256,
                uint256,
                address,
                bytes4,
                bytes));
            (context.appLevel, context.callType) = ContextDefinitions.decodeCallInfo(callInfo);
        }
        {
            uint256 allowanceIO;
            (
                allowanceIO,
                context.appAllowanceUsed,
                context.appAddress,
                context.appAllowanceToken
            ) = abi.decode(ctx2, (
                uint256,
                int256,
                address,
                ISuperfluidToken));
            context.appAllowanceGranted = allowanceIO & type(uint128).max;
            context.appAllowanceWanted = allowanceIO >> 128;
        }
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
                    CallUtils.revertFromReturnedData(returnedData);
                } else {
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
        // 1.a ctx needs to be padded to align with 32 bytes boundary
        uint256 dataLen = data.length;

        // Double check if the ctx is a placeholder ctx
        //
        // NOTE: This can't check all cases - user can still put nonzero length of zero data
        // developer experience check. So this is more like a sanity check for clumsy app developers.
        //
        // So, agreements MUST NOT TRUST the ctx passed to it, and always use the isCtxValid first.
        {
            uint256 placeHolderCtxLength;
            // NOTE: len(data) is data.length + 32 https://docs.soliditylang.org/en/latest/abi-spec.html
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
        // NOTE: the alternative placeholderCtx is passing extra calldata to the agreements
        // agreements would use assembly code to read the ctx
        // Because selector is part of calldata, we do the padding internally, instead of
        // outside
    }

    modifier requireValidCtx(bytes memory ctx) {
        require(_isCtxValid(ctx), "SF: APP_RULE_CTX_IS_NOT_VALID");
        _;
    }

    modifier assertValidCtx(bytes memory ctx) {
        assert(_isCtxValid(ctx));
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

    modifier isValidAppAction(bytes memory callData) {
        bytes4 actionSelector = CallUtils.parseSelector(callData);
        if (actionSelector == ISuperApp.beforeAgreementCreated.selector ||
            actionSelector == ISuperApp.afterAgreementCreated.selector ||
            actionSelector == ISuperApp.beforeAgreementUpdated.selector ||
            actionSelector == ISuperApp.afterAgreementCreated.selector ||
            actionSelector == ISuperApp.beforeAgreementTerminated.selector ||
            actionSelector == ISuperApp.afterAgreementCreated.selector) {
            revert("SF: agreement callback is not action");
        }
        _;
    }
}
