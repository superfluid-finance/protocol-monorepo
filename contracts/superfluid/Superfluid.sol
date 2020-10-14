// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;
pragma experimental ABIEncoderV2;

import { Ownable } from "../access/Ownable.sol";
import { Proxiable } from "../upgradability/Proxiable.sol";
import { Proxy } from "../upgradability/Proxy.sol";

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperAgreement,
    ISuperApp,
    SuperAppDefinitions,
    ISuperToken,
    IERC20
} from "../interfaces/superfluid/ISuperfluid.sol";

import { SuperToken } from "./SuperToken.sol";

import { Create2 } from "@openzeppelin/contracts/utils/Create2.sol";
import { Address } from "@openzeppelin/contracts/utils/Address.sol";


contract SuperfluidStorage {

    struct FullContext {
        // For callbacks it is used to know which agreement function selector is called
        bytes4 agreementSelector;
        // The level of the app
        uint8 appLevel;
        // The intended message sender for the call
        address msgSender;
        // deposit allowance given
        uint256 allowance;
        // deposit allowance used
        uint256 allowanceUsed;
    }

    struct AppManifest {
        uint256 configWord;
    }

    /* WARNING: NEVER RE-ORDER VARIABLES! Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev Flag to avoid double initialization
    bool internal _initialized;

    /// @dev Governance contract
    ISuperfluidGovernance internal _gov;

    /// @dev Super token logic contract
    ISuperToken internal _superTokenLogic;

    /// @dev App manifests
    mapping(ISuperApp => AppManifest) internal _appManifests;
    /// @dev Composite app white-listing: source app => (target app => isAllowed)
    mapping(ISuperApp => mapping(ISuperApp => bool)) internal _compositeApps;
    /// @dev Ctx stamp of the current transaction, it should always be cleared to
    ///      zero before transaction finishes
    bytes32 internal _ctxStamp;
}

contract Superfluid is
    Ownable,
    SuperfluidStorage,
    ISuperfluid,
    Proxiable {

    enum Info {
        A_1_MANIFEST,
        A_2_DOWNSTREAM_WHITELIST,
        A_3_IMMUTABLE_CALLBACK,
        B_1_READONLY_CONTEXT,
        B_2_UPSTREAM_CONTEXT,
        B_3_CALL_JAIL_APP,
        C_2_TERMINATION_CALLBACK,
        C_3_REVERT_NO_REASON,
        C_4_GAS_LIMIT,
        E_2_GAS_REFUND,
        J_1_UPSTREAM_RESPONSABILITY
    }

    // ????? TODO
    uint64 constant private _GAS_RESERVATION = 5000;

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Proxiable
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    function initialize() external {
        require(!_initialized, "already initialized");
        _owner = msg.sender;
        _initialized = true;
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperfluidRegistry.implementation");
    }

    function updateCode(address newAddress) external onlyOwner {
        return _updateCodeAddress(newAddress);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Governance
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    function getGovernance() external view override returns (ISuperfluidGovernance) {
        return _gov;
    }

    function setGovernance(ISuperfluidGovernance gov) external onlyOwner {
        _gov = gov;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // ERC20 Token Registry
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    function setSuperTokenLogic(ISuperToken logic) external onlyOwner {
        _superTokenLogic = logic;
    }

    function getSuperTokenLogic() external view override returns (ISuperToken) {
        return _superTokenLogic;
    }

    function getERC20Wrapper(
        IERC20 underlyingToken,
        string calldata symbol
    )
        external view override
        returns (address wrapperAddress, bool created)
    {
        bytes32 salt = _genereateERC20WrapperSalt(underlyingToken, symbol);
        wrapperAddress = Create2.computeAddress(salt, keccak256(type(Proxy).creationCode));
        created = Address.isContract(wrapperAddress);
    }

    function createERC20Wrapper(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        string calldata name,
        string calldata symbol
    )
        external override
        returns (ISuperToken)
    {
        require(address(underlyingToken) != address(0), "SuperfluidRegistry: ZERO_ADDRESS");
        bytes32 salt = _genereateERC20WrapperSalt(underlyingToken, symbol);
        address wrapperAddress = Create2.computeAddress(salt, keccak256(type(Proxy).creationCode));
        require(!Address.isContract(wrapperAddress), "SuperfluidRegistry: WRAPPER_EXIST");
        Proxy proxy = new Proxy{salt: salt}();
        proxy.initializeProxy(address(_superTokenLogic));
        require(wrapperAddress == address(proxy), "Superfluid: UNEXPECTED_WRAPPER_ADDRESS");
        // initialize the token
        SuperToken superToken = SuperToken(address(proxy));
        superToken.initialize(
            underlyingToken,
            underlyingDecimals,
            name,
            symbol,
            this
        );
    }

    function _genereateERC20WrapperSalt(
        IERC20 underlyingToken,
        string calldata symbol
    )
        private pure
        returns (bytes32 salt)
    {
        return keccak256(abi.encodePacked(
            underlyingToken,
            symbol
        ));
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // App Registry
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * @notice Message sender declares it as a super app.
     * @param configWord The super app manifest configuration
     */
    function registerApp(
        uint256 configWord
    )
        external override
    {
        require(configWord > 0, "Superfluid: invalid config word");
        require(_appManifests[ISuperApp(msg.sender)].configWord == 0 , "Superfluid: app already registered");
        _appManifests[ISuperApp(msg.sender)] = AppManifest(configWord);
    }

    function isApp(ISuperApp app) public view override returns(bool) {
        return _appManifests[app].configWord > 0;
    }

    function getAppLevel(ISuperApp appAddr) public override view returns(uint8) {
        if (_appManifests[appAddr].configWord & SuperAppDefinitions.TYPE_APP_FINAL > 0) {
            return 1;
        } else if (_appManifests[appAddr].configWord & SuperAppDefinitions.TYPE_APP_SECOND > 0) {
            return 2;
        }
        return 0;
    }

    function getAppManifest(
        ISuperApp app
    )
        external view override
    returns (
        bool exist,
        uint256 configWord
    )
    {
        AppManifest memory manifest = _appManifests[app];
        return ((manifest.configWord > 0), manifest.configWord);
    }

    function isAppJailed(
        ISuperApp app
    )
        public view override
        returns(bool)
    {
        return (_appManifests[app].configWord & SuperAppDefinitions.JAIL) > 0;
    }

    /**
     * @notice White-list the target app for app composition for the source app `msg.sender`
     * @param targetApp The taget super app address
     */
    function allowCompositeApp(
        ISuperApp targetApp
    )
        external override
    {
        require(isApp(ISuperApp(msg.sender)), "Superfluid: msg.sender is not an app");
        require(isApp(targetApp), "Superfluid: target is not an app");
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
     * Agreement Callback System
     *************************************************************************/

    //Split the callback in the two functions so they can have different rules and returns data formats
    //TODO : msg.sender should be only SuperAgreement
    function callAppBeforeCallback(
        ISuperApp app,
        bytes calldata data,
        bool isTermination,
        bytes calldata ctx
    )
        external override
        onlyAgreement
        isAppActive(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        //TODO: _callCallback
        (bool success, bytes memory returnedData) = _callCallback(app, data, true);
        if (success) {
            cbdata = abi.decode(returnedData, (bytes));
            //(newCtx, cbdata) = splitReturnedData(returnedData);
            newCtx = ctx;
        } else {
            if (!isTermination) {
                revert("Superfluid: before callback failed");
            } else {
                emit Jail(app, uint256(Info.C_2_TERMINATION_CALLBACK));
            }
        }
    }

    function callAppAfterCallback(
        ISuperApp app,
        bytes calldata data,
        bool isTermination,
        bytes calldata /* ctx */
    )
        external override
        onlyAgreement
        isAppActive(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory newCtx)
    {
        require(!isAppJailed(app), "SF: App already jailed");

        (bool success, bytes memory returnedData) = _callCallback(app, data, false);
        if(success) {
            newCtx = abi.decode(returnedData, (bytes));
            if(!_isCtxValid(newCtx)) {
                // TODO: JAIL if callback changes ctx
                //Change return context
                emit Jail(app, uint256(Info.B_1_READONLY_CONTEXT));
            }
        } else {
            if (!isTermination) {
                revert("Superfluid: after callback failed");
            } else {
                emit Jail(app, uint256(Info.C_2_TERMINATION_CALLBACK));
            }
        }
    }

    function ctxUpdate(
        bytes calldata ctx,
        uint8 appLevel,
        uint256 allowance,
        uint256 allowanceUsed

    )
        external override
        onlyAgreement
        returns (bytes memory newCtx)
    {
        FullContext memory context = _decodeFullContext(ctx);
        context.appLevel = appLevel;
        context.allowance = allowance;
        context.allowanceUsed = allowanceUsed;
        newCtx = _updateContext(context);
    }


    /**************************************************************************
    * Non-app Call Proxies
    *************************************************************************/

    function callAgreement(
        ISuperAgreement agreementClass,
        bytes memory data
    )
        public override
        cleanCtx
        isAgreement(agreementClass)
        returns(bytes memory returnedData)
    {
        //Build context data
        bytes memory ctx;
        // beaware of the endiness
        bytes4 agreementSelector = bytes4(
            uint32(uint8(data[3])) |
            (uint32(uint8(data[2])) << 8) |
            (uint32(uint8(data[1])) << 16) |
            (uint32(uint8(data[0])) << 24));
        ctx = _updateContext(FullContext({
            agreementSelector: agreementSelector,
            appLevel: 0,
            msgSender: msg.sender,
            allowance: 0,
            allowanceUsed: 0
        }));
        bool success;
        (success, returnedData) = _callExternal(address(agreementClass), data, ctx);
        if (success) {
            _ctxStamp = 0;
        } else {
            revert("SF: call agreement failed");
        }
    }

    function callAppAction(
        ISuperApp app,
        bytes memory data
    )
        public override
        cleanCtx
        isAppActive(app)
        returns(bytes memory returnedData)
    {
        require(!isAppJailed(app), "SF: App already jailed");

        //Build context data
        //TODO: Where we get the gas reservation?
        bool success;

        bytes memory ctx;
        ctx = _updateContext(FullContext({
            agreementSelector: 0,
            appLevel: 0,
            msgSender: msg.sender,
            allowance: 0,
            allowanceUsed: 0
        }));
        (success, returnedData) = _callExternal(address(app), data, ctx);
        if(!success) {
            revert(string(returnedData));
        }
        _ctxStamp = 0;
    }

    function batchCall(
       Operation[] memory operations
    )
       external override
    {
        require(operations.length > 1, "SF: Use the single method");
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
               callAgreement(
                   ISuperAgreement(operations[i].target),
                   operations[i].data);
            } else if (opType == OperationType.CallApp) {
               callAppAction(
                   ISuperApp(operations[i].target),
                   operations[i].data);
            } else {
               revert("SF: Unknown operation type");
            }
        }
    }

    /**************************************************************************
     * Contextual Call Proxy and Context Utilities
     *************************************************************************/
    function callAgreementWithContext(
        ISuperAgreement agreementClass,
        bytes calldata data,
        bytes calldata ctx
    )
        external override
        isAgreement(agreementClass)
        validCtx(ctx)
        returns(bytes memory newCtx, bytes memory returnedData)
    {
        FullContext memory context = _decodeFullContext(ctx);
        address oldSender = context.msgSender;
        context.msgSender = msg.sender;
        newCtx = _updateContext(context);

        //Call app
        bool success;
        (success, returnedData) = _callExternal(address(agreementClass), data, newCtx);
        if(success) {
            (newCtx) = abi.decode(returnedData, (bytes));
            context = _decodeFullContext(newCtx);
            context.msgSender = oldSender;
            newCtx = _updateContext(context);
        } else {
            revert("SF: call agreement failed");
        }
    }

    function callAppActionWithContext(
        ISuperApp app,
        bytes calldata data,
        bytes calldata ctx
    )
        external override
        validCtx(ctx)
        returns(bytes memory newCtx)
    {
        FullContext memory context = _decodeFullContext(ctx);

        context.appLevel++;
        require(_checkAppCallDepth(ISuperApp(msg.sender), context.appLevel), "SF: App Call Stack too deep");
        newCtx = _updateContext(context);

        _callExternal(address(app), data, newCtx);
        context.appLevel--;
        newCtx = _updateContext(context);
    }

    function chargeGasFee(
        bytes calldata ctx,
        uint fee
    )
        external override
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        // FIXME do some non-sense with the fee for now
       // solhint-disable-next-line no-empty-blocks
        for (uint i = 0; i < fee; ++i) { }
        newCtx = ctx;
    }

    function decodeCtx(bytes calldata ctx)
        external pure override
        returns (
            bytes4 agreementSelector,
            uint8 appLevel,
            address msgSender,
            uint256 allowance,
            uint256 allowanceUsed
        )
    {
        FullContext memory context = _decodeFullContext(ctx);
        agreementSelector = context.agreementSelector;
        appLevel = context.appLevel;
        msgSender = context.msgSender;
        allowance = context.allowance;
        allowanceUsed = context.allowanceUsed;
    }

    /* Basic Law Rules */
    function _checkAppCallDepth(ISuperApp appAddr, uint8 currentAppLevel) internal view returns(bool) {
        uint8 appLevel = getAppLevel(appAddr);
        if(appLevel == 1 && currentAppLevel > 1) {
            return false;
        }
        if(appLevel == 2 && currentAppLevel > 2) {
            return false;
        }
        return true;
    }

    function _callExternal(
        address target,
        bytes memory data,
        bytes memory ctx
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        // STEP 1 : replace placeholder ctx with actual ctx

        // ctx needs to be padded to align with 32 bytes bouondary
        uint256 paddedLength = (ctx.length / 32 + 1) * 32;
        // ctx length has to be stored in the length word of placehoolder ctx
        // we support up to 2^16 length of the data
        data[data.length - 2] = byte(uint8(ctx.length >> 8));
        data[data.length - 1] = byte(uint8(ctx.length));
        // pack data with the replacement ctx
        ctx = abi.encodePacked(
            data,
            ctx, new bytes(paddedLength - ctx.length) // ctx padding
        );

        // STEP 2: Call external with replaced context
        /* solhint-disable-next-line avoid-low-level-calls */
        (success, returnedData) = target.call(ctx);

        if(!success) {
            revert(string(returnedData));
        }
    }

    function _callCallback(
        ISuperApp app,
        bytes memory data,
        bool isStaticall
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        //uint256 gasBudget = gasleft() - _GAS_RESERVATION;
        (success, returnedData) = isStaticall ?
            /* solhint-disable-next-line avoid-low-level-calls*/
            address(app).staticcall(data) : address(app).call(data);
         if (!success) {
             if (gasleft() < _GAS_RESERVATION) {
                 // this is out of gas, but the call may still fail if more gas is provied
                 // and this is okay, because there can be incentive to jail the app by providing
                 // more gas
                 revert("SF: Send more gas");
             } else {
                revert(string(returnedData));
                 //_appManifests[app].configWord |= SuperAppDefinitions.JAIL;
             }
         }
    }

    function _decodeFullContext(bytes memory ctx)
        private pure
        returns (FullContext memory context) {
        (
            context.agreementSelector,
            context.appLevel,
            context.msgSender,
            context.allowance,
            context.allowanceUsed
        ) = abi.decode(ctx, (bytes4, uint8, address, uint256, uint256));
    }

    function _updateContext(FullContext memory context)
        private
        returns (bytes memory ctx)
    {
        ctx = abi.encode(
            context.agreementSelector,
            context.appLevel,
            context.msgSender,
            context.allowance,
            context.allowanceUsed
        );
        _ctxStamp = keccak256(abi.encodePacked(ctx));
    }

    function _isCtxValid(bytes memory ctx) private view returns (bool) {
        return ctx.length > 0 && keccak256(abi.encodePacked(ctx)) == _ctxStamp;
    }

    modifier cleanCtx() {
        require(_ctxStamp == 0, "Superfluid: Ctx is not clean");
        _;
    }

    modifier validCtx(bytes memory ctx) {
        if(!_isCtxValid(ctx)) {
            _appManifests[ISuperApp(msg.sender)].configWord |= SuperAppDefinitions.JAIL;
            emit Jail(ISuperApp(msg.sender), uint256(Info.B_1_READONLY_CONTEXT));
        } else {
            _;
        }
    }

    modifier isAgreement(ISuperAgreement agreementClass) {
        require(_gov.isAgreementListed(address(agreementClass)), "SF: Only listed agreeement allowed");
        _;
    }

    modifier onlyAgreement() {
        require(_gov.isAgreementListed(msg.sender), "SF: Only listed agreeement allowed");
        _;
    }

    modifier isAppActive(ISuperApp app) {
        uint256 w = _appManifests[app].configWord;
        require( w > 0 && (w & SuperAppDefinitions.JAIL) == 0, "Superfluid: not an active app");
        _;
    }
}
