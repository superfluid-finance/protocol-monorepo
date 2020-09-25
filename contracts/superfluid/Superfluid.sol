// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;
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
        uint8 appLevel;
        address msgSender;
        uint256 allowance;
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
    // ERC20 Token Registry
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    function setSuperTokenLogic(ISuperToken logic) external onlyOwner {
        _superTokenLogic = logic;
    }

    function getSuperTokenLogic() external view override returns (ISuperToken) {
        return _superTokenLogic;
    }

    function _genereateERC20WrapperSalt(
        string memory symbol,
        uint8 decimals,
        IERC20 token
    ) private pure returns (bytes32 salt) {
        return keccak256(abi.encodePacked(
            symbol,
            decimals,
            token
        ));
    }

    function getERC20Wrapper(
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    view
    override
    returns (address wrapperAddress, bool created) {
        bytes32 salt = _genereateERC20WrapperSalt(symbol, decimals, token);
        wrapperAddress = Create2.computeAddress(salt, keccak256(type(Proxy).creationCode));
        created = Address.isContract(wrapperAddress);
    }

    function createERC20Wrapper(
        string calldata name,
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    override
    returns (ISuperToken)
    {
        require(address(token) != address(0), "SuperfluidRegistry: ZERO_ADDRESS");
        bytes32 salt = _genereateERC20WrapperSalt(symbol, decimals, token);
        address wrapperAddress = Create2.computeAddress(salt, keccak256(type(Proxy).creationCode));
        require(!Address.isContract(wrapperAddress), "SuperfluidRegistry: WRAPPER_EXIST");
        Proxy proxy = new Proxy{salt: salt}();
        proxy.initializeProxy(address(_superTokenLogic));
        require(wrapperAddress == address(proxy), "Superfluid: UNEXPECTED_WRAPPER_ADDRESS");
        // initialize the token
        SuperToken superToken = SuperToken(address(proxy));
        superToken.initialize(
            name,
            symbol,
            decimals,
            token,
            this
        );
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Governance
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    function setGovernance(ISuperfluidGovernance gov) external onlyOwner {
        _gov = gov;
    }

    function getGovernance() external view override returns (ISuperfluidGovernance) {
        return _gov;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // App System
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
        require(configWord > 0, "Superfluid: invalid config word");
        require(_appManifests[ISuperApp(msg.sender)].configWord == 0 , "Superfluid: app already registered");
        _appManifests[ISuperApp(msg.sender)] = AppManifest(configWord);
    }

    function getAppManifest(
        ISuperApp app
    )
    external
    view
    override
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
        public
        view
        override
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
        external
        override
    {
        require(_isApp(ISuperApp(msg.sender)), "Superfluid: msg.sender is not an app");
        require(_isApp(targetApp), "Superfluid: target is not an app");
        _compositeApps[ISuperApp(msg.sender)][targetApp] = true;
    }

    function isCompositeAppAllowed(
        ISuperApp app,
        ISuperApp targetApp
    )
        external
        view
        override
        returns (bool)
    {
        return _compositeApps[app][targetApp];
    }

    function batchCall(
        Operation[] memory operations
    )
        external
        override
    {
        require(operations.length > 1, "SF: Use the single method");
        for(uint256 i = 0; i < operations.length; i++) {
            if(operations[i].opType == OperationType.CallApp) {
                /* solhint-disable-next-line avoid-low-level-calls */
                _callAppAction(ISuperApp(operations[i].target), operations[i].data);
            } else if(operations[i].opType == OperationType.CallAgreement) {
                /* solhint-disable-next-line avoid-low-level-calls */
                _callAgreement(ISuperAgreement(operations[i].target), operations[i].data);
            } else {
                revert("not implemented");
            }
        }
    }

    //Split the callback in the two functions so they can have different rules and returns data formats
    //TODO : msg.sender should be only SuperAgreement
    function callAppBeforeCallback(
        ISuperApp app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
        onlyAgreement
        onlyApp(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        //TODO: _callCallback
        (bool success, bytes memory returnedData) = _callCallback(app, data, true);
        if (success) {
            cbdata = abi.decode(returnedData, (bytes));
            //(newCtx, cbdata) = splitReturnedData(returnedData);
            //TODO Change counter gas measurement
            newCtx = ctx;
        } else {
            revert("Superfluid: before callback failed");
            // TODO jail if it is termination callback
        }
    }

    function callAppAfterCallback(
        ISuperApp app,
        bytes calldata data,
        bytes calldata /*ctx*/
    )
        external
        override
        onlyAgreement
        onlyApp(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory newCtx)
    {
        require(!isAppJailed(app), "SF: App already jailed");

        (bool success, bytes memory returnedData) = _callCallback(app, data, false);
        newCtx = abi.decode(returnedData, (bytes));
        if(success) {
            if(!_isCtxValid(newCtx)) {
                // TODO: JAIL if callback changes ctx
                //Change return context
                emit Jail(app, uint256(Info.B_1_READONLY_CONTEXT));
            }
        } else {
            revert("SF: Insuccessful external call");
            // TODO jail if it is termination callback
        }
    }

    function callAgreement(
        ISuperAgreement agreementClass,
        bytes calldata data
    )
        external
        override
        cleanCtx
        returns(bytes memory returnedData)
    {
        //Build context data
        bytes memory ctx;
        ctx = _updateContext(FullContext({
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

    function _callAgreement(
        ISuperAgreement agreementClass,
        bytes memory data
    )
        private
        cleanCtx
        returns(bytes memory returnedData)
    {
        //Build context data
        bytes memory ctx;
        ctx = _updateContext(FullContext({
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

    function callAgreementWithContext(
        ISuperAgreement agreementClass,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
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

    function callAppAction(
        ISuperApp app,
        bytes calldata data
    )
        external
        override
        cleanCtx
        onlyApp(app)
        returns(bytes memory returnedData)
    {
        return _callAppAction(app, data);
    }

    function _callAppAction(
        ISuperApp app,
        bytes memory data
    )
        private
        cleanCtx
        onlyApp(app)
        returns(bytes memory returnedData)
    {
        require(!isAppJailed(app), "SF: App already jailed");

        //Build context data
        //TODO: Where we get the gas reservation?
        bool success;

        bytes memory ctx;
        ctx = _updateContext(FullContext({
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

    function callAppActionWithContext(
        ISuperApp app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
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
        external
        override
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        // FIXME do some non-sense with the fee for now
       // solhint-disable-next-line no-empty-blocks
        for (uint i = 0; i < fee; ++i) { }
        newCtx = ctx;
    }

    function decodeCtx(bytes calldata ctx)
        external pure
        override
        returns (
            uint8 appLevel,
            address msgSender,
            uint256 allowance,
            uint256 allowanceUsed
        )
    {
        FullContext memory context = _decodeFullContext(ctx);
        appLevel = context.appLevel;
        msgSender = context.msgSender;
        allowance = context.allowance;
        allowanceUsed = context.allowanceUsed;
    }

    function ctxUpdate(
        bytes calldata ctx,
        uint8 appLevel,
        uint256 allowance,
        uint256 allowanceUsed

    )
        external
        override
        returns (bytes memory newCtx)
    {
        FullContext memory context = _decodeFullContext(ctx);
        context.appLevel = appLevel;
        context.allowance = allowance;
        context.allowanceUsed = allowanceUsed;
        newCtx = _updateContext(context);
    }

    /* Basic Law Rules */
    function isApp(ISuperApp app) external view override returns(bool) {
        return _isApp(app);
    }

    function _isApp(ISuperApp app) internal view returns(bool) {
        return _appManifests[app].configWord > 0;
    }

    function _checkAppCallDepth(ISuperApp appAddr, uint8 currentAppLevel) internal view returns(bool) {
        uint8 appLevel = _getAppLevel(appAddr);
        if(appLevel == 1 && currentAppLevel > 1) {
            return false;
        }
        if(appLevel == 2 && currentAppLevel > 2) {
            return false;
        }
        return true;
    }

    function _getAppLevel(ISuperApp appAddr) internal view returns(uint8) {
        if (_appManifests[appAddr].configWord & SuperAppDefinitions.TYPE_APP_FINAL > 0) {
            return 1;
        } else if (_appManifests[appAddr].configWord & SuperAppDefinitions.TYPE_APP_SECOND > 0) {
            return 2;
        }
        return 0;
    }

    function getAppLevel(ISuperApp appAddr) external override view returns(uint8) {
        return _getAppLevel(appAddr);
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
            context.appLevel,
            context.msgSender,
            context.allowance,
            context.allowanceUsed
        ) = abi.decode(ctx, (uint8, address, uint256, uint256));
    }

    function _updateContext(FullContext memory context)
        private
        returns (bytes memory ctx)
    {
        ctx = abi.encode(
            context.appLevel,
            context.msgSender,
            context.allowance,
            context.allowanceUsed
        );
        _ctxStamp = keccak256(abi.encodePacked(ctx));
    }

    function _isCtxValid(bytes memory ctx) private view returns (bool) {
        return keccak256(abi.encodePacked(ctx)) == _ctxStamp;
    }

    modifier cleanCtx() {
        require(_ctxStamp == 0, "Superfluid: Ctx is not clean");
        _;
    }

    modifier onlyAgreement() {
        require(_gov.isAgreementListed(msg.sender), "SF: Only listed agreeement allowed");
        _;
    }

    modifier onlyApp(ISuperApp app) {
        require(_isApp(app), "Superfluid: target is not an app");
        _;
    }

    modifier onlyGovernance() {
        require(msg.sender == address(_gov), "SF: action not from governance");
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
}
