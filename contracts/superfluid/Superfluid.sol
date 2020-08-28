// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;

import { Create2 } from "@openzeppelin/contracts/utils/Create2.sol";
import { Address } from "@openzeppelin/contracts/utils/Address.sol";

import { Ownable } from "../interfaces/Ownable.sol";
import { ISuperfluid, IERC20, ISuperToken, ISuperfluidGovernance } from "../interfaces/ISuperfluid.sol";
import { Proxiable } from "../upgradability/Proxiable.sol";
import { Proxy } from "../upgradability/Proxy.sol";

import { SuperToken } from "./SuperToken.sol";
import { SuperAppDefinitions } from "./SuperAppDefinitions.sol";
import { ContextLibrary } from "./ContextLibrary.sol";


contract SuperfluidStorage {
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

    // Composite app white-listing: source app => (target app => isAllowed)
    mapping(address => mapping(address => bool)) internal _compositeApps;
    // App manifests
    mapping(address => AppManifest) internal _appManifests;
    // Ctx stamp of the current transaction, it should always be cleared to zero before transaction finishes
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

    event Jail(address app, uint256 info);

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
            _gov
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
        require(_appManifests[msg.sender].configWord == 0 , "Superfluid: app already registered");
        _appManifests[msg.sender] = AppManifest(configWord);
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
        AppManifest memory manifest = _appManifests[app];
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
        return (_appManifests[app].configWord & SuperAppDefinitions.JAIL) != SuperAppDefinitions.JAIL;
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
        require(_isApp(msg.sender), "Superfluid: msg.sender is not an app");
        require(_isApp(targetApp), "Superfluid: target is not an app");
        _compositeApps[msg.sender][targetApp] = true;
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
        return _compositeApps[app][targetApp];
    }

    //Split the callback in the two functions so they can have different rules and returns data formats
    //TODO : msg.sender should be only SuperAgreement
    function callAppBeforeCallback(
        address app,
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
            //(newCtx, cbdata) = ContextLibrary.splitReturnedData(returnedData);
            //TODO Change counter gas measurement
            newCtx = ctx;
        } else {
            revert("Superfluid: before callback failed");
            // TODO jail if it is termination callback
        }
    }

    function callAppAfterCallback(
        address app,
        bytes calldata data,
        bytes calldata /*ctx*/
    )
        external
        override
        onlyAgreement
        onlyApp(app) // although agreement library should make sure it is an app, but we decide to double check it
        returns(bytes memory newCtx)
    {
        // TODO jail rule cleanup
        if(isAppJailed(app)) {
            _appManifests[msg.sender].configWord |= SuperAppDefinitions.JAIL;
        }

        (bool success, bytes memory returnedData) = _callCallback(app, data, false);
        newCtx = abi.decode(returnedData, (bytes));
        if(success) {
            if(!ContextLibrary.validate(newCtx, _ctxStamp)) {
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
        address agreementClass,
        bytes calldata data
    )
        external
        override
        cleanCtx
        returns(bytes memory returnedData)
    {
        //Build context data
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

    function callAgreementWithContext(
        address agreementClass,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
        validCtx(ctx)
        returns(bytes memory newCtx, bytes memory returnedData)
    {
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

    function callAppAction(
        address app,
        bytes calldata data
    )
        external
        override
        cleanCtx
        onlyApp(app)
        returns(bytes memory returnedData)
    {
        if(isAppJailed(app)) {
            _appManifests[msg.sender].configWord |= SuperAppDefinitions.JAIL;
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

    function callAppActionWithContext(
        address app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        override
        validCtx(ctx)
        returns(bytes memory newCtx)
    {
        ContextLibrary.Context memory stcCtx = ContextLibrary.decode(ctx);

        stcCtx.level++;
        require(_checkAppCallDepth(msg.sender, stcCtx.level), "SF: App Call Stack too deep");
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

    function _isAgreement(address) internal pure returns(bool) {
        return true;
    }

    function _isApp(address app) internal view returns(bool) {
        return _appManifests[app].configWord > 0;
    }

    function _checkAppCallDepth(address appAddr, uint8 currentAppLevel) internal view returns(bool) {
        uint8 appLevel = _getAppLevel(appAddr);
        if(appLevel == 1 && currentAppLevel > 1) {
            return false;
        }
        if(appLevel == 2 && currentAppLevel > 2) {
            return false;
        }
        return true;
    }

    function _getAppLevel(address appAddr) internal view returns(uint8) {
        if (_appManifests[appAddr].configWord & SuperAppDefinitions.TYPE_APP_FINAL > 0) {
            return 1;
        } else if (_appManifests[appAddr].configWord & SuperAppDefinitions.TYPE_APP_SECOND > 0) {
            return 2;
        }
        revert("Superfluid: invalid app level");
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
        bytes memory data,
        bool isStaticall
    )
        private
        returns(bool success, bytes memory returnedData)
    {
        //uint256 gasBudget = gasleft() - _GAS_RESERVATION;
        /* solhint-disable-next-line avoid-low-level-calls*/
        (success, returnedData) = isStaticall ? app.staticcall(data) : app.call(data);
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

    modifier cleanCtx() {
        require(_ctxStamp == 0, "Superfluid: Ctx is not clean");
        _;
    }

    modifier onlyAgreement() {
        require(_isAgreement(msg.sender), "Superfluid: msg.sender is not agreement");
        _;
    }

    modifier onlyApp(address app) {
        require(_isApp(app), "Superfluid: target is not an app");
        _;
    }

    modifier validCtx(bytes memory ctx) {
        require(ContextLibrary.validate(ctx, _ctxStamp), "Superfluid: Invalid ctx");
        _;
    }

}
