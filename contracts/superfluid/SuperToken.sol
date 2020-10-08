// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.1;

import { Proxiable } from "../upgradability/Proxiable.sol";
import { Ownable } from "../access/Ownable.sol";

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperToken,
    ISuperAgreement,
    IERC20,
    IERC777,
    TokenInfo
} from "../interfaces/superfluid/ISuperfluid.sol";

import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { IERC1820Registry } from "@openzeppelin/contracts/introspection/IERC1820Registry.sol";
import { IERC777Recipient } from "@openzeppelin/contracts/token/ERC777/IERC777Recipient.sol";
import { IERC777Sender } from "@openzeppelin/contracts/token/ERC777/IERC777Sender.sol";
import { Address } from "@openzeppelin/contracts/utils/Address.sol";


/**
 * @dev Storage layout of SuperToken
 */
contract SuperTokenStorage {
    /* WARNING: NEVER RE-ORDER VARIABLES! Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev avoid double initialization
    bool internal _initialized;

    /// @dev The underlaying ERC20 token
    IERC20 internal _underlyingToken;
    /// @dev Decimals of the underlying token
    uint8 internal _underlyingDecimals;

    /// @dev ERC20 Name property
    string internal _name;
    /// @dev ERC20 Symbol property
    string internal _symbol;

    /// @dev Superfluid contract
    ISuperfluid internal _host;

    /// @dev Settled balance for the account
    mapping(address => int256) internal _balances;

    /// @dev ERC20 Allowances Storage
    mapping(address => mapping (address => uint256)) internal _allowances;

    /// @dev Active agreement bitmap
    mapping(address => uint256) internal _inactiveAgreementBitmap;

    /// @dev ERC777 operators
    mapping(address => mapping(address => bool)) internal _operators;

}

/**
 * @title Superfluid's token implementation
 * @author Superfluid
 */
contract SuperToken is
    Ownable,
    SuperTokenStorage, // storage should come after logic contract
    ISuperToken,
    Proxiable
{

    using Address for address;

    IERC1820Registry constant internal _ERC1820_REGISTRY = IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);

    // keccak256("ERC777TokensSender")
    bytes32 constant private _TOKENS_SENDER_INTERFACE_HASH =
        0x29ddb589b1fb5fc7cf394961c1adf5f8c6454761adf795e67fe149f658abe895;

    // keccak256("ERC777TokensRecipient")
    bytes32 constant private _TOKENS_RECIPIENT_INTERFACE_HASH =
        0xb281fc8c12954d22544db45de3159a39272895b169a852b314f9cc762e44c53b;

    string constant private _ERR_TRANSFER_FROM_ZERO_ADDRESS = "SuperToken: transfer from zero address";
    string constant private _ERR_TRANSFER_TO_ZERO_ADDRESS = "SuperToken: transfer to zero address";
    string constant private _ERR_CALLER_NOT_AN_OPERATOR = "SuperToken: caller is not an operator for holder";
    string constant private _ERR_NOT_SUPPORTED = "SuperToken: not supported";

    uint8 constant public STANDARD_DECIMALS = 18;

    using SignedSafeMath for int256;
    using SafeMath for uint256;

    function initialize(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        string calldata name,
        string calldata symbol,
        ISuperfluid host
    )
        external
    {
        require(!_initialized, "already initialized");
        _initialized = true;
        _owner = msg.sender;

        _underlyingToken = underlyingToken;
        _underlyingDecimals = underlyingDecimals;

        _name = name;
        _symbol = symbol;

        _host = host;
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperToken.implementation");
    }

    function updateCode(address newAddress) external onlyOwner {
        return _updateCodeAddress(newAddress);
    }

    /**************************************************************************
     * ERC20 Token Info
     *************************************************************************/

    function name() external view override returns (string memory) {
        return _name;
    }

    function symbol() external view override returns (string memory) {
        return _symbol;
    }

    function decimals() external pure override returns (uint8) {
        return STANDARD_DECIMALS;
    }

    /**************************************************************************
     * ERC20 Implementations
     *************************************************************************/

    function totalSupply()
        public view override returns (uint256)
    {
        return _underlyingToken.balanceOf(address(this));
    }

    function balanceOf(
        address account
    )
        public
        view
        override
        returns(uint256 balance)
    {
        (int256 availableBalance, , ) = _calcAvailabelBalance(account, block.timestamp);
        return availableBalance < 0 ? 0 : uint256(availableBalance);
    }

    function transfer(address recipient, uint256 amount)
        public override returns (bool)
    {
        return _transferFrom(msg.sender, msg.sender, recipient, amount);
    }

    function allowance(address account, address spender)
        public view override returns (uint256)
    {
        return _allowances[account][spender];
    }

    function approve(address spender, uint256 amount)
        public override
        returns (bool)
    {
        _approve(msg.sender, spender, amount);
        return true;
    }

    function transferFrom(address holder, address recipient, uint256 amount)
        public override returns (bool)
    {
        return _transferFrom(msg.sender, holder, recipient, amount);
    }

    function _transferFrom(address spender, address holder, address recipient, uint amount)
        private returns (bool)
    {
        require(holder != address(0), _ERR_TRANSFER_FROM_ZERO_ADDRESS);
        require(recipient != address(0), _ERR_TRANSFER_TO_ZERO_ADDRESS);

        address operator = msg.sender;

        _callTokensToSend(operator, holder, recipient, amount, "", "");

        _move(operator, holder, recipient, amount, "", "");

        if (spender != holder) {
            _approve(
                holder,
                spender,
                _allowances[holder][spender].sub(amount, "SuperToken: transfer amount exceeds allowance"));
        }

        _callTokensReceived(operator, holder, recipient, amount, "", "", false);

        return true;
    }

    /// @dev Calculate balance as split result if negative return as zero.
    function _calcAvailabelBalance(
        address account,
        uint256 timestamp
    )
        private view
        returns(int256 availableBalance, uint256 deposit, uint256 owedDeposit)
    {
        int256 realtimeBalance = _balances[account];
        address[] memory activeAgreements = getAccountActiveAgreements(account);
        for (uint256 i = 0; i < activeAgreements.length; i++) {
            (
                int256 agreementDynamicBalance,
                uint256 agreementDeposit,
                uint256 agreementOwedDeposit) = ISuperAgreement(activeAgreements[i])
                    .realtimeBalanceOf(
                         this,
                         account,
                         timestamp
                     );
            realtimeBalance = realtimeBalance.add(agreementDynamicBalance);
            deposit = deposit.add(agreementDeposit);
            owedDeposit = owedDeposit.add(agreementOwedDeposit);
        }
        //availableBalance = realtimeBalance;
        availableBalance = realtimeBalance
            .sub(int256(deposit))
            .add(int256(_min(deposit, owedDeposit)));
    }

    /**
     * @dev Send tokens
     * @param operator address operator address
     * @param from address token holder address
     * @param to address recipient address
     * @param amount uint256 amount of tokens to transfer
     * @param userData bytes extra information provided by the token holder (if any)
     * @param operatorData bytes extra information provided by the operator (if any)
     * @param requireReceptionAck if true, contract recipients are required to implement ERC777TokensRecipient
     */
    function _send(
        address operator,
        address from,
        address to,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData,
        bool requireReceptionAck
    )
        private
    {
        require(from != address(0), _ERR_TRANSFER_FROM_ZERO_ADDRESS);
        require(to != address(0), _ERR_TRANSFER_TO_ZERO_ADDRESS);

        _callTokensToSend(operator, from, to, amount, userData, operatorData);

        _move(operator, from, to, amount, userData, operatorData);

        _callTokensReceived(operator, from, to, amount, userData, operatorData, requireReceptionAck);
    }

    function _move(
        address operator,
        address from,
        address to,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData
    )
        private
    {
        require(balanceOf(from) >= amount, "SuperToken: transfer amount exceeds balance");

        _balances[from] = _balances[from].sub(int256(amount));
        _balances[to] = _balances[to].add(int256(amount));

        emit Sent(operator, from, to, amount, userData, operatorData);
        emit Transfer(from, to, amount);
    }

    /**
     * @dev Creates `amount` tokens and assigns them to `account`, increasing
     * the total supply.
     *
     * If a send hook is registered for `account`, the corresponding function
     * will be called with `operator`, `data` and `operatorData`.
     *
     * See {IERC777Sender} and {IERC777Recipient}.
     *
     * Emits {Minted} and {IERC20-Transfer} events.
     *
     * Requirements
     *
     * - `account` cannot be the zero address.
     * - if `account` is a contract, it must implement the {IERC777Recipient}
     * interface.
     */
    function _mint(
        address operator,
        address account,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData
    )
        internal
    {
        require(account != address(0), "SuperToken: mint to zero address");

        _balances[account] = _balances[account].add(int256(amount));

        _callTokensReceived(operator, address(0), account, amount, userData, operatorData, true);

        emit Minted(operator, account, amount, userData, operatorData);
        emit Transfer(address(0), account, amount);
    }

    /**
     * @dev Burn tokens
     * @param from address token holder address
     * @param amount uint256 amount of tokens to burn
     * @param data bytes extra information provided by the token holder
     * @param operatorData bytes extra information provided by the operator (if any)
     */
    function _burn(
        address operator,
        address from,
        uint256 amount,
        bytes memory data,
        bytes memory operatorData
    )
        internal
    {
        require(from != address(0), "SuperToken: burn from zero address");

        _callTokensToSend(operator, from, address(0), amount, data, operatorData);

        // NB! Check balance after the _callTokensToSend is called
        require(balanceOf(from) >= amount, "SuperToken: burn amount exceeds balance");

        // Update state variables
        _balances[from] = _balances[from].sub(int256(amount));

        emit Burned(operator, from, amount, data, operatorData);
        emit Transfer(from, address(0), amount);
    }

    /**
     * @notice Sets `amount` as the allowance of `spender` over the `account`s tokens.
     *
     * This is internal function is equivalent to `approve`, and can be used to
     * e.g. set automatic allowances for certain subsystems, etc.
     *
     * Emits an {Approval} event.
     *
     * Requirements:
     *
     * - `account` cannot be the zero address.
     * - `spender` cannot be the zero address.
     */
    function _approve(address account, address spender, uint256 amount)
        private
    {
        require(account != address(0), "SuperToken: approve from zero address");
        require(spender != address(0), "SuperToken: approve to zero address");

        _allowances[account][spender] = amount;
        emit Approval(account, spender, amount);
    }

    /**
     * @dev Call from.tokensToSend() if the interface is registered
     * @param operator address operator requesting the transfer
     * @param from address token holder address
     * @param to address recipient address
     * @param amount uint256 amount of tokens to transfer
     * @param userData bytes extra information provided by the token holder (if any)
     * @param operatorData bytes extra information provided by the operator (if any)
     */
    function _callTokensToSend(
        address operator,
        address from,
        address to,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData
    )
        private
    {
        address implementer = _ERC1820_REGISTRY.getInterfaceImplementer(from, _TOKENS_SENDER_INTERFACE_HASH);
        if (implementer != address(0)) {
            IERC777Sender(implementer).tokensToSend(operator, from, to, amount, userData, operatorData);
        }
    }

    /**
     * @dev Call to.tokensReceived() if the interface is registered. Reverts if the recipient is a contract but
     * tokensReceived() was not registered for the recipient
     * @param operator address operator requesting the transfer
     * @param from address token holder address
     * @param to address recipient address
     * @param amount uint256 amount of tokens to transfer
     * @param userData bytes extra information provided by the token holder (if any)
     * @param operatorData bytes extra information provided by the operator (if any)
     * @param requireReceptionAck if true, contract recipients are required to implement ERC777TokensRecipient
     */
    function _callTokensReceived(
        address operator,
        address from,
        address to,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData,
        bool requireReceptionAck
    )
        private
    {
        address implementer = _ERC1820_REGISTRY.getInterfaceImplementer(to, _TOKENS_RECIPIENT_INTERFACE_HASH);
        if (implementer != address(0)) {
            IERC777Recipient(implementer).tokensReceived(operator, from, to, amount, userData, operatorData);
        } else if (requireReceptionAck) {
            require(
                !to.isContract(),
                "SuperToken: not an ERC777TokensRecipient");
        }
    }

    /**************************************************************************
     * ERC-777 functions
     *************************************************************************/

    function granularity() external pure override returns (uint256) { return 1; }

    function send(address recipient, uint256 amount, bytes calldata data) external override {
        _send(msg.sender, msg.sender, recipient, amount, data, "", true);
    }

    function burn(uint256 /* amount */, bytes calldata /* data */) external pure override {
        revert(_ERR_NOT_SUPPORTED);
    }

    function isOperatorFor(address operator, address tokenHolder) public override view returns (bool) {
        return operator == tokenHolder || _operators[tokenHolder][operator];
    }

    function authorizeOperator(address operator) external override {
        address holder = msg.sender;
        require(holder != operator, "SuperToken: authorizing self as operator");
        _operators[holder][operator] = true;
        emit AuthorizedOperator(operator, holder);
    }

    function revokeOperator(address operator) external override {
        address holder = msg.sender;
        delete _operators[holder][operator];
        emit RevokedOperator(operator, holder);
    }

    // solhint-disable no-empty-blocks
    function defaultOperators() external override pure returns (address[] memory) {
        // FIXME support default operators
    }

    function operatorSend(
        address sender,
        address recipient,
        uint256 amount,
        bytes calldata data,
        bytes calldata operatorData
    ) external override {
        address operator = msg.sender;
        require(isOperatorFor(operator, sender), _ERR_CALLER_NOT_AN_OPERATOR);
        _send(operator, sender, recipient, amount, data, operatorData, true);
    }

    function operatorBurn(
        address /* account */,
        uint256 /* amount */,
        bytes calldata /* data */,
        bytes calldata /* operatorData */
    ) external pure override {
        revert(_ERR_NOT_SUPPORTED);
    }

    /**************************************************************************
     * Account functions
     *************************************************************************/

    /// @dev ISuperToken.getAccountActiveAgreements implementation
    function getAccountActiveAgreements(address account)
        public
        override
        view
        returns(address[] memory)
    {
        ISuperfluidGovernance gov = _host.getGovernance();
        return gov.mapAgreements(~_inactiveAgreementBitmap[account]);
    }

    /// @dev ISuperToken.isAccountInsolvent implementation
    function isAccountInsolvent(
        address account
    )
        public
        view
        override
        returns(bool)
    {
        (int256 amount, ,) = realtimeBalanceOf(account, block.timestamp);
        return amount < 0;
    }

    /// @dev ISuperToken.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        address account,
        uint256 timestamp
    )
        public
        override
        view
        returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit)
    {
        (availableBalance,
         deposit,
         owedDeposit
        ) = _calcAvailabelBalance(account, timestamp);
    }

    function realtimeBalanceOfNow(
        address account
    )
        external
        override
        view
        returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit) {
        return realtimeBalanceOf(account, block.timestamp);
    }

    function transferAll(address recipient)
        external
        override
    {
        _transferFrom(msg.sender, msg.sender, recipient, balanceOf(msg.sender));
    }

    /**************************************************************************
     * Agreement hosting functions
     *************************************************************************/

    /// @dev ISuperToken.createAgreement implementation
    function createAgreement(
        bytes32 id,
        bytes32[] calldata data
    )
        external override
        onlyAgreement
    {
        // TODO check data existence??
        address agreementClass = msg.sender;
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        _storeData(slot, data);
        emit AgreementCreated(agreementClass, id, data);
    }

    /// @dev ISuperToken.getAgreementData implementation
    function getAgreementData(
        address agreementClass,
        bytes32 id,
        uint dataLength
    )
        external view override
        returns(bytes32[] memory data)
    {
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        data = _loadData(slot, dataLength);
    }

    /// @dev ISuperToken.updateAgreementData implementation
    function updateAgreementData(
        bytes32 id,
        bytes32[] calldata data
    )
        external override
        onlyAgreement
    {
        address agreementClass = msg.sender;
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        _storeData(slot, data);
        emit AgreementUpdated(msg.sender, id, data);
    }

    /// @dev ISuperToken.terminateAgreement implementation
    function terminateAgreement(
        bytes32 id,
        uint dataLength
    )
        external override
        onlyAgreement
    {
        address agreementClass = msg.sender;
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        _eraseData(slot, dataLength);
        emit AgreementTerminated(msg.sender, id);
    }

    /// @dev ISuperToken.liquidateAgreement implementation
    function liquidateAgreement
    (
        address liquidator,
        bytes32 id,
        address account,
        uint256 deposit
    )
        external override
        onlyAgreement
    {
        ISuperfluidGovernance gov = _host.getGovernance();
        address rewardAccount = gov.getRewardAddress(address(this));

        // reward go to liquidator if reward address is null
        if (rewardAccount == address(0)) {
            rewardAccount = liquidator;
        }

        (int256 availableBalance, , ) = realtimeBalanceOf(account, block.timestamp);

        int256 remain = availableBalance.add(int256(deposit));

        // Liquidation rules:
        // #1 Can the agreement deposit can still cover the available balance deficit?
        if (remain > 0) {
            // #1.1 yes: then the reward address takes the deposit
            _balances[rewardAccount] = _balances[rewardAccount].add(int256(deposit));
            // #2.1 the account pays for the deposit
            _balances[account] = _balances[account].sub(int256(deposit));
            emit AgreementLiquidated(msg.sender, id, account, rewardAccount, deposit);
        } else {
            // #1.2 no: then the liquidator takes the deposit
            _balances[liquidator] = _balances[liquidator].add(int256(deposit));
            // #2.2 the account still pays for the deposit, but also refunded with the deficit
            _balances[account] = _balances[account]
                .sub(availableBalance)
                .sub(int256(deposit));
            // #2.3 and the reward address pay the deficit
            _balances[rewardAccount] = _balances[rewardAccount].add(availableBalance);
            emit AgreementLiquidated(msg.sender, id, account, liquidator, deposit);
        }
    }

    /// @dev ISuperToken.updateAgreementState implementation
    function updateAgreementStateSlot(
        address account,
        uint256 slotId,
        bytes32[] calldata slotData
    )
        external override
        onlyAgreement
    {
        bytes32 slot = keccak256(abi.encode("AgreementState", msg.sender, account, slotId));
        _storeData(slot, slotData);
        // FIXME change how this is done
        //_addAgreementClass(msg.sender, account);
        emit AgreementStateUpdated(msg.sender, account, slotId);
    }

    /// @dev ISuperToken.getAgreementState implementation
    function getAgreementStateSlot(
        address agreementClass,
        address account,
        uint256 slotId,
        uint dataLength
    )
        external override view
        returns (bytes32[] memory slotData) {
        bytes32 slot = keccak256(abi.encode("AgreementState", agreementClass, account, slotId));
        slotData = _loadData(slot, dataLength);
    }

    function settleBalance(
        address account,
        int256 delta
    )
        external override
        onlyAgreement
    {
        _balances[account] = _balances[account].add(delta);
    }

    /**************************************************************************
     * ERC20 wrapping
     *************************************************************************/

    /// @dev ISuperfluidGovernance.getUnderlayingToken implementation
    function getUnderlayingToken() external view override returns(address) {
        return address(_underlyingToken);
    }

    /// @dev ISuperToken.upgrade implementation
    function upgrade(uint256 amount) external override {
        _upgrade(msg.sender, amount);
    }

    function _upgrade(address account, uint256 amount) private {
        uint256 underlyingAmount;
        (underlyingAmount, amount) = _toUnderlyingAmount(amount);
        _underlyingToken.transferFrom(account, address(this), underlyingAmount);
        _mint(msg.sender /* operator */, account, amount, "", "");
        emit TokenUpgraded(account, amount);
    }

    /// @dev ISuperToken.downgrade implementation
    function downgrade(uint256 amount) external override {
        _downgrade(msg.sender, amount);
    }

    function _downgrade(address account, uint256 amount) private {
        // - even though _burn will check the (actual) amount availability again
        // we need to first check it here
        // - in case of downcasting of decimals, actual amount can be smaller than
        // requested amount
        require(balanceOf(account) >= amount, "SuperToken: downgrade amount exceeds balance");
        uint256 underlyingAmount;
        (underlyingAmount, amount) = _toUnderlyingAmount(amount);
        _burn(msg.sender /* operator */, account, amount, "", "");
        _underlyingToken.transfer(account, underlyingAmount);
        emit TokenDowngraded(account, amount);
    }

    function _toUnderlyingAmount(uint256 amount)
        private view
        returns (uint256 underlyingAmount, uint256 actualAmount)
    {
        uint256 factor;
        if (_underlyingDecimals < STANDARD_DECIMALS) {
            factor = 10 ** (STANDARD_DECIMALS - _underlyingDecimals);
            underlyingAmount = amount / factor;
            // remove precision errors
            actualAmount = underlyingAmount * factor;
        } else if (_underlyingDecimals > STANDARD_DECIMALS) {
            factor = 10 ** (_underlyingDecimals - STANDARD_DECIMALS);
            underlyingAmount = amount * factor;
            actualAmount = amount;
        } else {
            underlyingAmount = actualAmount = amount;
        }
    }

    /**************************************************************************
     * Superfluid Batch Operations
     *************************************************************************/

    function operationApprove(
        address account,
        address spender,
        uint256 amount
    )
        external override
        onlyHost
    {
        _approve(account, spender, amount);
    }

    function operationTransferFrom(
        address account,
        address sender,
        address recipient,
        uint256 amount
    )
        external override
        onlyHost
    {
        _transferFrom(account, sender, recipient, amount);
    }

    function operationUpgrade(address account, uint256 amount)
        external override
        onlyHost
    {
        _upgrade(account, amount);
    }

    function operationDowngrade(address account, uint256 amount)
        external override
        onlyHost
    {
        _downgrade(account, amount);
    }

    /**************************************************************************
    * System functions
    *************************************************************************/

    function getHost() external view override returns(address host) {
        return address(_host);
    }

    /**************************************************************************
    * Modifiers
    *************************************************************************/

    modifier onlyHost() {
        require(address(_host) == msg.sender, "SF: Only host contract allowed");
        _;
    }

    modifier onlyAgreement() {
        ISuperfluidGovernance gov = _host.getGovernance();
        require(gov.isAgreementListed(msg.sender), "SF: Only listed agreeement allowed");
        _;
    }

    /**************************************************************************
    * Other utility private functions
    *************************************************************************/

    function _storeData(bytes32 slot, bytes32[] memory data) private {
        for (uint j = 0; j < data.length; ++j) {
            bytes32 d = data[j];
            assembly { sstore(add(slot, j), d) }
        }
    }

    function _loadData(bytes32 slot, uint dataLength) private view returns (bytes32[] memory data) {
        data = new bytes32[](dataLength);
        for (uint j = 0; j < dataLength; ++j) {
            bytes32 d;
            assembly { d := sload(add(slot, j)) }
            data[j] = d;
        }
    }

    function _eraseData(bytes32 slot, uint dataLength) private {
        for (uint j = 0; j < dataLength; ++j) {
            assembly { sstore(add(slot, j), 0) }
        }
    }

    function _min(uint256 a, uint256 b) internal pure returns (uint256) {
        return a < b ? a : b;
    }

}
