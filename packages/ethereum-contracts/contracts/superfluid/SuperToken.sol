// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperToken,
    ISuperAgreement,
    IERC20,
    IERC777,
    TokenInfo
} from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperfluidToken, SuperfluidToken } from "./SuperfluidToken.sol";

import { ERC777Helper } from "../utils/ERC777Helper.sol";

import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/SafeCast.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { IERC777Recipient } from "@openzeppelin/contracts/token/ERC777/IERC777Recipient.sol";
import { IERC777Sender } from "@openzeppelin/contracts/token/ERC777/IERC777Sender.sol";
import { Address } from "@openzeppelin/contracts/utils/Address.sol";


/**
 * @title Superfluid's super token implementation
 * @author Superfluid
 */
contract SuperToken is
    UUPSProxiable,
    SuperfluidToken,
    ISuperToken
{

    using SafeMath for uint256;
    using SafeCast for uint256;
    using SignedSafeMath for int256;
    using Address for address;
    using ERC777Helper for ERC777Helper.Operators;

    uint8 constant private _STANDARD_DECIMALS = 18;

    /* WARNING: NEVER RE-ORDER VARIABLES! Including the base contracts.
       Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev The underlaying ERC20 token
    IERC20 internal _underlyingToken;

    /// @dev Decimals of the underlying token
    uint8 internal _underlyingDecimals;

    /// @dev TokenInfo Name property
    string internal _name;

    /// @dev TokenInfo Symbol property
    string internal _symbol;

    /// @dev ERC20 Allowances Storage
    mapping(address => mapping (address => uint256)) internal _allowances;

    /// @dev ERC777 operators support data
    ERC777Helper.Operators internal _operators;

    // NOTE: for future compatibility, these are reserved solidity slots
    // The sub-class of SuperToken solidity slot will start after _reserve19
    uint256 internal _reserve10;
    uint256 private _reserve11;
    uint256 private _reserve12;
    uint256 private _reserve13;
    uint256 private _reserve14;
    uint256 private _reserve15;
    uint256 private _reserve16;
    uint256 private _reserve17;
    uint256 private _reserve18;
    uint256 internal _reserve19;

    constructor(
        ISuperfluid host
    )
        SuperfluidToken(host)
        // solhint-disable-next-line no-empty-blocks
    {
    }

    function initialize(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        string calldata n,
        string calldata s
    )
        external override
        initializer // OpenZeppelin Initializable
    {
        _underlyingToken = underlyingToken;
        _underlyingDecimals = underlyingDecimals;

        _name = n;
        _symbol = s;

        // register interfaces
        ERC777Helper.register(address(this));
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperToken.implementation");
    }

    function updateCode(address newAddress) external override {
        require(msg.sender == address(_host), "only host can update code");
        UUPSProxiable._updateCodeAddress(newAddress);
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
        return _STANDARD_DECIMALS;
    }

    /**************************************************************************
     * (private) Token Logics
     *************************************************************************/

    function _transferFrom(address spender, address holder, address recipient, uint amount)
        internal returns (bool)
    {
        require(holder != address(0), "SuperToken: transfer from zero address");
        require(recipient != address(0), "SuperToken: transfer to zero address");

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
        require(from != address(0), "SuperToken: transfer from zero address");
        require(to != address(0), "SuperToken: transfer to zero address");

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
        SuperfluidToken._move(from, to, amount.toInt256());

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
        bool requireReceptionAck,
        bytes memory userData,
        bytes memory operatorData
    )
        internal
    {
        require(account != address(0), "SuperToken: mint to zero address");

        SuperfluidToken._mint(account, amount);

        _callTokensReceived(operator, address(0), account, amount, userData, operatorData, requireReceptionAck);

        emit Minted(operator, account, amount, userData, operatorData);
        emit Transfer(address(0), account, amount);
    }

    /**
     * @dev Burn tokens
     * @param from address token holder address
     * @param amount uint256 amount of tokens to burn
     * @param userData bytes extra information provided by the token holder
     * @param operatorData bytes extra information provided by the operator (if any)
     */
    function _burn(
        address operator,
        address from,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData
    )
        internal
    {
        require(from != address(0), "SuperToken: burn from zero address");

        _callTokensToSend(operator, from, address(0), amount, userData, operatorData);

        SuperfluidToken._burn(from, amount);

        emit Burned(operator, from, amount, userData, operatorData);
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
        internal
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
        address implementer = ERC777Helper._ERC1820_REGISTRY.getInterfaceImplementer(
            from, ERC777Helper._TOKENS_SENDER_INTERFACE_HASH);
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
        address implementer = ERC777Helper._ERC1820_REGISTRY.getInterfaceImplementer(
            to, ERC777Helper._TOKENS_RECIPIENT_INTERFACE_HASH);
        if (implementer != address(0)) {
            IERC777Recipient(implementer).tokensReceived(operator, from, to, amount, userData, operatorData);
        } else if (requireReceptionAck) {
            require(
                !to.isContract(),
                "SuperToken: not an ERC777TokensRecipient");
        }
    }

    /**************************************************************************
     * ERC20 Implementations
     *************************************************************************/

    function totalSupply()
        public view override returns (uint256)
    {
        return _totalSupply;
    }

    function balanceOf(
        address account
    )
        public
        view
        override
        returns(uint256 balance)
    {
        // solhint-disable-next-line not-rely-on-time
        (int256 availableBalance, , ) = super.realtimeBalanceOf(account, block.timestamp);
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

    function increaseAllowance(address spender, uint256 addedValue)
        public override returns (bool) {
        _approve(msg.sender, spender, _allowances[msg.sender][spender].add(addedValue));
        return true;
    }

    function decreaseAllowance(address spender, uint256 subtractedValue)
        public override returns (bool) {
        _approve(msg.sender, spender, _allowances[msg.sender][spender].sub(subtractedValue,
            "SuperToken: decreased allowance below zero"));
        return true;
    }

    /**************************************************************************
     * ERC-777 functions
     *************************************************************************/

    function granularity() external pure override returns (uint256) { return 1; }

    function send(address recipient, uint256 amount, bytes calldata data) external override {
        _send(msg.sender, msg.sender, recipient, amount, data, "", true);
    }

    function burn(uint256 amount, bytes calldata data) external override {
        _downgrade(msg.sender, msg.sender, amount, data, "");
    }

    function isOperatorFor(address operator, address tokenHolder) external override view returns (bool) {
        return _operators.isOperatorFor(operator, tokenHolder);
    }

    function authorizeOperator(address operator) external override {
        address holder = msg.sender;
        _operators.authorizeOperator(holder, operator);
        emit AuthorizedOperator(operator, holder);
    }

    function revokeOperator(address operator) external override {
        address holder = msg.sender;
        _operators.revokeOperator(holder, operator);
        emit RevokedOperator(operator, holder);
    }

    function defaultOperators() external override view returns (address[] memory) {
        return ERC777Helper.defaultOperators(_operators);
    }

    function operatorSend(
        address sender,
        address recipient,
        uint256 amount,
        bytes calldata data,
        bytes calldata operatorData
    ) external override {
        address operator = msg.sender;
        require(_operators.isOperatorFor(operator, sender), "SuperToken: caller is not an operator for holder");
        _send(operator, sender, recipient, amount, data, operatorData, true);
    }

    function operatorBurn(
        address account,
        uint256 amount,
        bytes calldata data,
        bytes calldata operatorData
    ) external override {
        address operator = msg.sender;
        require(_operators.isOperatorFor(operator, account), "SuperToken: caller is not an operator for holder");
        _downgrade(operator, account, amount, data, operatorData);
    }

    function _setupDefaultOperators(address[] memory operators) internal {
        _operators.setupDefaultOperators(operators);
    }

    /**************************************************************************
     * SuperToken custom token functions
     *************************************************************************/

    function selfMint(
        address account,
        uint256 amount,
        bytes memory userData
    )
        external override
        onlySelf
    {
        _mint(msg.sender, account, amount,
            true /* requireReceptionAck */, userData, new bytes(0));
    }

    function selfBurn(
       address account,
       uint256 amount,
       bytes memory userData
    )
       external override
       onlySelf
    {
       _burn(msg.sender, account, amount, userData, new bytes(0));
    }

    /**************************************************************************
     * SuperToken extra functions
     *************************************************************************/

    function transferAll(address recipient)
        external override
    {
        _transferFrom(msg.sender, msg.sender, recipient, balanceOf(msg.sender));
    }

    /**************************************************************************
     * ERC20 wrapping
     *************************************************************************/

    /// @dev ISuperfluidGovernance.getUnderlyingToken implementation
    function getUnderlyingToken() external view override returns(address) {
        return address(_underlyingToken);
    }

    /// @dev ISuperToken.upgrade implementation
    function upgrade(uint256 amount) external override {
        _upgrade(msg.sender, msg.sender, msg.sender, amount, "", "");
    }

    /// @dev ISuperToken.upgradeTo implementation
    function upgradeTo(address to, uint256 amount, bytes calldata data) external override {
        _upgrade(msg.sender, msg.sender, to, amount, "", data);
    }

    /// @dev ISuperToken.downgrade implementation
    function downgrade(uint256 amount) external override {
        _downgrade(msg.sender, msg.sender, amount, "", "");
    }

    function _upgrade(
        address operator,
        address account,
        address to,
        uint256 amount,
        bytes memory userData,
        bytes memory operatorData
    ) private {
        require(address(_underlyingToken) != address(0), "SuperToken: no underlying token");
        (uint256 underlyingAmount, uint256 actualAmount) = _toUnderlyingAmount(amount);
        _underlyingToken.transferFrom(account, address(this), underlyingAmount);
        _mint(operator, to, actualAmount,
            // if `to` is diffferent from `account`, we requireReceptionAck
            account != to, userData, operatorData);
        emit TokenUpgraded(account, actualAmount);
    }

    function _downgrade(
        address operator,
        address account,
        uint256 amount,
        bytes memory data,
        bytes memory operatorData) private {
        require(address(_underlyingToken) != address(0), "SuperToken: no underlying token");
        // - in case of downcasting of decimals, actual amount can be smaller than requested amount
        (uint256 underlyingAmount, uint256 actualAmount) = _toUnderlyingAmount(amount);
         // _burn will check the (actual) amount availability again
        _burn(operator, account, actualAmount, data, operatorData);
        _underlyingToken.transfer(account, underlyingAmount);
        emit TokenDowngraded(account, actualAmount);
    }

    function _toUnderlyingAmount(uint256 amount)
        private view
        returns (uint256 underlyingAmount, uint256 actualAmount)
    {
        uint256 factor;
        if (_underlyingDecimals < _STANDARD_DECIMALS) {
            factor = 10 ** (_STANDARD_DECIMALS - _underlyingDecimals);
            underlyingAmount = amount / factor;
            // remove precision errors
            actualAmount = underlyingAmount * factor;
        } else if (_underlyingDecimals > _STANDARD_DECIMALS) {
            factor = 10 ** (_underlyingDecimals - _STANDARD_DECIMALS);
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
        address spender,
        address recipient,
        uint256 amount
    )
        external override
        onlyHost
    {
        _transferFrom(account, spender, recipient, amount);
    }

    function operationUpgrade(address account, uint256 amount)
        external override
        onlyHost
    {
        _upgrade(msg.sender, account, account, amount, "", "");
    }

    function operationDowngrade(address account, uint256 amount)
        external override
        onlyHost
    {
        _downgrade(msg.sender, account, amount, "", "");
    }

    /**************************************************************************
    * Modifiers
    *************************************************************************/

    modifier onlySelf() {
        require(msg.sender == address(this), "SuperToken: only self allowed");
        _;
    }

}
