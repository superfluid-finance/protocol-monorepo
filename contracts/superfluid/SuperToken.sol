// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { Proxiable } from "../upgradability/Proxiable.sol";
import { Ownable } from "../interfaces/Ownable.sol";
import { IERC20, ISuperToken } from "../interfaces/ISuperToken.sol";
import { ISuperfluidGovernance } from "../interfaces/ISuperfluidGovernance.sol";
import { ISuperAgreement } from "../interfaces/ISuperAgreement.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";

/**
 * @dev Storage layout of SuperToken
 */
contract SuperTokenStorage {
    /* WARNING: NEVER RE-ORDER VARIABLES! Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev avoid double initialization
    bool internal _initialized;

    /// @dev ERC20 Name property
    string internal _name;
    /// @dev ERC20 Symbol property
    string internal _symbol;
    /// @dev ERC20 Decimals property
    uint8 internal _decimals;

    /// @dev The underlaying ERC20 token
    IERC20 internal _token;

    /// @dev Governance contract
    ISuperfluidGovernance internal _gov;

    /// @dev Mapping to agreement data.
    ///      Mapping order: .agreementClass.agreementID.
    ///      The generation of agreementDataID is the logic of agreement contract
    mapping(address => mapping (bytes32 => bytes)) internal _agreementData;

    /// @dev Mapping from account to agreement state of the account.
    ///      Mapping order: .agreementClass.account.
    ///      It is like RUNTIME state of the agreement for each account.
    mapping(address => mapping (address => bytes)) internal _accountStates;

    /// @dev List of enabled agreement classes for the account
    mapping(address => address[]) internal _activeAgreementClasses;

    /// @dev Settled balance for the account
    mapping(address => int256) internal _balances;

    /// @dev ERC20 Allowances Storage
    mapping (address => mapping (address => uint256)) internal _allowances;

    /// @dev Deposit needed to possible liquidations
    mapping(address => uint256) internal _deposits;
}

/**
 * @title Superfluid's token implementation
 * @author Superfluid
 */
contract SuperToken is
    Ownable,
    SuperTokenStorage, // storage should come after logic contract
    ISuperToken,
    Proxiable {
    using SignedSafeMath for int256;
    using SafeMath for uint256;

    function initialize(
        string calldata name,
        string calldata symbol,
        uint8 decimals,
        IERC20 token,
        ISuperfluidGovernance gov
    )
        external
    {
        require(!_initialized, "already initialized");
        _initialized = true;
        _owner = msg.sender;
        _name = name;
        _symbol = symbol;
        _decimals = decimals;
        _token = token;
        _gov = gov;
    }

    /*
     *  ERC20 Implementation
     */

    /**
     * @dev Returns the name of the token.
     */
    function name() public view override returns (string memory) {
        return _name;
    }

    /**
     * @dev Returns the symbol of the token, usually a shorter version of the
     * name.
     */
    function symbol() public view override returns (string memory) {
        return _symbol;
    }

    /**
     * @dev Returns the number of decimals used to get its user representation.
     * For example, if `decimals` equals `2`, a balance of `505` tokens should
     * be displayed to a user as `5,05` (`505 / 10 ** 2`).
     *
     * Tokens usually opt for a value of 18, imitating the relationship between
     * Ether and Wei. This is the value {ERC20} uses, unless {_setupDecimals} is
     * called.
     *
     * Note: This information is only used for _display_ purposes: it in
     * no way affects any of the arithmetic of the contract, including
     * {IERC20-balanceOf} and {IERC20-transfer}.
     */
    function decimals() public view override returns (uint8) {
        return _decimals;
    }

    /**
     * @dev See {IERC20-totalSupply}.
     */
    function totalSupply()
        public view override returns (uint256)
    {
        return _token.balanceOf(address(this));
    }


    /**
     * @dev See {IERC20-transfer}.
     *
     * Requirements:
     *
     * - `recipient` cannot be the zero address.
     * - the caller must have a balance of at least `amount`.
     */
    function transfer(address recipient, uint256 amount)
        public override returns (bool)
    {
        _transfer(msg.sender, recipient, amount);
        return true;
    }

    /**
     * @dev See {IERC20-allowance}.
     */
    function allowance(address account, address spender)
        public view override returns (uint256)
    {
        return _allowances[account][spender];
    }

    /**
     * @dev See {IERC20-approve}.
     *
     * Requirements:
     *
     * - `spender` cannot be the zero address.
     */
    function approve(address spender, uint256 amount)
        public override returns (bool)
    {
        _approve(msg.sender, spender, amount);
        return true;
    }

    /**
     * @dev See {IERC20-transferFrom}.
     *
     * Emits an {Approval} event indicating the updated allowance. This is not
     * required by the EIP. See the note at the beginning of {ERC20};
     *
     * Requirements:
     * - `sender` and `recipient` cannot be the zero address.
     * - `sender` must have a balance of at least `amount`.
     * - the caller must have allowance for ``sender``'s tokens of at least
     * `amount`.
     */
    function transferFrom(address sender, address recipient, uint256 amount)
        public override returns (bool)
    {
        _transfer(sender, recipient, amount);
        _approve(
            sender,
            msg.sender,
            _allowances[sender][msg.sender].sub(amount, "ERC20: transfer amount exceeds allowance")
        );
        return true;
    }

    /**
     * @dev Moves tokens `amount` from `sender` to `recipient`.
     *
     * This is internal function is equivalent to {transfer}, and can be used to
     * e.g. implement automatic token fees, slashing mechanisms, etc.
     *
     * Emits a {Transfer} event.
     *
     * Requirements:
     *
     * - `sender` cannot be the zero address.
     * - `recipient` cannot be the zero address.
     * - `sender` must have a balance of at least `amount`.
     */
    function _transfer(address sender, address recipient, uint256 amount)
        private
    {
        require(recipient != address(0), "transfer to zero address");
        require(balanceOf(sender) >= amount, "transfer amount exceeds balance");

        _balances[sender] = _balances[sender].sub(int256(amount));
        _balances[recipient] = _balances[recipient].add(int256(amount));
        emit Transfer(sender, recipient, amount);
    }

    /** @dev Creates `amount` tokens and assigns them to `account`, increasing
     * the total supply.
     *
     * Emits a {Transfer} event with `from` set to the zero address.
     *
     * Requirements
     *
     * - `to` cannot be the zero address.
     */
    function _mint(address account, uint256 amount)
        internal
    {
        require(account != address(0), "mint to zero address");

        _balances[account] = _balances[account].add(int256(amount));
        emit Transfer(address(0), account, amount);
    }

    /**
     * @dev Destroys `amount` tokens from `account`, reducing the
     * total supply.
     *
     * Emits a {Transfer} event with `to` set to the zero address.
     *
     * Requirements
     *
     * - `account` cannot be the zero address.
     * - `account` must have at least `amount` tokens.
     */
    function _burn(address account, uint256 amount)
        internal
    {
        require(account != address(0), "burn from zero address");

        _balances[account] = _balances[account].sub(int256(amount));
        emit Transfer(account, address(0), amount);
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
        require(account != address(0), "approve from zero address");
        require(spender != address(0), "approve to zero address");

        _allowances[account][spender] = amount;
        emit Approval(account, spender, amount);
    }
    /*
     * Account functions
     */

    /// @dev ISuperToken.getAccountActiveAgreements implementation
    function getAccountActiveAgreements(address account)
        public
        override
        view
        returns(address[] memory)
    {
        return _activeAgreementClasses[account];
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
        return realtimeBalanceOf(account, block.timestamp) < 0;
    }

    /// @dev ERC20.balanceOf implementation
    function balanceOf(
        address account
    )
        public
        view
        override
        returns(uint256 balance)
    {
        (int256 calBalance) = _calculateBalance(account, block.timestamp);
        return calBalance < 0 ? 0 : uint256(calBalance);
    }


    /// @dev ISuperToken.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        address account,
        uint256 timestamp
    )
        public
        override
        view
        returns (int256)
    {
        return _calculateBalance(account, timestamp);
    }


    /*
    *   Agreement functions
    */


    function depositBalanceOf(
        address account
    )
        external
        view
        override
        returns(uint256 deposit)
    {
        return _deposits[account];
    }

    /// @dev ISuperToken.getAgreementAccountState implementation
    function getAgreementAccountState(
        address agreementClass,
        address account
    )
        external
        view
        override
        returns (bytes memory state)
    {
        return _accountStates[agreementClass][account];
    }

    /// @dev ISuperToken.updateAgreementAccountState implementation
    function updateAgreementAccountState(
        address account,
        bytes calldata state
    )
        external
        override
    {
        // msg.sender is agreementClass
        require(msg.sender != account, "SuperToken: unauthorized agreement storage access");
        _takeBalanceSnapshot(account);
        _accountStates[msg.sender][account] = state;
        state.length != 0 ? _addAgreementClass(msg.sender, account) : _delAgreementClass(msg.sender, account);
        emit AgreementAccountStateUpdated(msg.sender, account, state);
    }

    /// @dev ISuperToken.createAgreement implementation
    function createAgreement(
        bytes32 id,
        bytes calldata data
    )
        external
        override
    {
        //require(_agreementData[msg.sender][id].length == 0, "SuperToken: agreement already exist");
        _agreementData[msg.sender][id] = data;
        emit AgreementCreated(msg.sender, id, data);
    }

    /// @dev ISuperToken.getAgreementData implementation
    function getAgreementData(
        address agreementClass,
        bytes32 id
    )
        external
        view
        override
        returns(bytes memory data)
    {
        return _agreementData[agreementClass][id];
    }

    /// @dev ISuperToken.updateAgreementData implementation
    function updateAgreementData(
        bytes32 id,
        bytes calldata data
    )
        external
        override
    {
        require(_agreementData[msg.sender][id].length != 0, "SuperToken: agreement does not exist");
        _agreementData[msg.sender][id] = data;
        emit AgreementUpdated(msg.sender, id, data);
    }

    /// @dev ISuperToken.terminateAgreement implementation
    function terminateAgreement(
        bytes32 id
    )
        external
        override
    {
        require(_agreementData[msg.sender][id].length != 0, "SuperToken: agreement does not exist");
        delete _agreementData[msg.sender][id];
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
    external
    override
    {

        address rewardAccount = _gov.getRewardAddress(address(_token));

        int256 balance = realtimeBalanceOf(account, block.timestamp);
        int256 remain = balance.sub(int256(deposit));

        //if there is fees to be collected discount user account, if not then discount rewardAccount
        if (remain > 0) {
            _balances[account] = _balances[account].sub(int256(deposit));
            _balances[rewardAccount] = _balances[rewardAccount].add(int256(deposit));
        } else {
            _balances[account] = _balances[account].sub(balance);
            _balances[rewardAccount] = _balances[rewardAccount].add(remain);
            _balances[liquidator] = _balances[liquidator].add(int256(deposit));
        }

        delete _agreementData[msg.sender][id];
        emit AgreementTerminated(msg.sender, id);
        emit AgreementLiquidated(msg.sender, id, account, remain > 0 ? rewardAccount : liquidator, uint256(deposit));
    }

    /*
     * ERC20 compatability functions
     */

    /// @dev ISuperToken.upgrade implementation
    function upgrade(uint256 amount) external override {
        _token.transferFrom(msg.sender, address(this), amount);
        _mint(msg.sender, amount);
        emit TokenUpgraded(msg.sender, amount);
    }

    /// @dev ISuperToken.downgrade implementation
    function downgrade(uint256 amount) external override {
        require(uint256(balanceOf(msg.sender)) >= amount, "SuperToken: downgrade amount exceeds balance");
        //review TODO touch only need, by the requirement amount
        _touch(msg.sender);
        _burn(msg.sender, amount);
        _token.transfer(msg.sender, amount);
        emit TokenDowngraded(msg.sender, amount);
    }

    /// @dev ISuperfluidGovernance.getGovernanceAddress implementation
    function getGovernanceAddress() external override view returns(address) {
        return address(_gov);
    }

    /// @dev ISuperfluidGovernance.getUnderlayingToken implementation
    function getUnderlayingToken() external override view returns(address) {
        return address(_token);
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperToken.implementation");
    }

    function getFramework() external view override returns(address gov, address superfluid) {
        return (address(_gov), _gov.getSuperfluid());
    }

    /*
    *  Internal functions
    */

    /* solhint-disable mark-callable-contracts */
    /// @dev Calculate balance as split result if negative return as zero.
    function _calculateBalance(address account, uint256 timestamp) internal view returns(int256) {

        int256 eachAgreementClassBalance;
        address agreementClass;

        for (uint256 i = 0; i < _activeAgreementClasses[account].length; i++) {
            agreementClass = _activeAgreementClasses[account][i];
            eachAgreementClassBalance = eachAgreementClassBalance.add(
                ISuperAgreement(agreementClass).realtimeBalanceOf(
                    this,
                    account,
                    _accountStates[agreementClass][account],
                    timestamp)
            );
        }

        return _balances[account].add(eachAgreementClassBalance);
    }
    /* solhint-enable mark-callable-contracts */

    /* solhint-disable mark-callable-contracts */
    /// @notice for each receiving flow, lets set the timestamp to `now`, making a partial settlement
    function _touch(address account) internal {

        address agreementClass;
        bytes memory touchState;
        _takeBalanceSnapshot(account);

        for (uint256 i = 0; i < _activeAgreementClasses[account].length; i++) {

            agreementClass = _activeAgreementClasses[account][i];
            touchState = ISuperAgreement(agreementClass).touch(
                account,
                _accountStates[agreementClass][account],
                block.timestamp);

            _accountStates[agreementClass][account] = touchState;
        }
        // FIXME: Review the settled balance vs. static balance, it should be able to merge them.
        // Examples:
        // case 1: realtime balance -2 (settled balance is 0, static balance is 0, state balance -2)
        // after touching:
        // WRONG: real-time balance 0 (settled balance is 0, static balance is 0, state balance 0)
        // CORRECT: real-time balance 0 (settled balance is -2, static balance is 0, state balance 0)
        /*
        if (_balances[account] > 0) {
            _mint(account, uint256(_balances[account]));
            _balances[account] = 0;
        }
        */
    }
    /* solhint-enable mark-callable-contracts */


    /// @dev if returns -1 agreement is not active
    function _indexOfAgreementClass(address agreementClass, address account) internal view returns(int256) {

        int256 i;
        int256 size = int256(_activeAgreementClasses[account].length);

        while (i < size) {

            if (_activeAgreementClasses[account][uint256(i)] == agreementClass) {
                return i;
            }

            i++;
        }

        return -1;
    }

    /// @dev Delete Agreement Class to account
    /// @param agreementClass Agreement address to delete
    /// @param account Address to delete the agreeement
    function _delAgreementClass(address agreementClass, address account) internal {

        int256 idx = _indexOfAgreementClass(agreementClass, account);
        uint256 size = _activeAgreementClasses[account].length;

        if (idx >= 0) {

            if (size - 1 == uint256(idx)) {
                _activeAgreementClasses[account].pop();
            } else {
                //swap element and pop
                _activeAgreementClasses[account][uint256(idx)] = _activeAgreementClasses[account][size - 1];
                _activeAgreementClasses[account].pop();
            }
        }
    }

    /// @dev Add Agreement Class to account
    /// @param agreementClass Agreement address to add
    /// @param account Address to add the agreeement
    function _addAgreementClass(address agreementClass, address account) internal {
        if (_indexOfAgreementClass(agreementClass, account) == -1) {
            _activeAgreementClasses[account].push(agreementClass);
        }
    }

    /// @dev Save the balance until now
    /// @param account User to snapshot balance
    function _takeBalanceSnapshot(address account) internal {
        _balances[account] = realtimeBalanceOf(account, block.timestamp);
    }

    function updateCode(address newAddress) external onlyOwner {
        return _updateCodeAddress(newAddress);
    }

    function takeDeposit(address account, int256 deposit) external override {
        //TODO: Lock to only agreement call
        _balances[account] = _balances[account].sub(deposit);
        _deposits[account] = _deposits[account].add(uint256(deposit));
    }

    function _partialSettle(address account, int256 delta) internal {
        //TODO: Lock caller to be agreement
        _balances[account] = _balances[account].add(delta);
    }

}
