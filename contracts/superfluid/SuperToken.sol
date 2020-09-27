// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { Proxiable } from "../upgradability/Proxiable.sol";
import { Ownable } from "../access/Ownable.sol";

import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperToken,
    ISuperAgreement,
    IERC20
} from "../interfaces/superfluid/ISuperfluid.sol";

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

    /// @dev Superfluid contract
    ISuperfluid internal _host;

    /// @dev Settled balance for the account
    mapping(address => int256) internal _balances;

    /// @dev ERC20 Allowances Storage
    mapping (address => mapping (address => uint256)) internal _allowances;

    /// @dev Active agreement bitmap
    mapping(address => uint256) internal _inactiveAgreementBitmap;
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
        ISuperfluid host
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

    /**************************************************************************
     * ERC20 Implementations
     *************************************************************************/
    /**
     * @dev See {IERC20-totalSupply}.
     */
    function totalSupply()
        public view override returns (uint256)
    {
        return _token.balanceOf(address(this));
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
        (int256 availableBalance, , ) = _calcAvailabelBalance(account, block.timestamp);
        return availableBalance < 0 ? 0 : uint256(availableBalance);
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
        address rewardAccount = gov.getRewardAddress(address(_token));

        (int256 balance, , ) = realtimeBalanceOf(account, block.timestamp);
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

        //delete _agreementData[msg.sender][id];
        emit AgreementTerminated(msg.sender, id);
        emit AgreementLiquidated(msg.sender, id, account, remain > 0 ? rewardAccount : liquidator, deposit);
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
        return address(_token);
    }

    /// @dev ISuperToken.upgrade implementation
    function upgrade(uint256 amount) external override {
        _token.transferFrom(msg.sender, address(this), amount);
        _mint(msg.sender, amount);
        emit TokenUpgraded(msg.sender, amount);
    }

    /// @dev ISuperToken.downgrade implementation
    function downgrade(uint256 amount) external override {
        require(uint256(balanceOf(msg.sender)) >= amount, "SuperToken: downgrade amount exceeds balance");
        _burn(msg.sender, amount);
        _token.transfer(msg.sender, amount);
        emit TokenDowngraded(msg.sender, amount);
    }

    /**************************************************************************
    * System functions
    *************************************************************************/
    function getHost() external view override returns(address host) {
        return address(_host);
    }

    /*
    *  Internal functions
    */

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

    modifier onlyAgreement() override {
        ISuperfluidGovernance gov = _host.getGovernance();
        require(gov.isAgreementListed(msg.sender), "SF: Only listed agreeement allowed");
        _;
    }
}
