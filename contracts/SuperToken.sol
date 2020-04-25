pragma solidity 0.6.6;

import { IERC20 } from "./interface/IERC20.sol";
import { ISuperToken } from "./interface/ISuperToken.sol";
import "./ERC20Base.sol";
import "./interface/ISuperAgreement.sol";

/**
 * @title Superfluid's token contract
 * @notice The token contract that can upgrade any ERC20 token with realtime
 *         finance capabilities, while being ERC20 compatible itself.
 * @author Superfluid
 */
contract SuperToken is ISuperToken, ERC20Base {

    enum AccountType { Creditor, Debitor }

    //keeping the same type for clarity
    struct Account {
        int256 creditFlow;
        int256 debitFlow;
    }

    //This is needed because when we change the starting timestamp
    mapping(address => mapping(address => int256)) private _balanceSnapshot;

    //Agreements => User => Data
    mapping(address => mapping(address => bytes)) private _dataAgreements;

    //Save the relation between user and aggrement contract
    mapping(address => address[]) private _userToAgreements;

    //Sender => Receiver => Agreement
    mapping(address => mapping(address => bytes)) private _usersInAgreements;

    //Save the most recent account setting for queries
    mapping(address => Account) private _userAccount;

    //Underlaying ERC20 token
    IERC20 private _token;

    constructor (IERC20 token, string memory name, string memory symbol)
    public
    ERC20Base(name, symbol) {
        _token = token;
    }

    /// @notice Get the state of an user and agreement
    /// @param agreementClass Contract of the FlowAgreement
    /// @param account User that is part of this aggrement
    /// @return state of agreement
    function getState
    (
        address agreementClass,
        address account
    )
    external
    view
    override
    returns (bytes memory state)
    {
        return _dataAgreements[agreementClass][account];
    }

    /// @notice Register or update a agreement, for each positive flow there is a negative one
    /// @param sender User that is sending tokens in this aggrement
    /// @param receiver User that is receiving tokens in this aggrement
    /// @param senderState Sender state of agreement that will be register
    /// @param receiverState Receiver state of agreement that will be register
    function updateState
    (
        address sender,
        address receiver,
        bytes calldata senderState,
        bytes calldata receiverState
    )
    external
    override
    {
        //here the sender is the contract that implements the ISuperAgreement
        //TODO: validate the approved implementations of agreements
        require(msg.sender != sender && msg.sender != receiver, "Use the agreement contract");
        //is a update or a new aggrement?
        bool _newSender = _dataAgreements[msg.sender][sender].length == 0;
        bool _newReciever = _dataAgreements[msg.sender][receiver].length == 0;

        //if new account register so we can query later
        if (_newSender) {
            _userToAgreements[sender].push(msg.sender);
        } else {
            _takeSnapshot(msg.sender, sender);
        }

        if (_newReciever) {
            _userToAgreements[receiver].push(msg.sender);
        } else {
            _takeSnapshot(msg.sender, receiver);
        }

        //Must update accounts before changing the state
        //_updateAccount(AccountType.Debitor, msg.sender, sender, _dataAgreements[msg.sender][sender], senderState);
        //_updateAccount(AccountType.Creditor, msg.sender, receiver, _dataAgreements[msg.sender][receiver], receiverState);

        _updateAccount(AccountType.Debitor, msg.sender, sender, _dataAgreements[msg.sender][sender], senderState);
        _updateAccount(AccountType.Creditor, msg.sender, receiver, _dataAgreements[msg.sender][receiver], receiverState);

        _dataAgreements[msg.sender][sender] = senderState;
        _dataAgreements[msg.sender][receiver] = receiverState;

        _usersInAgreements[sender][receiver] = senderState;
        _usersInAgreements[receiver][sender] = receiverState;
    }

    /// @notice Upgrade ERC20 to SuperToken. This method will ´transferFrom´ the tokens. Before calling this function you should ´approve´ this contract
    /// @param amount Number of tokens to be upgraded
    function upgrade(uint256 amount) external override {
        _token.transferFrom(msg.sender, address(this), amount);
        _mint(msg.sender, amount);
    }

    /// @notice Calculate the real balance of a user, taking in consideration all flows of tokens
    /// @param account User to calculate balance
    /// @return balance User balance
    function balanceOf(address account) public view override returns (int256 balance) {

        //query each agreement contract
        int256 _agreeBalances;

        for (uint256 i = 0; i < _userToAgreements[account].length; i++) {
            /* solhint-disable not-rely-on-time, mark-callable-contracts */
            //Atention: External call
            _agreeBalances += ISuperAgreement(
                _userToAgreements[account][i]
            ).balanceOf(
                _dataAgreements[_userToAgreements[account][i]][account],
                block.timestamp
            );

            _agreeBalances += _balanceSnapshot[_userToAgreements[account][i]][account];
        }

        return int256(_balances[account]) + _agreeBalances;
    }

    function currentState(address sender, address receiver) external view override returns(bytes memory state) {
        return _usersInAgreements[sender][receiver];
    }

    function getAccountRateFlows(
        address account
    )
        external
        view
        override
        returns(int256 creditor, int256 debitor)
    {
        int256 _cred = _userAccount[account].creditFlow;
        int256 _deb = _userAccount[account].debitFlow;
        return (_cred, _deb);
    }

    /// @notice Save the balance until now
    /// @param account User to snapshot balance
    function _takeSnapshot(address agreementClass, address account) internal {
        _balanceSnapshot[agreementClass][account] += balanceOf(account);
    }

    function _updateAccount(
        AccountType actype,
        address agreement,
        address account,
        bytes memory oldState,
        bytes memory newState
    )
        internal
    {
        //Atention: External calls
        int256 _updateValue = ISuperAgreement(agreement).updateAccount(newState);
        int256 _oldValue = oldState.length == 0 ? 0 : ISuperAgreement(agreement).updateAccount(oldState);

        if (actype == AccountType.Creditor) {
            _userAccount[account].creditFlow += (_updateValue - _oldValue);
        } else {
            _userAccount[account].debitFlow += (_updateValue - _oldValue);
        }
    }
}
