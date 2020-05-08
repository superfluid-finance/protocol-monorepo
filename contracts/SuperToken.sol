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
        address[] userAgreements;
    }

    struct Counter {
        uint256 flowIn;
        uint256 flowOut;
    }

    //This is needed because when we change the starting timestamp
    mapping(address => int256) private _balanceSnapshot;

    //Agreements => User => Data
    mapping(address => mapping(address => bytes)) private _dataAgreements;
    // agreement Data
    // Key: keccak(agreement, sender, receiver) => data
    mapping(bytes32 => bytes) private _usersInAgreements;

    //Save the number of flows by each agreement type
    mapping(address => mapping(address => Counter)) private _flowCounterPerAgreement;

    //Save the most recent account setting for queries
    mapping(address => Account) private _userAccount;

    //lock agreement contract caller

    mapping(address => address) public approvedAgreements;
    address public admin;

    //Underlaying ERC20 token
    IERC20 private _token;

    constructor (IERC20 token, string memory name, string memory symbol)
    public
    ERC20Base(name, symbol) {
        _token = token;
        admin = msg.sender;
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
        bool termination,
        bytes calldata senderState,
        bytes calldata receiverState
    )
    external
    onlyApproved
    override
    {
        //here the msg.sender is the contract that implements the ISuperAgreement
        //TODO: validate the approved implementations of agreements
        require(msg.sender != sender && msg.sender != receiver, "Use the agreement contract");
        _cleaningState(msg.sender, sender, receiver, termination);

        //Must update accounts before changing the state
        _updateAccount(
            AccountType.Debitor,
            _flowKey(msg.sender, sender, receiver),
            sender,
            _dataAgreements[msg.sender][sender],
            senderState
        );

        _updateAccount(
            AccountType.Creditor,
            _flowKey(msg.sender, receiver, sender),
            receiver,
            _dataAgreements[msg.sender][receiver],
            receiverState
        );

        _dataAgreements[msg.sender][sender] = senderState;
        _dataAgreements[msg.sender][receiver] = receiverState;
    }

    function _indexOf(address agreementClass, address account) internal returns(int256) {
        int256 i;
        int256 bound = int256(_userAccount[account].userAgreements.length);

        while (i < bound) {
            if (_userAccount[account].userAgreements[uint256(i)] == agreementClass) {
                return i;
            }

            i++;
        }

        return -1;
    }

    function _removeAgreement(address agreementClass, address account) internal {
        //if we only have one agreement then is just need it to pop that element
        if (_userAccount[account].userAgreements.length == 1) {
            _userAccount[account].userAgreements.pop();
        }

        int256 _idx = _indexOf(agreementClass, account);
        uint256 _size = _userAccount[account].userAgreements.length;

        //If we have a valid index then that element exist
        if (_idx >= 0) {
            if (_size - 1 == uint256(_idx)) {
                _userAccount[account].userAgreements.pop();
            } else {
                _userAccount[account].userAgreements[uint256(_idx)] = _userAccount[account].userAgreements[_size - 1];
                _userAccount[account].userAgreements.pop();
            }
        }
    }

    function _cleaningState(address agreementClass, address sender, address receiver, bool termination) internal {

        uint256 _senderLength = _userAccount[sender].userAgreements.length;
        uint256 _receiverLength = _userAccount[receiver].userAgreements.length;
        Counter storage senderCounter = _flowCounterPerAgreement[agreementClass][sender];
        Counter storage receiverCounter = _flowCounterPerAgreement[agreementClass][receiver];

        if (termination) {

            _takeSnapshot(receiver);
            if (senderCounter.flowIn == 0 && senderCounter.flowOut == 1) {
                _removeAgreement(agreementClass, sender);
                senderCounter.flowOut = 0;
            } else {
                senderCounter.flowOut--;
            }

            //Receiver user has only this in flow
            if (receiverCounter.flowIn == 1 && receiverCounter.flowOut == 0) {
                _removeAgreement(agreementClass, receiver);
                receiverCounter.flowIn = 0;
            } else {
                receiverCounter.flowIn--;
            }

        } else {
            //if new account register so we can query later
            if (_senderLength == 0 && _indexOf(agreementClass, sender) == -1) {

                _userAccount[sender].userAgreements.push(msg.sender);
                senderCounter.flowOut++;

            } else {
                _takeSnapshot(sender);
            }

            //if new account register so we can query later
            if (_receiverLength == 0 && _indexOf(agreementClass, receiver) == -1) {
                _userAccount[receiver].userAgreements.push(msg.sender);
                receiverCounter.flowIn++;

            } else {
                _takeSnapshot(receiver);
            }
        }
    }

    /// @notice Upgrade ERC20 to SuperToken. This method will ´transferFrom´ the tokens. Before calling this function you should ´approve´ this contract
    /// @param amount Number of tokens to be upgraded
    function upgrade(uint256 amount) external override {
        _token.transferFrom(msg.sender, address(this), amount);
        _mint(msg.sender, amount);
    }

    function downgrade(uint256 amount) external override {
        require(uint256(balanceOf(msg.sender)) >= amount, "amount not allowed");
        _touch(msg.sender);
        _burn(msg.sender, amount);
        _token.transfer(msg.sender, amount);
    }

    /// @notice Calculate the real balance of a user, taking in consideration all flows of tokens
    /// @param account User to calculate balance
    /// @return balance User balance
    function balanceOf(address account) public view override returns (int256 balance) {

        //query each agreement contract
        int256 _agreeBalances;

        for (uint256 i = 0; i < _userAccount[account].userAgreements.length; i++) {
            /* solhint-disable not-rely-on-time, mark-callable-contracts */
            //Atention: External call
            _agreeBalances += ISuperAgreement(
                _userAccount[account].userAgreements[i]
            ).balanceOf(
                _dataAgreements[_userAccount[account].userAgreements[i]][account],
                block.timestamp
            );
        }

        return int256(_settledBalances[account]) + _agreeBalances + _balanceSnapshot[account];
    }

    function currentState(address agreementClass, address sender, address receiver) external view override returns(bytes memory state) {
        return _usersInAgreements[_flowKey(agreementClass, sender, receiver)];
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
    function _takeSnapshot(address account) internal {
        _balanceSnapshot[account] += balanceOf(account);
    }

    function getSnapshot(address account) public view returns(int256) {
        return _balanceSnapshot[account];
    }

    function _updateAccount(
        AccountType actype,
        bytes32 flowKey,
        address account,
        bytes memory oldState,
        bytes memory newState
    )
        internal
    {
        //Atention: External calls
        (uint256 _updateTime, int256 _updateValue) = ISuperAgreement(msg.sender).decodeFlow(newState);
        int256 _oldValue;
        int256 _local;
        if (oldState.length > 0) {
            (, _oldValue) = ISuperAgreement(msg.sender).decodeFlow(oldState);
        }

        if (_usersInAgreements[flowKey].length > 0) {
            (, _local) = ISuperAgreement(msg.sender).decodeFlow(_usersInAgreements[flowKey]);
        }

        int256 _finalValue = _updateValue - _oldValue;
        bytes memory _finalState = ISuperAgreement(msg.sender).encodeFlow(_updateTime, _local + _finalValue);

        if (actype == AccountType.Creditor) {
            _userAccount[account].creditFlow += _finalValue;
        } else {
            _userAccount[account].debitFlow += _finalValue;
        }

        _usersInAgreements[flowKey] = _finalState;
    }

    /// @notice this function is a bootstrap function to test the rest of smart contract
    /// @notice for each receiving flow, lets set the timestamp to `now`, making a partial settlement
    function _touch(address account) internal {
        address _endpoint;
        bytes memory _touchState;
        int256 _settleBalance = balanceOf(account) - int256(_settledBalances[account]);

        for (uint256 i = 0; i < _userAccount[account].userAgreements.length; i++) {
            _endpoint = _userAccount[account].userAgreements[i];
            _touchState = ISuperAgreement(_endpoint).touch(_dataAgreements[_endpoint][account], block.timestamp);
            _dataAgreements[_endpoint][account] = _touchState;
            _balanceSnapshot[account] = 0;
        }

        if (_settleBalance > 0) {
            _mint(account, uint256(_settleBalance));
        }
    }

    /// @notice the key of a flow is defined as hash(agreement, sender, receiver)
    function _flowKey(address agreementClass, address sender, address receiver) internal pure returns(bytes32) {
        return keccak256(abi.encodePacked(agreementClass, sender, receiver));
    }

    //@notice add approve agreement
    function addAgreement(address agreementClass) public onlyAdmin {
        approvedAgreements[agreementClass] = agreementClass;
    }

    modifier onlyApproved() {
        require(msg.sender == approvedAgreements[msg.sender], "Use the agreement contract");
        _;
    }

    modifier onlyAdmin() {
        require(msg.sender == admin, "not admin");
        _;
    }
}
