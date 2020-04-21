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

    struct Account {
        bytes debit;
        bytes credit;
    }

    mapping(address => Account) private _accounts;

    //This is needed because when we change the starting timestamp
    mapping(address => int256) private _balanceSnapshot;

    //Agreements => User => Data
    mapping(address => mapping(address => bytes)) private _dataAgreements;
    //Save the relation between user and aggrement contract
    mapping(address => address[]) private _userToAgreements;

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
            snapshot(sender);
        }

        if (_newReciever) {
            _userToAgreements[receiver].push(msg.sender);
        } else {
            snapshot(receiver);
        }

        _dataAgreements[msg.sender][sender] = senderState;
        _dataAgreements[msg.sender][receiver] = receiverState;
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
            _agreeBalances += ISuperAgreement(_userToAgreements[account][0])
                .balanceOf(
                    _dataAgreements[_userToAgreements[account][0]][account],
                    block.timestamp
                );
        }

        return int256(_balances[account]) + _agreeBalances + _balanceSnapshot[account];
    }

    /// @notice Save the balance until now
    /// @param account User to snapshot balance
    function snapshot(address account) internal {
        _balanceSnapshot[account] += balanceOf(account);
    }
}
