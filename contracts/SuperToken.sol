pragma solidity 0.6.6;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { ISuperToken } from "./ISuperToken.sol";
import "./ERC20Base.sol";
import "./ISuperAgreement.sol";

/**
 * @title Superfluid's token contract
 * @notice The token contract that can upgrade any ERC20 token with realtime
 *         finance capabilities, while being ERC20 compatible itself.
 * @author Superfluid
 */
contract SuperToken is ISuperToken, ERC20Base {

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

    /// @notice Register or update a agreement
    /// @param account User that is part of this aggrement
    /// @param newState State of agreement that will be register
    function updateState
    (
        address account,
        bytes calldata newState
    )
    external
    override
    {
        //here the sender is the contract that implements the ISuperAgreement
        //TODO: validate the approved implementations of agreements

        require(msg.sender != account, "Use the agreement contract");
        //is a update or a new aggrement?
        if (_dataAgreements[msg.sender][account].length == 0) {
            //register relation
            _userToAgreements[account].push(msg.sender);
        }

        _dataAgreements[msg.sender][account] = newState;
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
            _agreeBalances += ISuperAgreement(_userToAgreements[account][0])
                .balanceOf(
                    _dataAgreements[_userToAgreements[account][0]][account],
                    block.timestamp
                );

        }

        return int256(_balances[account]) + _agreeBalances;
    }
}
