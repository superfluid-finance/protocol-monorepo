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

    /*
    struct AccountStates {
        mapping(address => bytes) agreementStateMap;
    }

    mapping(address => AccountStates) private accountStatesMap;
    */

    //Agreements => User => Data
    mapping(address => mapping(address => bytes)) private _dataAgreements;
    mapping(address => address[]) private _userToAgreements;

    IERC20 private _token;

    constructor(IERC20 token, string memory name, string memory symbol) ERC20Base("SuperToken", "STK") public {
        _token = token;
    }

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
        if(_dataAgreements[msg.sender][account].length == 0) {
            //register relation
            _userToAgreements[account].push(msg.sender);
        }

        _dataAgreements[msg.sender][account] = newState;
    }

    function upgrade(uint256 amount) external override {
        address owner = msg.sender;
        _token.transferFrom(owner, address(this), amount);
       // _mint(owner, amount);
    }

    function balanceOf(address account) public view override returns (int256) {

        //query each agreement contract
        int256 _agreeBalances;

        for(uint256 i = 0; i < _userToAgreements[account].length; i++) {
            _agreeBalances += ISuperAgreement(
                _userToAgreements[account][i]
            ).balanceOf(
                _dataAgreements[_userToAgreements[account][i]][msg.sender],
                block.timestamp
            );
        }

        return int256(_balances[account]) + _agreeBalances;
    }
}
