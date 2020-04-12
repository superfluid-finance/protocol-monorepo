pragma solidity 0.6.6;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import { ISuperToken } from "./ISuperToken.sol";

/**
 * @title Superfluid's token contract
 * @notice The token contract that can upgrade any ERC20 token with realtime
 *         finance capabilities, while being ERC20 compatible itself.
 * @author Superfluid
 */
contract SuperToken is ISuperToken {

    struct AccountStates {
        mapping(address => bytes) agreementStateMap;
    }

    mapping(address => AccountStates) private accountStatesMap;

    IERC20 private token;

    constructor(IERC20 token_) public {
        token = token_;
    }

    function getState(
        address /*agreementClass*/,
        address account)
        external view override
        returns (bytes memory state) {
        AccountStates storage accountStates = accountStatesMap[account];
        return accountStates.agreementStateMap[msg.sender];
    }

    function updateState(
        address /*agreementClass*/,
        address account,
        bytes calldata newState)
        external override {
        AccountStates storage accountStates = accountStatesMap[account];
        accountStates.agreementStateMap[msg.sender] = newState;
    }

    function upgrade(uint256 amount) external override {
        address owner = msg.sender;
        token.transferFrom(owner, address(this), amount);
        //_mint(owner, amount);
    }

}
