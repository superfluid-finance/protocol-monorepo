pragma solidity 0.5.17;

import "./ISuperToken.sol";

/**
 * @title Superfluid's token contract
 * @notice 
 * @author Superfluid
 */
contract SuperToken is ISuperToken {
    
    struct AccountStates {
        mapping(address => bytes) agreementStateMap;
    }

    mapping(address => AccountStates) private accountStatesMap;

    function getState(
        address agreementClass,
        address account) external view
        returns (bytes memory state) {
        AccountStates storage accountStates = accountStatesMap[account];
        return accountStates.agreementStateMap[msg.sender];
    }

    function updateState(
        address agreementClass,
        address account,
        bytes calldata newState) external {
        AccountStates storage accountStates = accountStatesMap[account];
        accountStates.agreementStateMap[msg.sender] = newState;
    }

}
