pragma solidity >= 0.6.6;

/**
 * @title Superfluid's token interface
 * @author Superfluid
 */
interface ISuperToken {

    function getState(
            address agreementClass,
            address account) external view
            returns (bytes memory state);

    function updateState(
            address account,
            bytes calldata newState) external;

    function upgrade(uint256 amount) external;

}
