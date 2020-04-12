pragma solidity >= 0.5.0;

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
            address agreementClass,
            address account,
            bytes calldata newState) external;

    function upgrade(uint256 amount) external;

}
