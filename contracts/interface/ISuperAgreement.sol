pragma solidity 0.6.6;

/**
 * @title Superfluid's agreement interface
 * @author Superfluid
 */
interface ISuperAgreement {

    function balanceOf(bytes calldata state, uint256 time)
        external
        pure
        returns (int256 amount);

    function encodeFlow(uint256 timestamp, int256 flowRate) external pure returns(bytes memory state);

    function decodeFlow(bytes calldata state) external pure returns(uint256, int256);

    function touch(bytes calldata currentAgreement, uint256 timestamp)
        external
        pure
        returns(bytes memory newState);
}
