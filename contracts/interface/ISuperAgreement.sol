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

}
