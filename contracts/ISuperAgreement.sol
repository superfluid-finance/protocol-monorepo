pragma solidity >= 0.5.0;

import { ISuperToken } from "./ISuperToken.sol";

/**
 * @title Superfluid's agreement interface
 * @author Superfluid
 */
interface ISuperAgreement {

    function balanceOf(bytes calldata state, uint256 time)
        external pure
        returns (uint256 amount);

}
