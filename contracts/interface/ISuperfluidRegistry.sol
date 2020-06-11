pragma solidity >= 0.6.0;

import "./ISuperToken.sol";
import "./ISuperfluidGovernance.sol";

/**
 * @title Superfluid's Registry interface
 * @author Superfluid
 */
interface ISuperfluidRegistry {

    function createSuperToken(
        ISuperToken underlying
    )
    external
    returns(address);

    function getGovernance(
        ISuperToken underlying
    )
    external
    returns (ISuperfluidGovernance);
}
