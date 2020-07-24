// SPDX-License-Identifier: MIT
pragma solidity >= 0.6.0;

import "./ISuperToken.sol";
import "./ISuperfluidGovernance.sol";

/**
 * @title Superfluid's Registry interface
 * @author Superfluid
 */
interface ISuperfluidRegistry {

    function getERC20Wrapper(
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    returns (address wrapperAddress, bool created);

    function createERC20Wrapper(
        string calldata name,
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    returns (ISuperToken);

    function getGovernance() external returns (ISuperfluidGovernance);
    function getSuperTokenLogic() external returns (ISuperToken);

}
