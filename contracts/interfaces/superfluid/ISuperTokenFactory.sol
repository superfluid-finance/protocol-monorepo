// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import { ISuperToken } from "./ISuperToken.sol";

import {
    IERC20,
    ERC20WithTokenInfo
} from "../tokens/ERC20WithTokenInfo.sol";


interface ISuperTokenFactory {

    function initialize() external;

    /**
     * @dev Get the current super token logic shared by super tokens
     */
    function getSuperTokenLogic() external view returns (ISuperToken superToken);

    enum Upgradability {
        NON_UPGRADABLE,
        SEMI_UPGRADABLE,
        FULL_UPGRADABE
    }

    function createERC20Wrapper(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        external;

    function createERC20Wrapper(
        ERC20WithTokenInfo underlyingToken,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        external;

    event SuperTokenCreated(ISuperToken token);

}
