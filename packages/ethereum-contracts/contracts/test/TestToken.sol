// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

/**
 * @dev Test ERC20 token that allows any one mint new tokens.
 */
contract TestToken is ERC20 {

    constructor(string memory name, string memory symbol, uint8 decimals)
        ERC20(name, symbol)
    {
        _setupDecimals(decimals);
    }

    /**
     * @dev See {ERC20-_mint}.
     */
    function mint(address account, uint256 amount) public returns (bool) {
        ERC20._mint(account, amount);
        return true;
    }

}
