// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

/**
 * @title Test token contract
 * @author Superfluid
 * @dev Test ERC20 token that allows any one mint new tokens.
 */
contract TestToken is ERC20 {

    uint256 public constant MINT_LIMIT = 1e12 ether;
    uint8 private _decimals;

    constructor(string memory name, string memory symbol, uint8 initDecimals)
        ERC20(name, symbol)
    {
        _decimals = initDecimals;
    }

    /**
     * @dev See {ERC20-_mint}.
     */
    function mint(address account, uint256 amount) public returns (bool) {
        require(amount <= MINT_LIMIT, "Don't mint too many");
        ERC20._mint(account, amount);
        return true;
    }

    function decimals() public view override returns (uint8) {
        return _decimals;
    }

}
