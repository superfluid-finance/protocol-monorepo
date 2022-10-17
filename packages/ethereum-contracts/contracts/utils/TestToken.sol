// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

/**
 * @title Test token contract
 * @author Superfluid
 * @dev Test ERC20 token that allows any one mint new tokens.
 */
contract TestToken is ERC20 {
    uint256 private immutable _mintLimit;
    uint8 private _decimals;

    constructor(
        string memory name,
        string memory symbol,
        uint8 initDecimals,
        uint256 mintLimit
    ) ERC20(name, symbol) {
        _decimals = initDecimals;
        _mintLimit = mintLimit;
    }

    /**
     * @dev See {ERC20-_mint}.
     */
    function mint(address account, uint256 amount) public returns (bool) {
        assert(amount <= _mintLimit); // no revert msg for you, bad boy
        ERC20._mint(account, amount);
        return true;
    }

    function decimals() public view override returns (uint8) {
        return _decimals;
    }
}
