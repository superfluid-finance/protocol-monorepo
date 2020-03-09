pragma solidity ^0.5.0;

import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

contract SimpleVault {

    using SafeMath for uint256;

    IERC20 public token;

    mapping(address => uint256) public balances;

    constructor(IERC20 token_) public {
        token = token_;
    }

    function deposit(uint amount) public returns (bool) {
        require(token.transferFrom(msg.sender, address(this), amount), "deposit failed during ERC20.transferFrom");
        balances[msg.sender] += amount;
    }

    function release(uint amount) public returns (bool) {
        require(balances[msg.sender] >= amount, "not enough balance");
        require(token.transfer(msg.sender, amount), "release failed during ERC20.transfer");
        balances[msg.sender] -= amount;
    }

}
