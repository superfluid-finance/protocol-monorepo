pragma solidity >=0.6.2 <0.7.0;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC777/IERC777.sol";

interface IWrapped777 is IERC777 {
  function token() external view returns (ERC20);

  function wrap(uint256 amount) external returns (uint256);
  function wrapTo(uint256 amount, address recipient) external returns (uint256);
  function gulp(address recipient) external returns (uint256);

  function unwrap(uint256 amount) external returns (uint256 unwrappedAmount);
  function unwrapTo(uint256 amount, address recipient) external returns (uint256 unwrappedAmount);
}
