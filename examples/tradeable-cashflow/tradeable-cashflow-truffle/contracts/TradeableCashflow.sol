// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import {RedirectAll, ISuperToken, ISuperfluid} from "./RedirectAll.sol";
import {ERC721} from "@openzeppelin/contracts/token/ERC721/ERC721.sol";

contract TradeableCashflow is ERC721, RedirectAll {

  constructor (
    address owner,
    string memory _name,
    string memory _symbol,
    ISuperfluid host,
    ISuperToken acceptedToken
  )
    ERC721 ( _name, _symbol )
    RedirectAll (
      host,
      acceptedToken,
      owner
     )
  {

      _mint(owner, 1);
  }

  //now I will insert a nice little hook in the _transfer, including the RedirectAll function I need
  function _beforeTokenTransfer(
    address /*from*/,
    address to,
    uint256 /*tokenId*/
  ) internal override {
      _changeReceiver(to);
  }
}
