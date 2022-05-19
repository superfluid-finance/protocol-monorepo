 //SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import {RedirectAllCallOption, ISuperToken, IConstantFlowAgreementV1, ISuperfluid} from "./RedirectAllCallOption.sol";

import {ERC721} from "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import {ERC20} from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract TradeableCashflowOption is ERC721, RedirectAllCallOption {

  constructor (
    address owner,
    string memory _name,
    string memory _symbol,
    ISuperfluid host,
    IConstantFlowAgreementV1 cfa,
    ISuperToken acceptedToken,
    ERC20 dai
  )
    ERC721 ( _name, _symbol )
    RedirectAllCallOption (
      host,
      cfa,
      acceptedToken,
      dai,
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