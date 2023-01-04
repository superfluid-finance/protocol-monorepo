// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;
// @note temporary
// solhint-disable no-empty-blocks
// solhint-disable no-unused-vars
// solhint-disable not-rely-on-time
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { CFANFTBase } from "./CFANFTBase.sol";

contract CFAOutflowNFT is CFANFTBase {
    constructor(
        ISuperToken _superToken,
        IConstantFlowAgreementV1 _cfa,
        string memory _name,
        string memory _symbol
    ) CFANFTBase(_superToken, _cfa, _name, _symbol) {}

    /// @notice Returns the owner of the outflowing flow with id `_tokenId`
    /// @dev `_tokenId` is the uint256 cast of the keccak256 hash of the sender and receiver addresses
    /// @param _tokenId uint256(keccak256(abi.encode(sender, receiver)))
    /// @return The owner of the outflowing flow NFT with id `_tokenId`
    function ownerOf(
        uint256 _tokenId
    ) external view override returns (address) {
        return _flowDataBySenderReceiver[bytes32(_tokenId)].sender;
    }

    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external override {
        // TODO: for outflow NFTs, we can allow a transfer to the _to address
        // IF and only IF the _from address has ACL permissions to create a flow
        // on behalf of the _to address
    }

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes calldata _data
    ) external override {
        // safe transfer of nft
    }

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external override {
        // safe transfer of nft
    }

    function tokenURI(
        uint256 _tokenId
    ) external view override returns (string memory) {
        // get token uri
    }

    function _mint(address _sender, address _receiver) internal override {
        unchecked {
            _balances[_sender] += 1;
        }
        _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _receiver))
        ] = FlowData(_sender, _receiver, uint64(block.timestamp));
    }

    function _burn(address _sender, address _receiver) internal override {
        unchecked {
            _balances[_sender] -= 1;
        }

        delete _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _receiver))
        ];
    }
}
