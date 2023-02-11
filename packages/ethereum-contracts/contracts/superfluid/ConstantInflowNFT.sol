// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantOutflowNFT
} from "../interfaces/superfluid/IConstantOutflowNFT.sol";
import {
    IConstantInflowNFT
} from "../interfaces/superfluid/IConstantInflowNFT.sol";
import { CFAv1NFTBase } from "./CFAv1NFTBase.sol";

/// @title ConstantInflowNFT Contract (CIF NFT)
/// @author Superfluid
/// @notice The ConstantInflowNFT contract to be minted to the flow sender on flow creation.
/// @dev This contract does not hold any storage, but references the ConstantOutflowNFT contract storage.
contract ConstantInflowNFT is CFAv1NFTBase {
    error CIF_NFT_ONLY_CONSTANT_OUTFLOW(); // 0xe81ef57a

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantInflowNFT.implementation"
            );
    }

    /// @notice The mint function emits the "mint" `Transfer` event.
    /// @dev We don't modify storage as this is handled in ConstantOutflowNFT.sol and this function's sole purpose
    /// is to inform clients that search for events.
    /// Only callable by ConstantOutflowNFT
    /// @param to the receiver of the inflow nft and desired flow receiver
    /// @param newTokenId the new token id
    function mint(
        address to,
        uint256 newTokenId
    ) external onlyConstantOutflowNFT {
        _mint(to, newTokenId);
    }

    /// @notice This burn function emits the "burn" `Transfer` event.
    /// @dev We don't modify storage as this is handled in ConstantOutflowNFT.sol and this function's sole purpose
    /// is to inform clients that search for events.
    /// Only callable by ConstantOutflowNFT
    /// @param tokenId desired token id to burn
    function burn(uint256 tokenId) external onlyConstantOutflowNFT {
        _burn(tokenId);
    }

    function flowDataByTokenId(
        uint256 tokenId
    ) public view override returns (CFAv1NFTFlowData memory flowData) {
        IConstantOutflowNFT constantOutflowNFT = superToken
            .constantOutflowNFT();
        flowData = constantOutflowNFT.flowDataByTokenId(tokenId);
    }

    function _safeTransfer(
        address from,
        address to,
        uint256 tokenId,
        bytes memory // data
    ) internal virtual override {
        _transfer(from, to, tokenId);
        // TODO
        // require(_checkOnERC721Received(from, to, tokenId, data),
        // "ERC721: transfer to non ERC721Receiver implementer");
    }

    /// @inheritdoc CFAv1NFTBase
    function _ownerOf(
        uint256 tokenId
    ) internal view virtual override returns (address) {
        CFAv1NFTFlowData memory flowData = flowDataByTokenId(tokenId);
        return flowData.flowReceiver;
    }

    /// @notice Transfer is currently not allowed.
    /// @dev Will revert currently.
    function _transfer(
        address, // from,
        address, // to,
        uint256 // tokenId
    ) internal virtual override {
        revert CFA_NFT_TRANSFER_IS_NOT_ALLOWED();
    }

    function _mint(address to, uint256 newTokenId) internal {
        emit Transfer(address(0), to, newTokenId);
    }

    function _burn(uint256 tokenId) internal {
        CFAv1NFTFlowData memory flowData = flowDataByTokenId(tokenId);
        emit Transfer(flowData.flowReceiver, address(0), tokenId);
    }

    modifier onlyConstantOutflowNFT() {
        address constantOutflowNFT = address(superToken.constantOutflowNFT());
        if (msg.sender != constantOutflowNFT) {
            revert CIF_NFT_ONLY_CONSTANT_OUTFLOW();
        }
        _;
    }
}
