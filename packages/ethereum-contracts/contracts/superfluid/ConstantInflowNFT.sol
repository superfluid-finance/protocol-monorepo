// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantOutflowNFT
} from "../interfaces/superfluid/IConstantOutflowNFT.sol";
import {
    IConstantInflowNFT
} from "../interfaces/superfluid/IConstantInflowNFT.sol";
import { CFAv1NFTBase } from "./CFAv1NFTBase.sol";

/// @note TODO: clean up the inheritance with IConstantInflowNFT and CFAv1Base
// solhint-disable no-empty-blocks
// solhint-disable no-unused-vars

/// @title ConstantInflowNFT Contract (CIF NFT)
/// @author Superfluid
/// @notice The ConstantInflowNFT contract to be minted to the flow sender on flow creation.
/// @dev This contract does not hold any storage, but references the ConstantOutflowNFT contract storage.
contract ConstantInflowNFT is CFAv1NFTBase {
    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantInflowNFT.implementation"
            );
    }

    /// @notice The mint function emits the "mint" `Transfer` event.
    /// @dev We don't modify storage as this is handled in ConstantOutflowNFT.sol and this function's sole purpose
    /// is to inform clients that search for events.
    /// @param _to the receiver of the inflow nft and desired flow receiver
    /// @param _newTokenId the new token id
    function mint(address _to, uint256 _newTokenId) external {
        _mint(_to, _newTokenId);
    }

    /// @notice This burn function emits the "burn" `Transfer` event.
    /// @dev We don't modify storage as this is handled in ConstantOutflowNFT.sol and this function's sole purpose
    /// is to inform clients that search for events.
    /// @param _tokenId desired token id to burn
    function burn(uint256 _tokenId) external {
        _burn(_tokenId);
    }

    function flowDataByTokenId(
        uint256 _tokenId
    ) public view override returns (FlowData memory flowData) {
        IConstantOutflowNFT constantOutflowNFT = superToken
            .constantOutflowNFT();
        flowData = constantOutflowNFT.flowDataByTokenId(_tokenId);
    }

    function _safeTransfer(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes memory // _data
    ) internal virtual override {
        _transfer(_from, _to, _tokenId);
        // TODO
        // require(_checkOnERC721Received(from, to, tokenId, data),
        // "ERC721: transfer to non ERC721Receiver implementer");
    }

    /// @inheritdoc CFAv1NFTBase
    function _ownerOf(
        uint256 _tokenId
    ) internal view virtual override returns (address) {
        FlowData memory flowData = flowDataByTokenId(_tokenId);
        return flowData.flowReceiver;
    }

    /// @notice Transfer is currently not allowed.
    /// @dev Will revert currently.
    function _transfer(
        address, // _from,
        address, // _to,
        uint256 // _tokenId
    ) internal virtual override {
        revert CFA_NFT_TRANSFER_IS_NOT_ALLOWED();
    }

    function _mint(address _to, uint256 _newTokenId) internal {
        emit Transfer(address(0), _to, _newTokenId);
    }

    function _burn(uint256 _tokenId) internal {
        FlowData memory flowData = flowDataByTokenId(_tokenId);
        emit Transfer(flowData.flowReceiver, address(0), _tokenId);
    }
}
