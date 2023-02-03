// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import {
    IERC721Metadata
} from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { ICFAv1NFTBase } from "./ICFAv1NFTBase.sol";

interface IConstantInflowNFT is IERC721Metadata, ICFAv1NFTBase {
    /**************************************************************************
     * Errors
     *************************************************************************/

    /**************************************************************************
     * View Functions
     *************************************************************************/

    /**************************************************************************
     * Write Functions
     *************************************************************************/

    /// @notice The mint function emits the "mint" `Transfer` event.
    /// @dev We don't modify storage as this is handled in ConstantOutflowNFT.sol and this function's sole purpose
    /// is to inform clients that search for events.
    /// @param _to the flow receiver (inflow NFT receiver)
    /// @param _newTokenId the new token id
    function mint(address _to, uint256 _newTokenId) external;

    /// @notice This burn function emits the "burn" `Transfer` event.
    /// @dev We don't modify storage as this is handled in ConstantOutflowNFT.sol and this function's sole purpose
    /// is to inform clients that search for events.
    /// @param _tokenId desired token id to burn
    function burn(uint256 _tokenId) external;

    function triggerMetadataUpdate(uint256 _tokenId) external;
}
