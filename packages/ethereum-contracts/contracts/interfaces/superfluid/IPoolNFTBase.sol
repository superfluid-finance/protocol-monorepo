// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";

interface IPoolNFTBase is IERC721Metadata {
    error POOL_NFT_APPROVE_TO_CALLER();
    error POOL_NFT_ONLY_SUPER_TOKEN_FACTORY();
    error POOL_NFT_INVALID_TOKEN_ID();
    error POOL_NFT_APPROVE_TO_CURRENT_OWNER();
    error POOL_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
    error POOL_NFT_NOT_REGISTERED_POOL();
    error POOL_NFT_TRANSFER_NOT_ALLOWED();
    error POOL_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();

    /// @notice Informs third-party platforms that NFT metadata should be updated
    /// @dev This event comes from https://eips.ethereum.org/EIPS/eip-4906
    /// @param tokenId the id of the token that should have its metadata updated
    event MetadataUpdate(uint256 tokenId);

    function initialize(string memory nftName, string memory nftSymbol) external; // initializer;

    function triggerMetadataUpdate(uint256 tokenId) external;

    /// @notice Gets the token id
    /// @dev For PoolAdminNFT, `account` is admin and for PoolMemberNFT, `account` is member
    function getTokenId(address pool, address account) external view returns (uint256 tokenId);
}
