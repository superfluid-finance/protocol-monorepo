// SPDX-License-Identifier: MIT
pragma solidity >=0.8.4;

import { IPoolNFTBase } from "./IPoolNFTBase.sol";

interface IPoolMemberNFT is IPoolNFTBase {
    // PoolMemberNFTData struct storage packing:
    // b = bits
    // WORD 1: | pool   | FREE
    //         | 160b   | 96b
    // WORD 2: | member | FREE
    //         | 160b   | 96b
    // WORD 3: | units  | FREE
    //         | 128b   | 128b
    struct PoolMemberNFTData {
        address pool;
        address member;
        uint128 units;
    }

    /// Errors ///

    error POOL_MEMBER_NFT_NO_ZERO_POOL();
    error POOL_MEMBER_NFT_NO_ZERO_MEMBER();
    error POOL_MEMBER_NFT_TRANSFER_NOT_ALLOWED();
    error POOL_MEMBER_NFT_NO_UNITS();
    error POOL_MEMBER_NFT_HAS_UNITS();

    function mint(address pool, address member) external;

    function update(uint256 tokenId) external;

    function burn(uint256 tokenId) external;

    /// View Functions ///

    function getTokenId(address pool, address member) external view returns (uint256 tokenId);

    function poolMemberDataByTokenId(uint256 tokenId) external view returns (PoolMemberNFTData memory data);
}
