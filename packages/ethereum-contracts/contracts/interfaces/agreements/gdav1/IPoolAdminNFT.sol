// SPDX-License-Identifier: MIT
pragma solidity >=0.8.11;

import { IPoolNFTBase } from "./IPoolNFTBase.sol";

interface IPoolAdminNFT is IPoolNFTBase {
    // PoolAdminNFTData struct storage packing:
    // b = bits
    // WORD 1: | pool   | FREE
    //         | 160b   | 96b
    // WORD 2: | admin  | FREE
    //         | 160b   | 96b
    struct PoolAdminNFTData {
        address pool;
        address admin;
    }

    /// Write Functions ///
    function mint(address pool) external;

    function poolAdminDataByTokenId(uint256 tokenId) external view returns (PoolAdminNFTData memory data);
}