// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { IPoolAdminNFT } from "../interfaces/superfluid/IPoolAdminNFT.sol";
import { PoolNFTBase } from "./PoolNFTBase.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";

contract PoolAdminNFT is PoolNFTBase, IPoolAdminNFT {
    //// Storage Variables ////

    /// NOTE: The storage variables in this contract MUST NOT:
    /// - change the ordering of the existing variables
    /// - change any of the variable types
    /// - rename any of the existing variables
    /// - remove any of the existing variables

    /// @notice A mapping from token id to FlowNFTData
    /// PoolAdminNFTData: { address pool, address admin }
    /// @dev The token id is uint256(keccak256(abi.encode(pool, admin)))
    mapping(uint256 => PoolAdminNFTData) internal _poolAdminDataByTokenId;

    constructor(ISuperfluid host) PoolNFTBase(host) { }

    // note that this is used so we don't upgrade to wrong logic contract
    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.PoolAdminNFT.implementation");
    }

    function _ownerOf(uint256 tokenId) internal view virtual override returns (address) {
        return _poolAdminDataByTokenId[tokenId].admin;
    }

    /// @notice Reverts - Transfer of pool member NFT is not allowed.
    /// @dev We revert when users attempt to transfer pool member NFTs.
    function _transfer(
        address, // from,
        address, // to,
        uint256 // tokenId
    ) internal virtual override {
        revert POOL_ADMIN_NFT_TRANSFER_NOT_ALLOWED();
    }

    function getTokenId(address pool, address admin) external view returns (uint256 tokenId) {
        return _getTokenId(pool, admin);
    }

    function _getTokenId(address pool, address admin) internal view returns (uint256 tokenId) {
        return uint256(keccak256(abi.encode(pool, admin)));
    }

    /// @inheritdoc PoolNFTBase
    function tokenURI(uint256 tokenId) external view override(IERC721Metadata, PoolNFTBase) returns (string memory) {
        return super._tokenURI(tokenId);
    }

    function mint(address pool, address admin) external {
        _mint(pool, admin);
    }

    /// @notice Mints `newTokenId` and transfers it to `flowSender`
    /// @dev `newTokenId` must not exist `flowSender` cannot be `address(0)` and we emit a {Transfer} event.
    /// `flowSender` cannot be equal to `flowReceiver`.
    /// @param pool The pool address
    /// @param admin The admin address
    function _mint(address pool, address admin) internal {
        // @note we can even remove admin and get the pool in here and mint ot admin
        assert(pool != address(0));
        assert(admin != address(0));
        assert(pool != admin);
        uint256 newTokenId = _getTokenId(pool, admin);
        assert(!_exists(newTokenId));

        // update mapping for new NFT to be minted
        _poolAdminDataByTokenId[newTokenId] = PoolAdminNFTData(pool, admin);

        // emit mint of new pool admin token with newTokenId
        emit Transfer(address(0), admin, newTokenId);
    }
}
