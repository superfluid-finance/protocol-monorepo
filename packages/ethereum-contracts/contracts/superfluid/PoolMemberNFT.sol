// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { IPoolMemberNFT } from "../interfaces/superfluid/IPoolMemberNFT.sol";
import { PoolNFTBase } from "./PoolNFTBase.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperfluidPool } from "../interfaces/superfluid/ISuperfluidPool.sol";

contract PoolMemberNFT is PoolNFTBase, IPoolMemberNFT {
    //// Storage Variables ////

    /// NOTE: The storage variables in this contract MUST NOT:
    /// - change the ordering of the existing variables
    /// - change any of the variable types
    /// - rename any of the existing variables
    /// - remove any of the existing variables

    /// @notice A mapping from token id to FlowNFTData
    /// PoolMemberNFTData: { address pool, address member, uint128 units }
    /// @dev The token id is uint256(keccak256(abi.encode(pool, member)))
    mapping(uint256 => PoolMemberNFTData) internal _poolMemberDataByTokenId;

    constructor(ISuperfluid host) PoolNFTBase(host) { }

    // note that this is used so we don't upgrade to wrong logic contract
    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.PoolMemberNFT.implementation");
    }

    function _ownerOf(uint256 tokenId) internal view virtual override returns (address) {
        return _poolMemberDataByTokenId[tokenId].member;
    }

    /// @notice Reverts - Transfer of pool member NFT is not allowed.
    /// @dev We revert when users attempt to transfer pool member NFTs.
    function _transfer(
        address, // from,
        address, // to,
        uint256 // tokenId
    ) internal virtual override {
        revert POOL_MEMBER_NFT_TRANSFER_NOT_ALLOWED();
    }

    function getTokenId(address pool, address member) external view returns (uint256 tokenId) {
        return uint256(keccak256(abi.encode(pool, member)));
    }

    /// @inheritdoc PoolNFTBase
    function tokenURI(uint256 tokenId) external view override(IERC721Metadata, PoolNFTBase) returns (string memory) {
        return super._tokenURI(tokenId);
    }

    function mint(address pool, address member, uint128 units) external {
        _mint(pool, member, units, uint256(keccak256(abi.encode(pool, member))));
    }

    function burn(uint256 tokenId) external {
        _burn(tokenId);
    }

    /// @notice Mints `newTokenId` and transfers it to `flowSender`
    /// @dev `newTokenId` must not exist `flowSender` cannot be `address(0)` and we emit a {Transfer} event.
    /// `flowSender` cannot be equal to `flowReceiver`.
    /// @param pool The pool address
    /// @param member The member address
    /// @param units The number of units
    /// @param newTokenId The token id of the new token to be minted
    function _mint(address pool, address member, uint128 units, uint256 newTokenId) internal {
        assert(pool != address(0));
        assert(member != address(0));
        assert(pool != member);
        assert(!_exists(newTokenId));

        if (ISuperfluidPool(pool).getUnits(member) == 0) {
            revert POOL_MEMBER_NFT_NO_UNITS();
        }

        // update mapping for new NFT to be minted
        _poolMemberDataByTokenId[newTokenId] = PoolMemberNFTData(pool, member, units);

        // emit mint of new pool member token with newTokenId
        emit Transfer(address(0), member, newTokenId);
    }

    /// @notice Destroys token with `tokenId` and clears approvals from previous owner.
    /// @dev `tokenId` must exist AND we emit a {Transfer} event
    /// @param tokenId the id of the token we are destroying
    function _burn(uint256 tokenId) internal override {
        PoolMemberNFTData storage data = _poolMemberDataByTokenId[tokenId];
        if (ISuperfluidPool(data.pool).getUnits(data.member) > 0) {
            revert POOL_MEMBER_NFT_HAS_UNITS();
        }

        address owner = _ownerOf(tokenId);

        super._burn(tokenId);

        // remove previous tokenId flow data mapping
        delete _poolMemberDataByTokenId[tokenId];

        // emit burn of pool member token with tokenId
        emit Transfer(owner, address(0), tokenId);
    }
}
