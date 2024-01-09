// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity 0.8.19;

import { PoolNFTBase } from "../agreements/gdav1/PoolNFTBase.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { PoolMemberNFT } from "../agreements/gdav1/PoolMemberNFT.sol";
import { PoolAdminNFT } from "../agreements/gdav1/PoolAdminNFT.sol";
import { IStorageLayoutBase } from "./IStorageLayoutBase.sol";

contract PoolNFTBaseStorageLayoutMock is PoolNFTBase, IStorageLayoutBase {
    constructor(ISuperfluid host) PoolNFTBase(host) { }

    function validateStorageLayout() public virtual {
        uint256 slot;
        uint256 offset;

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");

        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");

        assembly { slot := _reserve5.slot offset := _reserve5.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_reserve5");

        assembly { slot := _reserve21.slot offset := _reserve21.offset }
        if (slot != 21 || offset != 0) revert STORAGE_LOCATION_CHANGED("_reserve21");
    }

    // Dummy implementations for abstract functions
    function _ownerOf(
        uint256 //tokenId
    ) internal pure override returns (address) {
        return address(0);
    }

    function getTokenId(address /*pool*/, address /*account*/) external pure override returns (uint256 tokenId) {
        return 0;
    }

    function _transfer(
        address, //from,
        address, //to,
        uint256 //tokenId
    ) internal pure override {
        return;
    }

    function _safeTransfer(
        address from,
        address to,
        uint256 tokenId,
        bytes memory // data
    ) internal pure override {
        _transfer(from, to, tokenId);
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("");
    }

    function tokenURI(uint256 /*tokenId*/) external pure override returns (string memory) {
        return "";
    }
}

contract PoolAdminNFTStorageLayoutMock is PoolAdminNFT, IStorageLayoutBase {
    constructor(ISuperfluid host) PoolAdminNFT(host) { }

    function validateStorageLayout() public virtual {
        uint256 slot;
        uint256 offset;

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");

        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");

        assembly { slot := _reserve5.slot offset := _reserve5.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_reserve5");

        assembly { slot := _reserve21.slot offset := _reserve21.offset }
        if (slot != 21 || offset != 0) revert STORAGE_LOCATION_CHANGED("_reserve21");

        assembly { slot := _poolAdminDataByTokenId.slot offset := _poolAdminDataByTokenId.offset }
        if (slot != 22 || offset != 0) revert STORAGE_LOCATION_CHANGED("_poolAdminDataByTokenId");
    }

    // Dummy implementations for abstract functions
    function _safeTransfer(
        address from,
        address to,
        uint256 tokenId,
        bytes memory // data
    ) internal pure override {
        _transfer(from, to, tokenId);
    }
}

contract PoolMemberNFTStorageLayoutMock is PoolMemberNFT, IStorageLayoutBase {
    constructor(ISuperfluid host) PoolMemberNFT(host) { }

    function validateStorageLayout() public virtual {
        uint256 slot;
        uint256 offset;

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");

        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");

        assembly { slot := _reserve5.slot offset := _reserve5.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_reserve5");

        assembly { slot := _reserve21.slot offset := _reserve21.offset }
        if (slot != 21 || offset != 0) revert STORAGE_LOCATION_CHANGED("_reserve21");

        assembly { slot := _poolMemberDataByTokenId.slot offset := _poolMemberDataByTokenId.offset }
        if (slot != 22 || offset != 0) revert STORAGE_LOCATION_CHANGED("_poolMemberDataByTokenId");
    }

    // Dummy implementations for abstract functions
    function _safeTransfer(
        address from,
        address to,
        uint256 tokenId,
        bytes memory // data
    ) internal pure override {
        _transfer(from, to, tokenId);
    }
}
