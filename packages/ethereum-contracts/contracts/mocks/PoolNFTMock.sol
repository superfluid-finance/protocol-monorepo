// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity 0.8.19;

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { PoolAdminNFT } from "../superfluid/PoolAdminNFT.sol";
import { PoolMemberNFT } from "../superfluid/PoolMemberNFT.sol";

contract PoolAdminNFTMock is PoolAdminNFT {
    constructor(ISuperfluid host) PoolAdminNFT(host) { }

    /// @dev a mock mint function that exposes the internal _mint function
    function mockMint(address _pool) public {
        _mint(_pool);
    }

    /// @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }

    /// @dev This exposes the _tokenApprovals storage without the requireMinted call
    function mockGetApproved(uint256 _tokenId) public view returns (address) {
        return _tokenApprovals[_tokenId];
    }
}

contract PoolMemberNFTMock is PoolMemberNFT {

    constructor(ISuperfluid host) PoolMemberNFT(host) { }

    /// @dev a mock mint function that exposes the internal _mint function
    function mockMint(address _pool, address _member) public {
        _mint(_pool, _member);
    }

    /// @dev a mock burn function that exposes the internal _burn function
    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    /// @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }

    /// @dev This exposes the _tokenApprovals storage without the requireMinted call
    function mockGetApproved(uint256 _tokenId) public view returns (address) {
        return _tokenApprovals[_tokenId];
    }
}