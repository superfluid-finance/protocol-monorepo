// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity 0.8.19;

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";

contract ConstantOutflowNFTMock is ConstantOutflowNFT {
    constructor(ISuperfluid host, IConstantInflowNFT constantInflowNFT) ConstantOutflowNFT(host, constantInflowNFT) { }

    /// @dev a mock mint function that exposes the internal _mint function
    function mockMint(address _superToken, address _to, address _flowReceiver, uint256 _newTokenId) public {
        _mint(_superToken, _to, _flowReceiver, _newTokenId);
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

contract ConstantInflowNFTMock is ConstantInflowNFT {
    constructor(
        ISuperfluid host,
        IConstantOutflowNFT constantOutflowNFT
    ) ConstantInflowNFT(host, constantOutflowNFT) { }

    /// @dev a mock mint function to emit the mint Transfer event
    function mockMint(address _to, uint256 _newTokenId) public {
        _mint(_to, _newTokenId);
    }

    /// @dev a mock burn function to emit the burn Transfer event
    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    // @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }

    /// @dev This exposes the _tokenApprovals storage without the requireMinted call
    function mockGetApproved(uint256 _tokenId) public view returns (address) {
        return _tokenApprovals[_tokenId];
    }
}
