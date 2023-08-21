// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
// solhint-disable not-rely-on-time
pragma solidity 0.8.19;

import { Strings } from "@openzeppelin/contracts/utils/Strings.sol";
import { ISuperfluid, IConstantInflowNFT, IConstantOutflowNFT } from "../interfaces/superfluid/ISuperfluid.sol";
import { ConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";
import { FlowNFTBase } from "../superfluid/FlowNFTBase.sol";

/// @title FlowNFTBaseMock
/// @author Superfluid
/// @dev A mock contract for testing the functionality on FlowNFTBase
contract FlowNFTBaseMock is FlowNFTBase {
    using Strings for uint256;

    mapping(uint256 => FlowNFTData) internal _flowDataByTokenId;

    constructor(ISuperfluid host) FlowNFTBase(host) { }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.FlowNFTBaseMock.implementation");
    }

    /// @dev The owner of here is always the flow sender
    function _ownerOf(uint256 tokenId) internal view override returns (address) {
        return _flowDataByTokenId[tokenId].flowSender;
    }

    /// @dev a mock mint function that sets the FlowNFTData
    function mockMint(address _superToken, address _flowSender, address _flowReceiver) public {
        uint256 tokenId = _getTokenId(_superToken, _flowSender, _flowReceiver);
        _flowDataByTokenId[tokenId] = FlowNFTData({
            flowSender: _flowSender,
            flowStartDate: uint32(block.timestamp),
            flowReceiver: _flowReceiver,
            superToken: _superToken
        });
    }

    function _transfer(
        address, //from,
        address, //to,
        uint256 //tokenId
    ) internal pure override {
        revert CFA_NFT_TRANSFER_IS_NOT_ALLOWED();
    }

    function flowDataByTokenId(uint256 tokenId) public view override returns (FlowNFTData memory flowData) {
        return _flowDataByTokenId[tokenId];
    }

    function tokenURI(uint256 tokenId) external pure override returns (string memory) {
        return string(abi.encodePacked("tokenId=", tokenId.toString()));
    }
}

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