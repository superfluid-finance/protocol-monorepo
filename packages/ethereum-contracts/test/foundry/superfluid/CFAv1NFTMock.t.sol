// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IConstantFlowAgreementV1 } from "../../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";

contract ConstantOutflowNFTMock is ConstantOutflowNFT {
    constructor(IConstantFlowAgreementV1 _cfaV1) ConstantOutflowNFT(_cfaV1) {}

    /// @dev a mock mint function that exposes the internal _mint function
    function mockMint(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) public {
        _mint(_to, _flowReceiver, _newTokenId);
    }

    /// @dev a mock burn function that exposes the internal _burn function
    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    /// @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }
}

contract ConstantInflowNFTMock is ConstantInflowNFT {
    constructor(IConstantFlowAgreementV1 _cfaV1) ConstantInflowNFT(_cfaV1) {}

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

    /// @dev this exposes the internal flow data by token id for testing purposes
    function mockFlowNFTDataByTokenId(
        uint256 _tokenId
    ) public view returns (FlowNFTData memory flowData) {
        return flowDataByTokenId(_tokenId);
    }
}
