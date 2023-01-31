// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import {
    IERC721Metadata
} from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";

import { CFAv1NFTBase } from "../../superfluid/CFAv1NFTBase.sol";

interface IConstantOutflowNFT is IERC721Metadata {
    /**************************************************************************
     * Errors
     *************************************************************************/
    error COF_NFT_MINT_TO_ZERO_ADDRESS(); // 0x43d05e51
    error COF_NFT_TOKEN_ALREADY_EXISTS(); // 0xe2480183

    /**************************************************************************
     * View Functions
     *************************************************************************/

    /// @notice An external function for querying flow data by `_tokenId``
    /// @param _tokenId the token id
    /// @return flowData the flow data associated with `_tokenId`
    function flowDataByTokenId(
        uint256 _tokenId
    ) external view returns (CFAv1NFTBase.FlowData memory flowData);

    /**************************************************************************
     * Write Functions
     *************************************************************************/
    
    function onCreate(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) external;

    function onUpdate(uint256 _tokenId) external;

    function onDelete(uint256 _tokenId) external;

    /// @notice The mint function creates a flow from `_from` to `_to`.
    /// @dev If `msg.sender` is not equal to `_from`, we `createFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT creation.
    /// @param _from desired flow sender
    /// @param _to desired flow receiver
    /// @param _flowRate desired flow rate
    function mint(address _from, address _to, int96 _flowRate) external;

    /// @notice The burn function deletes the flow between `sender` and `receiver` stored in `_tokenId`
    /// @dev If `msg.sender` is not equal to `_from`, we `deleteFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT deletion.
    /// @param _tokenId desired token id to burn
    function burn(uint256 _tokenId) external;

    /// @notice Handles the mint of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param _to the receiver of the newly minted token
    /// @param _flowReceiver the flow receiver (owner of the InflowNFT)
    /// @param _newTokenId the new token id to be minted when an inflowNFT is minted
    function inflowTransferMint(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) external;

    /// @notice Handles the burn of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param _tokenId the token id to burn when an inflow NFT is transferred
    function inflowTransferBurn(uint256 _tokenId) external;
}
