// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import {
    IERC721Metadata
} from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { ISuperToken } from "./ISuperToken.sol";
import "./ICFAv1NFTBase.sol";

interface IConstantOutflowNFT is IERC721Metadata {
    /**************************************************************************
     * Errors
     *************************************************************************/
    error COF_NFT_MINT_TO_ZERO_ADDRESS(); // 0x43d05e51
    error COF_NFT_TOKEN_ALREADY_EXISTS(); // 0xe2480183

    /**************************************************************************
     * View Functions
     *************************************************************************/

    /// @notice An external function for querying flow data by `tokenId``
    /// @param tokenId the token id
    /// @return flowData the flow data associated with `tokenId`
    function flowDataByTokenId(
        uint256 tokenId
    ) external view returns (ICFAv1NFTBase.CFAv1NFTFlowData memory flowData);

    /**************************************************************************
     * Write Functions
     *************************************************************************/

    function initialize(
        ISuperToken superToken,
        string memory nftName,
        string memory nftSymbol
    ) external; // initializer;
    
    function onCreate(
        address to,
        address flowReceiver,
        uint256 newTokenId
    ) external;

    function onUpdate(uint256 tokenId) external;

    function onDelete(uint256 tokenId) external;

    /// @notice The mint function creates a flow from `from` to `to`.
    /// @dev If `msg.sender` is not equal to `from`, we `createFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT creation.
    /// @param from desired flow sender
    /// @param to desired flow receiver
    /// @param flowRate desired flow rate
    function mint(address from, address to, int96 flowRate) external;

    /// @notice The burn function deletes the flow between `sender` and `receiver` stored in `tokenId`
    /// @dev If `msg.sender` is not equal to `from`, we `deleteFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT deletion.
    /// @param tokenId desired token id to burn
    function burn(uint256 tokenId) external;

    /// @notice Handles the mint of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param to the receiver of the newly minted token
    /// @param flowReceiver the flow receiver (owner of the InflowNFT)
    /// @param newTokenId the new token id to be minted when an inflowNFT is minted
    function inflowTransferMint(
        address to,
        address flowReceiver,
        uint256 newTokenId
    ) external;

    /// @notice Handles the burn of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param tokenId the token id to burn when an inflow NFT is transferred
    function inflowTransferBurn(uint256 tokenId) external;
}
