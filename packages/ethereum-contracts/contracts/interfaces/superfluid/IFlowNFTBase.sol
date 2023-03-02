// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import {
    IERC721MetadataUpgradeable
} from "@openzeppelin/contracts-upgradeable/token/ERC721/extensions/IERC721MetadataUpgradeable.sol";
import { ISuperToken } from "./ISuperToken.sol";

interface IFlowNFTBase is IERC721MetadataUpgradeable {
    // FlowNFTData struct storage packing:
    // b = bits
    // WORD 1: | flowSender     | flowStartDate | FREE
    //         | 160b           | 32b           | 64b
    // WORD 2: | flowReceiver   | FREE
    //         | 160b           | 96b
    // @note Using 32 bits for flowStartDate is future proof "enough":
    // 2 ** 32 - 1 = 4294967295 seconds
    // Will overflow after: Sun Feb 07 2106 08:28:15
    struct FlowNFTData {
        address flowSender;
        uint32 flowStartDate;
        address flowReceiver;
    }

    function initialize(
        ISuperToken superToken,
        string memory nftName,
        string memory nftSymbol
    ) external; // initializer;

    /// @notice An external function for computing the deterministic tokenId
    /// @dev tokenId = uint256(keccak256(abi.encode(flowSender, flowReceiver)))
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    /// @return tokenId the tokenId
    function getTokenId(
        address flowSender,
        address flowReceiver
    ) external view returns (uint256);

    /**************************************************************************
     * Custom Errors
     *************************************************************************/

    error CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();   // 0xa3352582
    error CFA_NFT_APPROVE_TO_CALLER();                              // 0xd3c77329
    error CFA_NFT_APPROVE_TO_CURRENT_OWNER();                       // 0xe4790b25
    error CFA_NFT_INVALID_TOKEN_ID();                               // 0xeab95e3b
    error CFA_NFT_ONLY_HOST();                                      // 0x2d5a6dfa
    error CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();  // 0x2551d606
    error CFA_NFT_TRANSFER_FROM_INCORRECT_OWNER();                  // 0x5a26c744
    error CFA_NFT_TRANSFER_IS_NOT_ALLOWED();                        // 0xaa747eca
    error CFA_NFT_TRANSFER_TO_ZERO_ADDRESS();                       // 0xde06d21e

    /**************************************************************************
     * Events
     *************************************************************************/

    /// @notice Informs third-party platforms that NFT metadata should be updated
    /// @dev This event comes from https://eips.ethereum.org/EIPS/eip-4906
    /// @param tokenId the id of the token that should have its metadata updated
    event MetadataUpdate(uint256 tokenId);
}
