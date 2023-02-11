// SPDX-License-Identifier: AGPLv3
// solhint-disable not-rely-on-time
pragma solidity 0.8.18;

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantInflowNFT
} from "../interfaces/superfluid/IConstantInflowNFT.sol";
import {
    IConstantOutflowNFT
} from "../interfaces/superfluid/IConstantOutflowNFT.sol";
import { CFAv1NFTBase } from "./CFAv1NFTBase.sol";

/// @title ConstantOutflowNFT contract (COF NFT)
/// @author Superfluid
/// @notice The ConstantOutflowNFT contract to be minted to the flow sender on flow creation.
/// @dev This contract uses mint/burn interface for flow creation/deletion and holds the actual storage for both NFTs.
contract ConstantOutflowNFT is CFAv1NFTBase {
    /// @notice A mapping from token id to CFAv1NFTFlowData = { address sender, uint32 flowStartDate, address receiver}
    /// @dev The token id is uint256(keccak256(abi.encode(flowSender, flowReceiver)))
    mapping(uint256 => CFAv1NFTFlowData) internal _flowDataByTokenId;

    error COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME(); // 0x0d1d1161
    error COF_NFT_MINT_TO_ZERO_ADDRESS();           // 0x43d05e51
    error COF_NFT_ONLY_CONSTANT_INFLOW();           // 0xa495a718
    error COF_NFT_ONLY_CFA();                       // 0x054fae59
    error COF_NFT_OVERFLOW();                       // 0xb398aeb1
    error COF_NFT_TOKEN_ALREADY_EXISTS();           // 0xe2480183

    // note that this is used so we don't upgrade to wrong logic contract
    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }

    /// @notice An external function for querying flow data by `tokenId``
    /// @param tokenId the token id
    /// @return flowData the flow data associated with `tokenId`
    function flowDataByTokenId(
        uint256 tokenId
    ) public view override returns (CFAv1NFTFlowData memory flowData) {
        flowData = _flowDataByTokenId[tokenId];
    }

    /// @notice Hook called by CFA contract on flow creation
    /// @dev This function mints the COF NFT to the flow sender and mints the CIF NFT to the flow receiver
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    function onCreate(
        address flowSender,
        address flowReceiver
    ) external onlyCFAv1 {
        uint256 newTokenId = _getTokenId(flowSender, flowReceiver);
        _mint(flowSender, flowReceiver, newTokenId);

        IConstantInflowNFT constantInflowNFT = superToken.constantInflowNFT();
        constantInflowNFT.mint(flowReceiver, newTokenId);
    }

    /// @notice Hook called by CFA contract on flow update
    /// @dev This function triggers the metadata update of both COF and CIF NFTs
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    function onUpdate(
        address flowSender,
        address flowReceiver
    ) external onlyCFAv1 {
        uint256 tokenId = _getTokenId(flowSender, flowReceiver);

        _triggerMetadataUpdate(tokenId);

        IConstantInflowNFT constantInflowNFT = superToken.constantInflowNFT();
        constantInflowNFT.triggerMetadataUpdate(tokenId);
    }

    /// @notice Hook called by CFA contract on flow deletion
    /// @dev This function burns the COF NFT and burns the CIF NFT
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    function onDelete(
        address flowSender,
        address flowReceiver
    ) external onlyCFAv1 {
        uint256 tokenId = _getTokenId(flowSender, flowReceiver);
        // must "burn" inflow NFT first because we clear storage when burning outflow NFT
        IConstantInflowNFT constantInflowNFT = superToken.constantInflowNFT();
        constantInflowNFT.burn(tokenId);

        _burn(tokenId);
    }

    /// @notice Handles the mint of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param to the receiver of the newly minted token
    /// @param flowReceiver the flow receiver (owner of the InflowNFT)
    /// @param newTokenId the new token id to be minted when an inflowNFT is minted
    function inflowTransferMint(
        address to,
        address flowReceiver,
        uint256 newTokenId
    ) external onlyConstantInflowNFT {
        _mint(to, flowReceiver, newTokenId);
    }

    /// @notice Handles the burn of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param tokenId the token id to burn when an inflow NFT is transferred
    function inflowTransferBurn(
        uint256 tokenId
    ) external onlyConstantInflowNFT {
        _burn(tokenId);
    }

    function _safeTransfer(
        address from,
        address to,
        uint256 tokenId,
        bytes memory // data
    ) internal virtual override {
        _transfer(from, to, tokenId);
    }

    /// @inheritdoc CFAv1NFTBase
    function _ownerOf(
        uint256 tokenId
    ) internal view virtual override returns (address) {
        return _flowDataByTokenId[tokenId].flowSender;
    }

    /// @notice Reverts - Transfer of outflow NFT is not allowed.
    /// @dev We revert when users attempt to transfer outflow NFTs.
    function _transfer(
        address, // from,
        address, // to,
        uint256 // tokenId
    ) internal virtual override {
        revert CFA_NFT_TRANSFER_IS_NOT_ALLOWED();
    }

    /// @notice Mints `newTokenId` and transfers it to `to`
    /// @dev `newTokenId` must not exist `to` cannot be `address(0)` and we emit a {Transfer} event.
    /// `to` cannot be equal to `flowReceiver`.
    /// @param to the receiver of the newly minted outflow nft (flow sender)
    /// @param flowReceiver the flow receiver (owner of the InflowNFT)
    /// @param newTokenId the new token id to be minted
    function _mint(
        address to,
        address flowReceiver,
        uint256 newTokenId
    ) internal {
        if (to == address(0)) {
            revert COF_NFT_MINT_TO_ZERO_ADDRESS();
        }

        if (to == flowReceiver) {
            revert COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME();
        }

        if (_exists(newTokenId)) {
            revert COF_NFT_TOKEN_ALREADY_EXISTS();
        }
        if (block.timestamp != uint256(uint32(block.timestamp))) {
            revert COF_NFT_OVERFLOW();
        }

        // update mapping for new NFT to be minted
        _flowDataByTokenId[newTokenId] = CFAv1NFTFlowData(
            to,
            uint32(block.timestamp),
            flowReceiver
        );

        // emit mint of new outflow token with newTokenId
        emit Transfer(address(0), to, newTokenId);
    }

    /// @notice Destroys token with `tokenId` and clears approvals from previous owner.
    /// @dev `tokenId` must exist AND we emit a {Transfer} event
    /// @param tokenId the id of the token we are destroying
    function _burn(uint256 tokenId) internal {
        address owner = CFAv1NFTBase.ownerOf(tokenId);

        // clear approvals from the previous owner
        delete _tokenApprovals[tokenId];

        // remove previous tokenId flow data mapping
        delete _flowDataByTokenId[tokenId];

        // emit burn of outflow token with tokenId
        emit Transfer(owner, address(0), tokenId);
    }

    modifier onlyConstantInflowNFT() {
        address constantInflowNFT = address(superToken.constantInflowNFT());
        if (msg.sender != constantInflowNFT) {
            revert COF_NFT_ONLY_CONSTANT_INFLOW();
        }
        _;
    }

    modifier onlyCFAv1() {
        if (msg.sender != address(cfaV1)) {
            revert COF_NFT_ONLY_CFA();
        }
        _;
    }
}
