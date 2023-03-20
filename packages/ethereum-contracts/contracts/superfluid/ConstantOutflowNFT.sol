// SPDX-License-Identifier: AGPLv3
// solhint-disable not-rely-on-time
pragma solidity 0.8.19;

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    IConstantInflowNFT
} from "../interfaces/superfluid/IConstantInflowNFT.sol";
import {
    IConstantOutflowNFT
} from "../interfaces/superfluid/IConstantOutflowNFT.sol";
import { FlowNFTBase, IFlowNFTBase } from "./FlowNFTBase.sol";

/// @title ConstantOutflowNFT contract (COF NFT)
/// @author Superfluid
/// @notice The ConstantOutflowNFT contract to be minted to the flow sender on flow creation.
/// @dev This contract uses mint/burn interface for flow creation/deletion and holds the actual storage for both NFTs.
contract ConstantOutflowNFT is FlowNFTBase, IConstantOutflowNFT {
    /// @notice A mapping from token id to FlowNFTData
    /// FlowNFTData: { address flowSender, uint32 flowStartDate, address flowReceiver}
    /// @dev The token id is uint256(keccak256(abi.encode(flowSender, flowReceiver)))
    mapping(uint256 => FlowNFTData) internal _flowDataByTokenId;

    /**************************************************************************
     * Custom Errors
     *************************************************************************/

    error COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME(); // 0x0d1d1161
    error COF_NFT_MINT_TO_ZERO_ADDRESS();           // 0x43d05e51
    error COF_NFT_ONLY_CONSTANT_INFLOW();           // 0xa495a718
    error COF_NFT_ONLY_CFA();                       // 0x054fae59
    error COF_NFT_TOKEN_ALREADY_EXISTS();           // 0xe2480183

    // solhint-disable-next-line no-empty-blocks
    constructor(IConstantFlowAgreementV1 _cfaV1) FlowNFTBase(_cfaV1) {}

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
    )
        public
        view
        override(FlowNFTBase, IFlowNFTBase)
        returns (FlowNFTData memory flowData)
    {
        flowData = _flowDataByTokenId[tokenId];
    }

    /// @notice Hook called by CFA contract on flow creation
    /// @dev This function mints the COF NFT to the flow sender and mints the CIF NFT to the flow receiver
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    /// NOTE: We do an existence check in here to determine whether or not to execute the hook
    function onCreate(
        address flowSender,
        address flowReceiver
    ) external onlyFlowAgreements {
        uint256 newTokenId = _getTokenId(flowSender, flowReceiver);
        if (_flowDataByTokenId[newTokenId].flowSender == address(0)) {
            _mint(flowSender, flowReceiver, newTokenId);

            IConstantInflowNFT constantInflowNFT = superToken
                .constantInflowNFT();
            constantInflowNFT.mint(flowReceiver, newTokenId);
        }
    }

    /// @notice Hook called by CFA contract on flow update
    /// @dev This function triggers the metadata update of both COF and CIF NFTs
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    /// NOTE: We do an existence check in here to determine whether or not to execute the hook
    function onUpdate(
        address flowSender,
        address flowReceiver
    ) external onlyFlowAgreements {
        uint256 tokenId = _getTokenId(flowSender, flowReceiver);
        if (_flowDataByTokenId[tokenId].flowSender != address(0)) {
            _triggerMetadataUpdate(tokenId);

            IConstantInflowNFT constantInflowNFT = superToken
                .constantInflowNFT();
            constantInflowNFT.triggerMetadataUpdate(tokenId);
        }
    }

    /// @notice Hook called by CFA contract on flow deletion
    /// @dev This function burns the COF NFT and burns the CIF NFT
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    /// NOTE: We do an existence check in here to determine whether or not to execute the hook
    function onDelete(
        address flowSender,
        address flowReceiver
    ) external onlyFlowAgreements {
        uint256 tokenId = _getTokenId(flowSender, flowReceiver);
        if (_flowDataByTokenId[tokenId].flowSender != address(0)) {
            // must "burn" inflow NFT first because we clear storage when burning outflow NFT
            IConstantInflowNFT constantInflowNFT = superToken
                .constantInflowNFT();
            constantInflowNFT.burn(tokenId);

            _burn(tokenId);
        }
    }

    /// @inheritdoc FlowNFTBase
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

        // update mapping for new NFT to be minted
        _flowDataByTokenId[newTokenId] = FlowNFTData(
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
    function _burn(uint256 tokenId) internal override {
        address owner = FlowNFTBase.ownerOf(tokenId);

        super._burn(tokenId);

        // remove previous tokenId flow data mapping
        delete _flowDataByTokenId[tokenId];

        // emit burn of outflow token with tokenId
        emit Transfer(owner, address(0), tokenId);
    }

    modifier onlyFlowAgreements() {
        if (msg.sender != address(CONSTANT_FLOW_AGREEMENT_V1)) {
            revert COF_NFT_ONLY_CFA();
        }
        _;
    }
}
