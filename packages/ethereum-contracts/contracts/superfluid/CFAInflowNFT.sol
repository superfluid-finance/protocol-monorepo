// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;
// @note temporary
// solhint-disable no-empty-blocks
// solhint-disable no-unused-vars
// solhint-disable not-rely-on-time

import {
    ISuperfluid,
    ISuperToken
} from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { CFANFTBase } from "./CFANFTBase.sol";
import { CFAOutflowNFT } from "./CFAOutflowNFT.sol";

contract CFAInflowNFT is CFANFTBase {

    // Mapping from token ID to approved address
    // @note this only allows a single approved address per token
    mapping(uint256 => address) private _tokenApprovals;

    error CFA_INFLOW_NFT_ONLY_OWNER_OR_APPROVED();

    constructor(
        ISuperfluid _host,
        ISuperToken _superToken,
        IConstantFlowAgreementV1 _cfa,
        string memory _name,
        string memory _symbol
    ) CFANFTBase(_host, _superToken, _cfa, _name, _symbol) {}

    function cfaOutflowNFT() public view returns (CFAOutflowNFT) {
        return CFAOutflowNFT(superToken.cfaOutflowNFT());
    }

    /// inflow nft balance
    function balanceOf(
        address _owner
    ) external view override returns (uint256) {
        return cfaOutflowNFT().balanceOf(_owner);
    }

    /// @notice Grant `_to` a token with `_tokenId`
    /// @param _to The address you want to approve transfers for
    /// @param _tokenId The token id you want to approve transfers for
    function approve(address _to, uint256 _tokenId) public virtual override {
        address owner = ownerOf(_tokenId);
        require(_to != owner, "ERC721: approval to current owner");

        require(msg.sender == owner, "ERC721: approve caller is not token owner or approved for all");

        _approve(_to, _tokenId);
    }

    /**
     * @dev Approve `to` to operate on `_tokenId`
     *
     * Emits an {Approval} event.
     */
    function _approve(address to, uint256 _tokenId) internal virtual {
        _tokenApprovals[_tokenId] = to;
        emit Approval(_ownerOf(_tokenId), to, _tokenId);
    }

    /**
     * @dev Reverts if the `_tokenId` has not been minted yet.
     */
    function _requireMinted(uint256 _tokenId) internal view virtual {
        require(_exists(_tokenId), "ERC721: invalid token ID");
    }

    /**
     * @dev Returns whether `_tokenId` exists.
     *
     * Tokens can be managed by their owner or approved accounts via {approve} or {setApprovalForAll}.
     *
     * Tokens start existing when they are minted (`_mint`),
     * and stop existing when they are burned (`_burn`).
     */
    function _exists(uint256 _tokenId) internal view virtual returns (bool) {
        return _ownerOf(_tokenId) != address(0);
    }

    /**
     * @dev Returns the owner of the `_tokenId`. Does NOT revert if token doesn't exist
     */
    function _ownerOf(uint256 _tokenId) internal view virtual returns (address) {
        FlowData memory flowData = cfaOutflowNFT().getFlowDataByTokenId(_tokenId);
        return flowData.receiver;
    }

    /**
     * @dev See {IERC721-getApproved}.
     */
    function getApproved(uint256 _tokenId) public view override returns (address) {
        _requireMinted(_tokenId);
        return _tokenApprovals[_tokenId];
    }

    /// @notice This sets approval for operator to allow transfer of all your inflow NFTs
    /// @dev Can use this to allow someone to transfer all of your tokens
    /// @param _operator The address you want to grant or revoke ACL permissions for
    /// @param _approved Whether you want to grant (true) or revoke (false) permissions
    function setApprovalForAll(address _operator, bool _approved) external override {
    }


    /// @notice Returns the owner of the outflowing flow with id `_tokenId`
    /// @dev `_tokenId` is the uint256 cast of the keccak256 hash of the sender and receiver addresses
    /// @param _tokenId uint256(keccak256(abi.encode(sender, receiver)))
    /// @return The owner of the outflowing flow NFT with id `_tokenId`
    function ownerOf(
        uint256 _tokenId
    ) public view override returns (address) {
        return cfaOutflowNFT().getFlowDataByTokenId(_tokenId).receiver;
    }

    /// @notice Transfers an inflow NFT from `_from` to `_to`
    /// @dev In addition to the old NFT being burned and a new NFT being minted,
    /// the flow is redirected from `_from` to `_to`
    /// @param _from the flow sender of the inflow NFT
    /// @param _to the receiver of the inflow NFT
    /// @param _tokenId the id of the inflow NFT keccak256(abi.encode(sender, oldReceiver))
    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external override {
        // check if msg.sender has approval unless it is equal to _from
        if (msg.sender != _from && getApproved(_tokenId) != msg.sender) {
            revert CFA_INFLOW_NFT_ONLY_OWNER_OR_APPROVED();
        }

        // Clear approvals from the previous owner
        delete _tokenApprovals[_tokenId];

        FlowData memory flowData = cfaOutflowNFT().getFlowDataByTokenId(
            _tokenId
        );
        (, int96 flowRate, , ) = cfa.getFlow(
            superToken,
            flowData.sender,
            flowData.receiver
        );

        // this function deletes the flow data between the original sender and receiver
        // and creates new flow data between the original sender and _to
        // it also updates the balances mapping of the old receiver and new receiver
        cfaOutflowNFT().handleInflowTransfer(
            flowData.sender,
            flowData.receiver,
            _to
        );

        // delete the existing flow from sender to receiver
        bytes memory deleteFlowCallData = abi.encodeCall(
            cfa.deleteFlow,
            (superToken, flowData.sender, flowData.receiver, new bytes(0))
        );
        _forwardTokenCall(
            flowData.sender,
            address(cfa),
            deleteFlowCallData,
            new bytes(0)
        );

        // and create a new flow from sender to _to
        bytes memory createFlowCallData = abi.encodeCall(
            cfa.createFlow,
            (superToken, _to, flowRate, new bytes(0))
        );
        _forwardTokenCall(
            flowData.sender,
            address(cfa),
            createFlowCallData,
            new bytes(0)
        );

        emit Transfer(_from, _to, _tokenId);
    }

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes calldata _data
    ) external override {
        // safe transfer of nft
    }

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external override {
        // safe transfer of nft
    }

    function tokenURI(
        uint256 _tokenId
    ) external view override returns (string memory) {
        // get token uri
    }
}
