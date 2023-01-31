// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantInflowNFT
} from "../interfaces/superfluid/IConstantInflowNFT.sol";
import {
    IConstantOutflowNFT
} from "../interfaces/superfluid/IConstantOutflowNFT.sol";
import { CFAv1NFTBase } from "./CFAv1NFTBase.sol";

/// @note TODO: clean up the inheritance with IConstantOutflowNFT and CFAv1Base
// solhint-disable no-empty-blocks
// solhint-disable no-unused-vars

/// @title ConstantOutflowNFT contract (COF NFT)
/// @author Superfluid
/// @notice The ConstantOutflowNFT contract to be minted to the flow sender on flow creation.
/// @dev This contract uses mint/burn interface for flow creation/deletion and holds the actual storage for both NFTs.
contract ConstantOutflowNFT is CFAv1NFTBase {
    /// @notice A mapping from token id to FlowData = { address sender, address receiver}
    /// @dev The token id is uint256(keccak256(abi.encode(flowSender, flowReceiver)))
    mapping(uint256 => FlowData) internal _flowDataByTokenId;

    error COF_NFT_ONLY_CONSTANT_INFLOW();           // 0xa495a718
    error COF_NFT_ONLY_CFA();                       // 0x054fae59
    error COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME(); // 0x0d1d1161
    error COF_NFT_MINT_TO_ZERO_ADDRESS();           // 0x43d05e51
    error COF_NFT_TOKEN_ALREADY_EXISTS();           // 0xe2480183
    error COF_NFT_TRANSFER_IS_NOT_ALLOWED();        // 0x5b1855b1

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }

    /// @notice An external function for querying flow data by `_tokenId``
    /// @param _tokenId the token id
    /// @return flowData the flow data associated with `_tokenId`
    function flowDataByTokenId(
        uint256 _tokenId
    ) public view override returns (FlowData memory flowData) {
        flowData = _flowDataByTokenId[_tokenId];
    }

    /// @note Neither mint nor burn will work here because we need to forward these calls.

    /// @notice The mint function creates a flow from `_from` to `_to`.
    /// @dev If `msg.sender` is not equal to `_from`, we `createFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT creation.
    /// @param _from desired flow sender
    /// @param _to desired flow receiver
    /// @param _flowRate desired flow rate
    function mint(address _from, address _to, int96 _flowRate) external {
        // regular create flow
        if (msg.sender == _from) {
            // superToken.createFlow(_to, _flowRate);
        } else {
            // superToken.createFlowFrom(_from, _to, _flowRate);
        }
    }

    /// @notice The burn function deletes the flow between `sender` and `receiver` stored in `_tokenId`
    /// @dev If `msg.sender` is not equal to `_from`, we `deleteFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT deletion.
    /// @param _tokenId desired token id to burn
    function burn(uint256 _tokenId) external {
        FlowData memory flowData = _flowDataByTokenId[_tokenId];
        if (flowData.flowSender == msg.sender) {
            // superToken.deleteFlow(flowData.sender, flowData.receiver);
        } else {
            // superToken.deleteFlowFrom(flowData.sender, flowData.receiver);
        }
    }

    /// NOTE probably should be access controlled to only cfa
    function onCreate(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) external {
        _mint(_to, _flowReceiver, _newTokenId);

        IConstantInflowNFT constantInflowNFT = superToken.constantInflowNFT();
        constantInflowNFT.mint(_flowReceiver, _newTokenId);
    }

    /// NOTE probably should be access controlled to only cfa
    /// but also not super important for triggering metadata update
    function onUpdate(uint256 _tokenId) external {
        _triggerMetadataUpdate(_tokenId);

        IConstantInflowNFT constantInflowNFT = superToken.constantInflowNFT();
        constantInflowNFT.triggerMetadataUpdate(_tokenId);
    }

    /// NOTE probably should be access controlled to only cfa
    function onDelete(uint256 _tokenId) external {
        // must "burn" inflow NFT first because we clear storage when burning outflow NFT
        IConstantInflowNFT constantInflowNFT = superToken.constantInflowNFT();
        constantInflowNFT.burn(_tokenId);

        _burn(_tokenId);
    }

    /// @notice Handles the mint of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param _to the receiver of the newly minted token
    /// @param _flowReceiver the flow receiver (owner of the InflowNFT)
    /// @param _newTokenId the new token id to be minted when an inflowNFT is minted
    function inflowTransferMint(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) external onlyConstantInflowNFT {
        _mint(_to, _flowReceiver, _newTokenId);
    }

    /// @notice Handles the burn of ConstantOutflowNFT when an inflow NFT user transfers their NFT.
    /// @dev Only callable by ConstantInflowNFT
    /// @param _tokenId the token id to burn when an inflow NFT is transferred
    function inflowTransferBurn(uint256 _tokenId) external onlyConstantInflowNFT {
        _burn(_tokenId);
    }

    function _safeTransfer(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes memory // _data
    ) internal virtual override {
        _transfer(_from, _to, _tokenId);
    }

    /// @inheritdoc CFAv1NFTBase
    function _ownerOf(
        uint256 _tokenId
    ) internal view virtual override returns (address) {
        return _flowDataByTokenId[_tokenId].flowSender;
    }

    /// @notice Reverts - Transfer of outflow NFT is not allowed.
    /// @dev We revert when users attempt to transfer outflow NFTs.
    function _transfer(
        address, // _from,
        address, // _to,
        uint256 // _tokenId
    ) internal virtual override {
        // @note TODO WRITE A TEST TO ENSURE ALL THE TRANSFER FUNCTIONS REVERT
        revert COF_NFT_TRANSFER_IS_NOT_ALLOWED();
    }

    /// @notice Mints `_newTokenId` and transfers it to `_to`
    /// @dev `_newTokenId` must not exist `_to` cannot be `address(0)` and we emit a {Transfer} event.
    /// `_to` cannot be equal to `_flowReceiver`.
    /// @param _to the receiver of the newly minted outflow nft (flow sender)
    /// @param _flowReceiver the flow receiver (owner of the InflowNFT)
    /// @param _newTokenId the new token id to be minted
    function _mint(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) internal {
        if (_to == address(0)) {
            revert COF_NFT_MINT_TO_ZERO_ADDRESS();
        }

        if (_to == _flowReceiver) {
            revert COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME();
        }

        if (_exists(_newTokenId)) {
            revert COF_NFT_TOKEN_ALREADY_EXISTS();
        }

        // update mapping for new NFT to be minted
        _flowDataByTokenId[_newTokenId] = FlowData(_to, _flowReceiver);

        // emit mint of new outflow token with newTokenId
        emit Transfer(address(0), _to, _newTokenId);
    }

    /// @notice Destroys token with `_tokenId` and clears approvals from previous owner.
    /// @dev `_tokenId` must exist AND we emit a {Transfer} event
    /// @param _tokenId the id of the token we are destroying
    function _burn(uint256 _tokenId) internal {
        address owner = CFAv1NFTBase.ownerOf(_tokenId);

        // clear approvals from the previous owner
        delete _tokenApprovals[_tokenId];

        // remove previous _tokenId flow data mapping
        delete _flowDataByTokenId[_tokenId];

        // emit burn of outflow token with _tokenId
        emit Transfer(owner, address(0), _tokenId);
    }

    modifier onlyConstantInflowNFT() {
        address constantInflowNFT = address(superToken.constantInflowNFT());
        if (msg.sender != constantInflowNFT) revert COF_NFT_ONLY_CONSTANT_INFLOW();
        _;
    }
}
