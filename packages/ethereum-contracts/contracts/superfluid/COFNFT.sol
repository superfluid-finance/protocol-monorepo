// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";
import { CFANFTBase, IERC721 } from "./CFANFTBase.sol";

/// @title COFNFT contract (Constant Out Flow NFT)
/// @author Superfluid
/// @notice The COFNFT contract to be minted to the flow sender on flow creation.
/// @dev This function also uses mint/burn interface for flow creation/deletion.
contract COFNFT is CFANFTBase {
    using SuperTokenV1Library for ISuperToken;

    // mapping from uint256(keccak256(abi.encode(sender, receiver))) to FlowData
    mapping(uint256 => FlowData) private _flowDataBySenderReceiver;

    error COF_NFT_MINT_TO_ZERO_ADDRESS(); // 0x43d05e51
    error COF_NFT_TOKEN_ALREADY_EXISTS(); // 0xe2480183

    constructor(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    )
        CFANFTBase(_superToken, _nftName, _nftSymbol)
    // solhint-disable-next-line no-empty-blocks
    {

    }

    /// @notice This returns the Uniform Resource Identifier (URI), where the metadata for the NFT lives.
    /// @dev Returns the Uniform Resource Identifier (URI) for `_tokenId` token.
    /// @return the token URI
    function tokenURI(
        uint256 // _tokenId
    ) external view virtual override returns (string memory) {
        return "";
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
            superToken.createFlow(_to, _flowRate);
        } else {
            superToken.createFlowFrom(_from, _to, _flowRate);
        }
    }

    /// @notice The burn function deletes the flow between `sender` and `receiver` stored in `_tokenId`
    /// @dev If `msg.sender` is not equal to `_from`, we `deleteFlowByOperator`.
    /// Also important to note is that the agreement contract will handle the NFT deletion.
    /// @param _tokenId desired token id to burn
    function burn(uint256 _tokenId) external {
        FlowData memory flowData = _flowDataBySenderReceiver[_tokenId];
        if (flowData.sender == msg.sender) {
            superToken.deleteFlow(flowData.sender, flowData.receiver);
        } else {
            superToken.deleteFlowFrom(flowData.sender, flowData.receiver);
        }
    }

    /// @inheritdoc IERC721
    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external virtual override {
        if (!_isApprovedOrOwner(msg.sender, _tokenId)) {
            revert CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _transfer(_from, _to, _tokenId);
    }

    /// @inheritdoc IERC721
    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external virtual override {
        safeTransferFrom(_from, _to, _tokenId, "");
    }

    /// @inheritdoc IERC721
    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes memory _data
    ) public virtual override {
        if (!_isApprovedOrOwner(msg.sender, _tokenId)) {
            revert CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _safeTransfer(_from, _to, _tokenId, _data);
    }

    function _safeTransfer(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes memory // _data
    ) internal virtual {
        _transfer(_from, _to, _tokenId);
        // TODO
        // require(_checkOnERC721Received(from, to, tokenId, data),
        // "ERC721: transfer to non ERC721Receiver implementer");
    }

    /// @inheritdoc CFANFTBase
    function _ownerOf(
        uint256 _tokenId
    ) internal view virtual override returns (address) {
        return _flowDataBySenderReceiver[_tokenId].sender;
    }

    /// @notice Transfers `_tokenId` from `_from` to `_to`
    /// @dev `_from` must own `_tokenId` and `_to` cannot be `address(0)`.
    ///
    /// We emit three Transfer events from this COFNFT contract:
    /// 1. Transfer of `_tokenId` (`_from` -> `_to`) | `_from` is old OutflowNFT owner | `_to` is new OutflowNFT owner
    /// 2. Transfer (burn) of `_tokenId` (`_to` -> `address(0)`)
    /// 3. Transfer (mint) of `newTokenId` (`address(0)` -> `_to`)
    ///
    /// We also emit two Transfer events from the CIFNFT contract:
    /// 1. Transfer (burn) of `_tokenId` (`_from` -> `address(0)`) | `_from` is InflowNFT owner
    /// 2. Transfer (mint) of `_tokenId` (`address(0)` -> `_to`)   | `_to` is InflowNFT owner
    ///
    /// We also clear storage for `_tokenApprovals` and `_flowDataBySenderReceiver` with `_tokenId`
    /// and create new storage for `_flowDataBySenderReceiver` with `newTokenId`.
    /// @param _from the owner of _tokenId
    /// @param _to the receiver of the NFT
    /// @param _tokenId the token id to transfer
    function _transfer(address _from, address _to, uint256 _tokenId) internal {
        if (CFANFTBase.ownerOf(_tokenId) != _from) {
            revert CFA_NFT_TRANSFER_FROM_INCORRECT_OWNER();
        }

        if (_to == address(0)) {
            revert CFA_NFT_TRANSFER_TO_ZERO_ADDRESS();
        }

        FlowData memory oldFlowData = _flowDataBySenderReceiver[_tokenId];
        uint256 newTokenId = uint256(
            keccak256(abi.encode(_to, oldFlowData.receiver))
        );

        /// TODO: If we choose to use the _beforeTokenTransfer hook
        /// _beforeTokenTransfer(from, to, _tokenId, 1);

        // Check that _tokenId was not transferred by `_beforeTokenTransfer` hook
        // require(_ownerOf(_tokenId) == _from, "ERC721: transfer from incorrect owner");

        // Clear approvals from the previous owner
        delete _tokenApprovals[_tokenId];

        // emit initial transfer of outflow token with _tokenId (from -> to)
        emit Transfer(_from, _to, _tokenId);

        // burn the outflow token with _tokenId
        _burn(_tokenId);

        // mint a new outflow token with newTokenId
        _mint(_to, oldFlowData.receiver, newTokenId);

        // TODO: What is the functionality of transfer of the NFT at the protocol level?
        // Do we want to implement something which occurs on transfer at the protocol level?
    }

    /// @notice Mints `_newTokenId` and transfers it to `_to`
    /// @dev `_newTokenId` must not exist `_to` cannot be `address(0)` and we emit a {Transfer} event.
    /// @param _to the receiver of the newly minted token
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

        if (_exists(_newTokenId)) {
            revert COF_NFT_TOKEN_ALREADY_EXISTS();
        }

        // update mapping for new NFT to be minted
        _flowDataBySenderReceiver[_newTokenId] = FlowData(_to, _flowReceiver);

        // emit mint of new outflow token with newTokenId
        emit Transfer(address(0), _to, _newTokenId);

        // INFLOWNFT.mint(newTokenId) => this will ONLY emit a mint Transfer event
    }

    /// @notice Destroys token with `_tokenId`
    /// @dev `_tokenId` must exist AND we emit a {Transfer} event
    /// @param _tokenId the id of the token we are destroying
    function _burn(uint256 _tokenId) internal {
        address owner = CFANFTBase.ownerOf(_tokenId);
        // remove previous _tokenId flow data mapping
        delete _flowDataBySenderReceiver[_tokenId];

        // emit burn of outflow token with _tokenId
        emit Transfer(owner, address(0), _tokenId);

        // INFLOWNFT.burn(_tokenId) => this will ONLY emit a burn Transfer event
    }
}
