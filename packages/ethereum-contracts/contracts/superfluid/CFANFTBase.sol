// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    IERC165,
    IERC721,
    IERC721Metadata
} from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";

import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";

/// @title CFANFTBase abstract contract
/// @author Superfluid
/// @notice The abstract contract to be inherited by the Constant Flow NFTs.
/// @dev This contract inherits from IERC721Metadata and holds shared functions for the two NFT contracts.
/// Take note of the storage gap at the end of the contract which allows us to add an additional 45 storage
/// variables to this contract without breaking child COFNFT or CIFNFT storage.
abstract contract CFANFTBase is IERC721Metadata {
    struct FlowData {
        address sender;
        address receiver;
    }

    ISuperToken public immutable superToken;

    string internal _name;
    string internal _symbol;

    /// @notice Mapping for token approvals
    /// @dev tokenID => approved address mapping
    mapping(uint256 => address) internal _tokenApprovals;

    /// @notice Mapping for operator approvals
    /// @dev owner => operator => approved boolean mapping
    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    /// @notice Informs third-party platforms that NFT metadata should be updated
    /// @dev This event comes from https://eips.ethereum.org/EIPS/eip-4906
    /// @param _tokenId the id of the token that should have its metadata updated
    event MetadataUpdate(uint256 _tokenId);

    error CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();   // 0xa3352582
    error CFA_NFT_APPROVE_TO_CALLER();                              // 0xd3c77329
    error CFA_NFT_APPROVE_TO_CURRENT_OWNER();                       // 0xe4790b25
    error CFA_NFT_INVALID_TOKEN_ID();                               // 0xeab95e3b
    error CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();  // 0x2551d606
    error CFA_NFT_TRANSFER_FROM_INCORRECT_OWNER();                  // 0x5a26c744
    error CFA_NFT_TRANSFER_TO_ZERO_ADDRESS();                       // 0xde06d21e

    constructor(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    ) {
        superToken = _superToken;
        _name = _nftName;
        _symbol = _nftSymbol;
    }

    /// @notice This informs this contract supports IERC165, IERC721 and IERC721Metadata
    /// @dev This is part of the Standard Interface Detection EIP: https://eips.ethereum.org/EIPS/eip-165
    /// @param _interfaceId the XOR of all function selectors in the interface
    /// @return boolean true if the interface is supported
    /// @inheritdoc IERC165
    function supportsInterface(
        bytes4 _interfaceId
    ) external pure virtual override returns (bool) {
        return
            _interfaceId == type(IERC165).interfaceId ||
            _interfaceId == type(IERC721).interfaceId ||
            _interfaceId == type(IERC721Metadata).interfaceId;
    }

    /// @inheritdoc IERC721
    function ownerOf(
        uint256 _tokenId
    ) public view virtual override returns (address) {
        address owner = _ownerOf(_tokenId);
        if (owner == address(0)) {
            revert CFA_NFT_INVALID_TOKEN_ID();
        }
        return owner;
    }

    /// @notice Returns a hardcoded balance of 1
    /// @dev We always return 1 to avoid the need for additional mapping
    /// @return balance = 1
    function balanceOf(
        address // _owner
    ) external pure returns (uint256 balance) {
        balance = 1;
    }

    /// @notice Returns the name of the NFT
    /// @dev Should follow the naming convention: TOKENx Constant Outflow/Inflow NFT
    /// @return name of the NFT
    function name() external view virtual override returns (string memory) {
        return _name;
    }

    /// @notice Returns the symbol of the NFT
    /// @dev Should follow the naming convention: TOKENx(COF/CIF)
    /// @return symbol of the NFT
    function symbol() external view virtual override returns (string memory) {
        return _symbol;
    }

    /// @inheritdoc IERC721
    function approve(address _to, uint256 _tokenId) public virtual override {
        address owner = CFANFTBase.ownerOf(_tokenId);
        if (_to == owner) {
            revert CFA_NFT_APPROVE_TO_CURRENT_OWNER();
        }

        if (msg.sender != owner && !isApprovedForAll(owner, msg.sender)) {
            revert CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _approve(_to, _tokenId);
    }

    /// @inheritdoc IERC721
    function getApproved(
        uint256 _tokenId
    ) public view virtual override returns (address) {
        _requireMinted(_tokenId);

        return _tokenApprovals[_tokenId];
    }

    /// @inheritdoc IERC721
    function setApprovalForAll(
        address _operator,
        bool _approved
    ) external virtual override {
        _setApprovalForAll(msg.sender, _operator, _approved);
    }

    /// @inheritdoc IERC721
    function isApprovedForAll(
        address _owner,
        address _operator
    ) public view virtual override returns (bool) {
        return _operatorApprovals[_owner][_operator];
    }

    /// @dev Returns the owner of the `tokenId`. Does NOT revert if token doesn't exist.
    /// @param _tokenId the token id whose existence we're checking
    /// @return address the address of the owner of `_tokenId`
    function _ownerOf(uint256 _tokenId) internal view virtual returns (address);

    /// @notice Returns whether `_spender` is allowed to manage `_tokenId`.
    /// @dev Will revert if `_tokenId` doesn't exist.
    /// @param _spender the spender of the token
    /// @param _tokenId the id of the token to be spent
    /// @return whether `_tokenId` can be spent by `_spender`
    function _isApprovedOrOwner(
        address _spender,
        uint256 _tokenId
    ) internal view virtual returns (bool) {
        address owner = CFANFTBase.ownerOf(_tokenId);
        return (_spender == owner ||
            isApprovedForAll(owner, _spender) ||
            getApproved(_tokenId) == _spender);
    }

    /// @notice Returns whether `_tokenId` exists
    /// @dev Explain to a developer any extra details
    /// Tokens can be managed by their owner or approved accounts via `approve` or `setApprovalForAll`.
    /// Tokens start existing when they are minted (`_mint`),
    /// and stop existing when they are burned (`_burn`).
    /// @param _tokenId the token id we're interested in seeing if exists
    /// @return bool whether ot not the token exists
    function _exists(uint256 _tokenId) internal view virtual returns (bool) {
        return _ownerOf(_tokenId) != address(0);
    }

    function _approve(address to, uint256 tokenId) internal virtual {
        _tokenApprovals[tokenId] = to;

        emit Approval(_ownerOf(tokenId), to, tokenId);
    }

    function _setApprovalForAll(
        address _owner,
        address _operator,
        bool _approved
    ) internal virtual {
        if (_owner == _operator) revert CFA_NFT_APPROVE_TO_CALLER();

        _operatorApprovals[_owner][_operator] = _approved;

        emit ApprovalForAll(_owner, _operator, _approved);
    }

    /// @notice Reverts if `_tokenId` doesn't exist
    /// @param _tokenId the token id whose existence we are checking
    function _requireMinted(uint256 _tokenId) internal view virtual {
        if (!_exists(_tokenId)) revert CFA_NFT_INVALID_TOKEN_ID();
    }

    /// @notice This allows us to add new storage variables in the base contract
    /// without having to worry about messing up the storage layout that exists in COFNFT or CIFNFT.
    /// @dev This empty reserved space is put in place to allow future versions to add new
    /// variables without shifting down storage in the inheritance chain.
    /// Important to note that the array number is calculated so the amount of storage used
    /// by a contract adds up to 50.
    /// So each time we add a new storage variable above `_gap`, we must decrease the length of the
    /// array by one.
    /// See https://docs.openzeppelin.com/contracts/4.x/upgradeable#storage_gaps
    uint256[45] private _gap;
}