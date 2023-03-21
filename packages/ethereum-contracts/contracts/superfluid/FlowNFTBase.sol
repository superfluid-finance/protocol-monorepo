// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";
import {
    IERC165,
    IERC721,
    IERC721Metadata
} from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { Strings } from "@openzeppelin/contracts/utils/Strings.sol";
import { IFlowNFTBase } from "../interfaces/superfluid/IFlowNFTBase.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

/// @title FlowNFTBase abstract contract
/// @author Superfluid
/// @notice The abstract contract to be inherited by the Flow NFTs.
/// @dev This contract inherits from IFlowNFTBase which inherits from
/// IERC721Metadata and holds shared storage and functions for the two NFT contracts.
/// This contract is upgradeable and it inherits from our own ad-hoc UUPSProxiable contract which allows.
/// NOTE: the storage gap allows us to add an additional 16 storage variables to this contract without breaking child
/// COFNFT or CIFNFT storage.
abstract contract FlowNFTBase is UUPSProxiable, IFlowNFTBase {
    using Strings for uint256;

    string public constant BASE_URI =
        "https://nft.superfluid.finance/cfa/v1/getmeta";

    /**************************************************************************
     * Storage variables
     *************************************************************************/
    /// NOTE: The storage variables in this contract MUST NOT:
    /// - change the ordering of the existing variables
    /// - change any of the variable types
    /// - rename any of the existing variables
    /// - remove any of the existing variables
    /// - add any new variables after _gap
    /// - add any new variables before _gap and NOT decrement the length of the _gap array
    /// Go to CFAv1NFTUpgradability.t.sol for the tests and make sure to add new tests for upgrades.

    /// @notice ConstantFlowAgreementV1 contract address
    /// @dev This is the address of the CFAv1 contract cached so we don't have to
    /// do an external call for every flow created.
    // solhint-disable-next-line var-name-mixedcase
    IConstantFlowAgreementV1 public immutable CONSTANT_FLOW_AGREEMENT_V1;

    ISuperToken public superToken;

    string internal _name;
    string internal _symbol;

    /// @notice Mapping for token approvals
    /// @dev tokenID => approved address mapping
    mapping(uint256 => address) internal _tokenApprovals;

    /// @notice Mapping for operator approvals
    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    /// @notice This allows us to add new storage variables in the base contract
    /// without having to worry about messing up the storage layout that exists in COFNFT or CIFNFT.
    /// @dev This empty reserved space is put in place to allow future versions to add new
    /// variables without shifting down storage in the inheritance chain.
    /// Slots 6-21 are reserved for future use.
    /// We use this pattern in SuperToken.sol and favor this over the OpenZeppelin pattern
    /// as this prevents silly footgunning.
    /// See https://docs.openzeppelin.com/contracts/4.x/upgradeable#storage_gaps
    uint256 internal _reserve5;
    uint256 private _reserve6;
    uint256 private _reserve7;
    uint256 private _reserve8;
    uint256 private _reserve9;
    uint256 private _reserve10;
    uint256 private _reserve11;
    uint256 private _reserve12;
    uint256 private _reserve13;
    uint256 private _reserve14;
    uint256 private _reserve15;
    uint256 private _reserve16;
    uint256 private _reserve17;
    uint256 private _reserve18;
    uint256 private _reserve19;
    uint256 private _reserve20;
    uint256 internal _reserve21;

    constructor(
        IConstantFlowAgreementV1 cfaV1Contract
    ) {
        CONSTANT_FLOW_AGREEMENT_V1 = cfaV1Contract;
    }

    function initialize(
        ISuperToken superTokenContract,
        string memory nftName,
        string memory nftSymbol
    )
        external
        initializer // OpenZeppelin Initializable
    {
        superToken = superTokenContract;
        _name = nftName;
        _symbol = nftSymbol;
    }

    function updateCode(address newAddress) external override {
        if (msg.sender != address(superToken.getHost())) {
            revert CFA_NFT_ONLY_HOST();
        }

        UUPSProxiable._updateCodeAddress(newAddress);
    }

    /// @notice Emits the MetadataUpdate event with `tokenId` as the argument.
    /// @dev Callable by anyone.
    /// @param tokenId the token id to trigger a metaupdate for
    function triggerMetadataUpdate(uint256 tokenId) external {
        _triggerMetadataUpdate(tokenId);
    }

    /// @notice This contract supports IERC165, IERC721 and IERC721Metadata
    /// @dev This is part of the Standard Interface Detection EIP: https://eips.ethereum.org/EIPS/eip-165
    /// @param interfaceId the XOR of all function selectors in the interface
    /// @return boolean true if the interface is supported
    /// @inheritdoc IERC165
    function supportsInterface(
        bytes4 interfaceId
    ) external pure virtual override returns (bool) {
        return
            interfaceId == 0x01ffc9a7 || // ERC165 Interface ID for ERC165
            interfaceId == 0x80ac58cd || // ERC165 Interface ID for ERC721
            interfaceId == 0x5b5e139f; // ERC165 Interface ID for ERC721Metadata
    }

    /// @inheritdoc IERC721
    function ownerOf(
        uint256 tokenId
    ) public view virtual override returns (address) {
        address owner = _ownerOf(tokenId);
        if (owner == address(0)) {
            revert CFA_NFT_INVALID_TOKEN_ID();
        }
        return owner;
    }

    /// @notice Returns a hardcoded balance of 1
    /// @dev We always return 1 to avoid the need for additional mapping
    /// @return balance = 1
    function balanceOf(
        address // owner
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

    /// @notice This returns the Uniform Resource Identifier (URI), where the metadata for the NFT lives.
    /// @dev Returns the Uniform Resource Identifier (URI) for `tokenId` token.
    /// @return the token URI
    function tokenURI(
        uint256 tokenId
    ) external view virtual override returns (string memory) {
        FlowNFTData memory flowData = flowDataByTokenId(tokenId);
        address superTokenAddress = address(superToken);

        string memory superTokenSymbol = superToken.symbol();

        (
            uint256 lastUpdatedAt,
            int96 flowRate, ,
        ) = CONSTANT_FLOW_AGREEMENT_V1.getFlow(superToken, flowData.flowSender, flowData.flowReceiver);

        return
            string(
                abi.encodePacked(
                    BASE_URI,
                    "?chain_id=",
                    block.chainid.toString(),
                    "&token_address=",
                    Strings.toHexString(
                        uint256(uint160(superTokenAddress)),
                        20
                    ),
                    "&token_symbol=",
                    superTokenSymbol,
                    "&token_decimals=",
                    uint256(18).toString(),
                    "&sender=",
                    Strings.toHexString(
                        uint256(uint160(flowData.flowSender)),
                        20
                    ),
                    "&receiver=",
                    Strings.toHexString(
                        uint256(uint160(flowData.flowReceiver)),
                        20
                    ),
                    "&flowRate=",
                    uint256(uint96(flowRate)).toString(),
                    "&start_date=",
                    lastUpdatedAt.toString()
                )
            );
    }

    /// @inheritdoc IERC721
    function approve(address to, uint256 tokenId) public virtual override {
        address owner = FlowNFTBase.ownerOf(tokenId);
        if (to == owner) {
            revert CFA_NFT_APPROVE_TO_CURRENT_OWNER();
        }

        if (msg.sender != owner && !isApprovedForAll(owner, msg.sender)) {
            revert CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _approve(to, tokenId);
    }

    /// @inheritdoc IFlowNFTBase
    function getTokenId(
        address sender,
        address receiver
    ) external pure returns (uint256 tokenId) {
        tokenId = _getTokenId(sender, receiver);
    }

    function _getTokenId(
        address sender,
        address receiver
    ) internal pure returns (uint256 tokenId) {
        tokenId = uint256(keccak256(abi.encode(sender, receiver)));
    }

    /// @inheritdoc IERC721
    function getApproved(
        uint256 tokenId
    ) public view virtual override returns (address) {
        _requireMinted(tokenId);

        return _tokenApprovals[tokenId];
    }

    /// @inheritdoc IERC721
    function setApprovalForAll(
        address operator,
        bool approved
    ) external virtual override {
        _setApprovalForAll(msg.sender, operator, approved);
    }

    /// @inheritdoc IERC721
    function isApprovedForAll(
        address owner,
        address operator
    ) public view virtual override returns (bool) {
        return _operatorApprovals[owner][operator];
    }

    /// @inheritdoc IERC721
    function transferFrom(
        address from,
        address to,
        uint256 tokenId
    ) external virtual override {
        if (!_isApprovedOrOwner(msg.sender, tokenId)) {
            revert CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _transfer(from, to, tokenId);
    }

    /// @inheritdoc IERC721
    function safeTransferFrom(
        address from,
        address to,
        uint256 tokenId
    ) external virtual override {
        safeTransferFrom(from, to, tokenId, "");
    }

    /// @inheritdoc IERC721
    function safeTransferFrom(
        address from,
        address to,
        uint256 tokenId,
        bytes memory data
    ) public virtual override {
        if (!_isApprovedOrOwner(msg.sender, tokenId)) {
            revert CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _safeTransfer(from, to, tokenId, data);
    }

    /// @notice Returns whether `spender` is allowed to manage `tokenId`.
    /// @dev Will revert if `tokenId` doesn't exist.
    /// @param spender the spender of the token
    /// @param tokenId the id of the token to be spent
    /// @return whether `tokenId` can be spent by `spender`
    function _isApprovedOrOwner(
        address spender,
        uint256 tokenId
    ) internal view returns (bool) {
        address owner = FlowNFTBase.ownerOf(tokenId);
        return (spender == owner ||
            isApprovedForAll(owner, spender) ||
            getApproved(tokenId) == spender);
    }

    /// @notice Reverts if `tokenId` doesn't exist
    /// @param tokenId the token id whose existence we are checking
    function _requireMinted(uint256 tokenId) internal view {
        if (!_exists(tokenId)) revert CFA_NFT_INVALID_TOKEN_ID();
    }

    /// @notice Returns whether `tokenId` exists
    /// @dev Explain to a developer any extra details
    /// Tokens can be managed by their owner or approved accounts via `approve` or `setApprovalForAll`.
    /// Tokens start existing when they are minted (`_mint`),
    /// and stop existing when they are burned (`_burn`).
    /// @param tokenId the token id we're interested in seeing if exists
    /// @return bool whether ot not the token exists
    function _exists(uint256 tokenId) internal view returns (bool) {
        return _ownerOf(tokenId) != address(0);
    }

    function _triggerMetadataUpdate(uint256 tokenId) internal {
        emit MetadataUpdate(tokenId);
    }

    function _approve(address to, uint256 tokenId) internal {
        _tokenApprovals[tokenId] = to;

        emit Approval(_ownerOf(tokenId), to, tokenId);
    }

    function _setApprovalForAll(
        address owner,
        address operator,
        bool approved
    ) internal {
        if (owner == operator) revert CFA_NFT_APPROVE_TO_CALLER();

        _operatorApprovals[owner][operator] = approved;

        emit ApprovalForAll(owner, operator, approved);
    }

    /// @dev Returns the flow data of the `tokenId`. Does NOT revert if token doesn't exist.
    /// @param tokenId the token id whose existence we're checking
    /// @return flowData the FlowNFTData struct for `tokenId`
    function flowDataByTokenId(
        uint256 tokenId
    ) public view virtual returns (FlowNFTData memory flowData);

    /// @dev Returns the owner of the `tokenId`. Does NOT revert if token doesn't exist.
    /// @param tokenId the token id whose existence we're checking
    /// @return address the address of the owner of `tokenId`
    function _ownerOf(uint256 tokenId) internal view virtual returns (address);

    function _transfer(
        address from,
        address to,
        uint256 tokenId
    ) internal virtual;

    function _safeTransfer(
        address from,
        address to,
        uint256 tokenId,
        bytes memory // data
    ) internal virtual {
        _transfer(from, to, tokenId);
    }

    /// @dev Deletes the tokenApprovals for `tokenId`
    /// @param tokenId the token id whose approvals we're clearing
    function _burn(uint256 tokenId) internal virtual {
        // clear approvals from the previous owner
        delete _tokenApprovals[tokenId];
    }
}
