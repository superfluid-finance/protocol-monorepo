// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";
import {
    IERC165Upgradeable,
    IERC721Upgradeable,
    IERC721MetadataUpgradeable
} from "@openzeppelin/contracts-upgradeable/token/ERC721/extensions/IERC721MetadataUpgradeable.sol";
import { Strings } from "@openzeppelin/contracts/utils/Strings.sol";
import { ICFAv1NFTBase } from "../interfaces/superfluid/ICFAv1NFTBase.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

/// @title CFAv1NFTBase abstract contract
/// @author Superfluid
/// @notice The abstract contract to be inherited by the Constant Flow NFTs.
/// @dev This contract inherits from ICFAv1NFTBase which inherits from
/// IERC721MetadataUpgradeable and holds shared storage and functions for the two NFT contracts.
/// This contract is upgradeable and it inherits from our own ad-hoc UUPSProxiable contract which allows.
/// NOTE: the storage gap allows us to add an additional 45 storage variables to this contract without breaking child
/// COFNFT or CIFNFT storage.
abstract contract CFAv1NFTBase is UUPSProxiable, ICFAv1NFTBase {
    using Strings for uint256;

    string public constant BASE_URI =
        "https://nft.superfluid.finance/cfa/v1/getmeta";

    /// NOTE: The storage variables in this contract MUST NOT:
    /// - change the ordering of the existing variables
    /// - change any of the variable types
    /// - rename any of the existing variables
    /// - remove any of the existing variables
    /// - add any new variables after _gap
    /// - add any new variables before _gap and NOT decrement the length of the _gap array
    /// Go to CFAv1NFTUpgradability.t.sol for the tests and make sure to add new tests for upgrades.

    ISuperToken public superToken;

    string internal _name;
    string internal _symbol;

    /// @notice Mapping for token approvals
    /// @dev tokenID => approved address mapping
    mapping(uint256 => address) internal _tokenApprovals;

    /// @notice Mapping for operator approvals
    /// @dev owner => operator => approved boolean mapping
    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    /// @notice ConstantFlowAgreementV1 contract address
    /// @dev This is the address of the CFAv1 contract cached so we don't have to
    /// do an external call for every flow created.
    IConstantFlowAgreementV1 public cfaV1;

    /// @notice This allows us to add new storage variables in the base contract
    /// without having to worry about messing up the storage layout that exists in COFNFT or CIFNFT.
    /// @dev This empty reserved space is put in place to allow future versions to add new
    /// variables without shifting down storage in the inheritance chain.
    /// Slots 6-21 are reserved for future use.
    /// We use this pattern in SuperToken.sol and favor this over the OpenZeppelin pattern
    /// as this prevents silly footgunning.
    /// See https://docs.openzeppelin.com/contracts/4.x/upgradeable#storage_gaps
    uint256 internal _reserve6;
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

    /// @notice Informs third-party platforms that NFT metadata should be updated
    /// @dev This event comes from https://eips.ethereum.org/EIPS/eip-4906
    /// @param tokenId the id of the token that should have its metadata updated
    event MetadataUpdate(uint256 tokenId);

    error CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();   // 0xa3352582
    error CFA_NFT_APPROVE_TO_CALLER();                              // 0xd3c77329
    error CFA_NFT_APPROVE_TO_CURRENT_OWNER();                       // 0xe4790b25
    error CFA_NFT_INVALID_TOKEN_ID();                               // 0xeab95e3b
    error CFA_NFT_ONLY_HOST();                                      // 0x2d5a6dfa
    error CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();  // 0x2551d606
    error CFA_NFT_TRANSFER_FROM_INCORRECT_OWNER();                  // 0x5a26c744
    error CFA_NFT_TRANSFER_IS_NOT_ALLOWED();                        // 0xaa747eca
    error CFA_NFT_TRANSFER_TO_ZERO_ADDRESS();                       // 0xde06d21e

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
        cfaV1 = IConstantFlowAgreementV1(
            address(ISuperfluid(superToken.getHost()).getAgreementClass(
                keccak256(
                    "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                )
            ))
        );
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

    /// @notice This contract supports IERC165Upgradeable, IERC721Upgradeable and IERC721MetadataUpgradeable
    /// @dev This is part of the Standard Interface Detection EIP: https://eips.ethereum.org/EIPS/eip-165
    /// @param interfaceId the XOR of all function selectors in the interface
    /// @return boolean true if the interface is supported
    /// @inheritdoc IERC165Upgradeable
    function supportsInterface(
        bytes4 interfaceId
    ) external pure virtual override returns (bool) {
        return
            interfaceId == type(IERC165Upgradeable).interfaceId ||
            interfaceId == type(IERC721Upgradeable).interfaceId ||
            interfaceId == type(IERC721MetadataUpgradeable).interfaceId;
    }

    /// @inheritdoc IERC721Upgradeable
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
        CFAv1NFTFlowData memory flowData = flowDataByTokenId(tokenId);
        address superTokenAddress = address(superToken);

        string memory superTokenSymbol = superToken.symbol();

        (uint256 startDate, int96 flowRate) = _getFlow(
            flowData.flowSender,
            flowData.flowReceiver
        );

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
                    startDate.toString()
                )
            );
    }

    /// @inheritdoc IERC721Upgradeable
    function approve(address to, uint256 tokenId) public virtual override {
        address owner = CFAv1NFTBase.ownerOf(tokenId);
        if (to == owner) {
            revert CFA_NFT_APPROVE_TO_CURRENT_OWNER();
        }

        if (msg.sender != owner && !isApprovedForAll(owner, msg.sender)) {
            revert CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL();
        }

        _approve(to, tokenId);
    }

    /// @inheritdoc ICFAv1NFTBase
    function getTokenId(
        address sender,
        address receiver
    ) external view returns (uint256 tokenId) {
        tokenId = _getTokenId(sender, receiver);
    }

    function _getTokenId(
        address sender,
        address receiver
    ) internal view returns (uint256 tokenId) {
        tokenId = uint256(keccak256(abi.encode(sender, receiver)));
    }

    /// @inheritdoc IERC721Upgradeable
    function getApproved(
        uint256 tokenId
    ) public view virtual override returns (address) {
        _requireMinted(tokenId);

        return _tokenApprovals[tokenId];
    }

    /// @inheritdoc IERC721Upgradeable
    function setApprovalForAll(
        address operator,
        bool approved
    ) external virtual override {
        _setApprovalForAll(msg.sender, operator, approved);
    }

    /// @inheritdoc IERC721Upgradeable
    function isApprovedForAll(
        address owner,
        address operator
    ) public view virtual override returns (bool) {
        return _operatorApprovals[owner][operator];
    }

    /// @inheritdoc IERC721Upgradeable
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

    /// @inheritdoc IERC721Upgradeable
    function safeTransferFrom(
        address from,
        address to,
        uint256 tokenId
    ) external virtual override {
        safeTransferFrom(from, to, tokenId, "");
    }

    /// @inheritdoc IERC721Upgradeable
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
        address owner = CFAv1NFTBase.ownerOf(tokenId);
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

    function _getFlow(
        address sender,
        address receiver
    ) internal view returns (uint256 timestamp, int96 flowRate) {
        (timestamp, flowRate, , ) = superToken.getFlow(sender, receiver);
    }

    /// @dev Returns the flow data of the `tokenId`. Does NOT revert if token doesn't exist.
    /// @param tokenId the token id whose existence we're checking
    /// @return flowData the CFAv1NFTFlowData struct for `tokenId`
    function flowDataByTokenId(
        uint256 tokenId
    ) public view virtual returns (CFAv1NFTFlowData memory flowData);

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
        bytes memory data
    ) internal virtual;
}
