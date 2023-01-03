// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;
// @note temporary
// solhint-disable no-empty-blocks
// solhint-disable no-unused-vars
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

contract CFAOutflowNFT {

    struct FlowData {
        address sender;
        address receiver;
        uint64 startDate;
    }

    ISuperToken public superToken;
    IConstantFlowAgreementV1 public cfa;
    string public name;
    string public symbol;
    // uint256 tokenCount;

    // Mapping owner address to token count
    mapping(address => uint256) private _balances;

    // mapping from keccak256(abi.encode(sender, receiver)) to FlowData
    mapping(bytes32 => FlowData) internal _flowDataBySenderReceiver;

    /**
     * @dev Emitted when `tokenId` token is transferred from `from` to `to`.
     */
    event Transfer(
        address indexed from,
        address indexed to,
        uint256 indexed tokenId
    );

    /**
     * @dev Emitted when `owner` enables `approved` to manage the `tokenId` token.
     */
    event Approval(
        address indexed owner,
        address indexed approved,
        uint256 indexed tokenId
    );

    /**
     * @dev Emitted when `owner` enables or disables (`approved`) `operator` to manage all of its assets.
     */
    event ApprovalForAll(
        address indexed owner,
        address indexed operator,
        bool approved
    );

    error ONLY_SUPER_TOKEN();

    constructor(
        ISuperToken _superToken,
        IConstantFlowAgreementV1 _cfa,
        string memory _name,
        string memory _symbol
    ) {
        superToken = _superToken;
        cfa = _cfa;
        name = _name;
        symbol = _symbol;
    }

    modifier onlySuperToken() {
        if (msg.sender != address(superToken)) {
            revert ONLY_SUPER_TOKEN();
        }
        _;
    }

    function mint(address _to) public onlySuperToken {
        _mint(_to);
    }

    function burn(address _sender) public onlySuperToken {
        _burn(_sender);
    }

    function transferFrom(address _from, address _to, uint256 _tokenId) public {
        // TODO: for outflow NFTs, we can allow a transfer to the _to address
        // IF and only IF the _from address has ACL permissions to create a flow
        // on behalf of the _to address
    }

    /// @notice Explain to an end user what this does
    /// @dev _permissions contains the permissions and the granted flowRateAllowance in the following format:
    /// WORD A: | reserved  | permissions | reserved | flowRateAllowance |
    ///         | 120       | 8           | 32       | 96                |
    /// NOTE: This is consistent with the format of our flow operator data.
    /// @param _to The address you want to provide ACL permissions for
    /// @param _permissions The address you want to provide ACL permissions for
    function approve(address _to, uint256 _permissions) public {
        // approve an nft
    }

    /// @notice Explain to an end user what this does
    /// @dev Explain to a developer any extra details
    /// @param _operator The address you want to grant or revoke ACL permissions for
    /// @param _approved Whether you want to grant (true) or revoke (false) permissions
    function setApprovalForAll(address _operator, bool _approved) public {
        // set approval for all nfts
        if (_approved == true) {
            // grant permissions
        } else {
            // revoke permissions
        }
    }

    /// @notice This function will always return the zero address
    /// @return address(0)
    function getApproved(
        uint256 /// _tokenId
    ) public pure returns (address) {
        return address(0);
    }

    function isApprovedForAll(
        address _owner,
        address _operator
    ) public view returns (bool) {
        // check if approved for all nfts
    }

    /// @notice Returns the number of outflowing flows owned by `_owner`
    /// @param _owner The address to query the number of outflowing flows for
    /// @return The number of outflowing streams owned by `_owner`
    function balanceOf(address _owner) public view returns (uint256) {
        return _balances[_owner];
    }

    /// @notice Returns the owner of the outflowing flow with id `_tokenId`
    /// @dev `_tokenId` is the uint256 cast of the keccak256 hash of the sender and receiver addresses
    /// @param _tokenId uint256(keccak256(abi.encode(sender, receiver)))
    /// @return The owner of the outflowing flow NFT with id `_tokenId`
    function ownerOf(uint256 _tokenId) public view returns (address) {
        return _flowDataBySenderReceiver[bytes32(_tokenId)].sender;
    }

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes calldata _data
    ) public {
        // safe transfer of nft
    }

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) public {
        // safe transfer of nft
    }

    function tokenURI(uint256 _tokenId) public view returns (string memory) {
        // get token uri
    }

    function supportsInterface(bytes4 _interfaceId) public view returns (bool) {
        // check if interface is supported
    }

    function _mint(address _to) internal {
        unchecked {
            _balances[_to] += 1;
        }
    }

    function _burn(address _sender) internal {
        unchecked {
            _balances[_sender] -= 1;
        }
    }
}
