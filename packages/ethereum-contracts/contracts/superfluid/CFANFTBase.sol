// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;
// @note temporary
// solhint-disable no-empty-blocks
// solhint-disable no-unused-vars
// solhint-disable not-rely-on-time

import {
    BatchOperation,
    ISuperfluid,
    ISuperToken
} from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { CallUtils } from "../libs/CallUtils.sol";

abstract contract CFANFTBase {
    struct FlowData {
        address sender;
        address receiver;
    }

    ISuperToken public immutable superToken;
    IConstantFlowAgreementV1 public immutable cfa;
    ISuperfluid public immutable host;
    string public name;
    string public symbol;

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
        ISuperfluid _host,
        ISuperToken _superToken,
        IConstantFlowAgreementV1 _cfa,
        string memory _name,
        string memory _symbol
    ) {
        host = _host;
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

    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external virtual;

    function approve(address _to, uint256 _permissions) external virtual;

    function setApprovalForAll(address _operator, bool _approved) external virtual;

    /// @notice This function will always return the zero address
    /// @return address(0)
    function getApproved(
        uint256 _tokenId
    ) external view virtual returns (address);

    function isApprovedForAll(
        address _owner,
        address _operator
    ) external view returns (bool) {
        // check if approved for all nfts
    }

    /// @notice Explain to an end user what this does
    /// @dev Explain to a developer any extra details
    /// @param _owner a parameter just like in doxygen (must be followed by parameter name)
    /// @return The number of outflowing streams owned by `_owner`
    function balanceOf(address _owner) external view virtual returns (uint256);

    /// @notice Returns the owner of the NFT with id `_tokenId`
    /// @dev `_tokenId` is the uint256 cast of the keccak256 hash of the sender and receiver addresses
    /// @param _tokenId uint256(keccak256(abi.encode(sender, receiver)))
    /// @return The owner of the flow NFT with id `_tokenId`
    function ownerOf(uint256 _tokenId) external view virtual returns (address);

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId,
        bytes calldata _data
    ) external virtual;

    function safeTransferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external virtual;

    function tokenURI(
        uint256 _tokenId
    ) external view virtual returns (string memory);

    function supportsInterface(
        bytes4 _interfaceId
    ) external view returns (bool) {
        // check if interface is supported
    }

    // compiles the calldata of a single operation for the host invocation and executes it
    function _forwardTokenCall(
        address _sender,
        address target,
        bytes memory callData,
        bytes memory userData
    ) internal returns (bool) {
        ISuperfluid.Operation[] memory ops = new ISuperfluid.Operation[](1);
        ops[0] = ISuperfluid.Operation(
            BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
            address(target), // target
            abi.encode(callData, userData) // data
        );

        bytes memory fwBatchCallData = abi.encodeCall(
            host.trustedTokenBatchCall,
            (ops)
        );

        // https://eips.ethereum.org/EIPS/eip-2771
        // we encode the msg.sender as the last 20 bytes per EIP-2771 to extract the original txn signer later on
        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory returnedData) = address(host).call(
            abi.encodePacked(fwBatchCallData, _sender)
        );

        if (!success) {
            CallUtils.revertFromReturnedData(returnedData);
        }

        return true;
    }
}
