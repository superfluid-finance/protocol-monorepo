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
        uint64 startDate;
    }

    ISuperToken public immutable superToken;
    IConstantFlowAgreementV1 public immutable cfa;
    ISuperfluid public immutable host;
    string public name;
    string public symbol;

    // Mapping owner address to token count
    mapping(address => uint256) internal _balances;

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

    function mint(address _sender, address _receiver) external {
        _mint(_sender, _receiver);
    }

    function burn(address _sender, address _receiver) external {
        _burn(_sender, _receiver);
    }

    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external virtual;

    /// @notice Explain to an end user what this does
    /// @dev _permissions contains the permissions and the granted flowRateAllowance in the following format:
    /// WORD A: | reserved  | permissions | reserved | flowRateAllowance |
    ///         | 120       | 8           | 32       | 96                |
    /// NOTE: This is consistent with the format of our flow operator data.
    /// @param _to The address you want to provide ACL permissions for
    /// @param _permissions The address you want to provide ACL permissions for
    function approve(address _to, uint256 _permissions) external {
        // approve an nft
    }

    /// @notice Explain to an end user what this does
    /// @dev Explain to a developer any extra details
    /// @param _operator The address you want to grant or revoke ACL permissions for
    /// @param _approved Whether you want to grant (true) or revoke (false) permissions
    function setApprovalForAll(address _operator, bool _approved) external {
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
    ) external pure returns (address) {
        return address(0);
    }

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
    function balanceOf(address _owner) external view returns (uint256) {
        return _balances[_owner];
    }

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

    function _mint(address _sender, address _receiver) internal virtual;

    function _burn(address _sender, address _receiver) internal virtual;

    /**************************************************************************
     * CFAv1 Operations
     *************************************************************************/
    function createFlow(
        address _sender,
        address _receiver,
        int96 _flowRate
    ) external virtual;

    // function createFlow(
    //     address _sender,
    //     address _receiver,
    //     int96 _flowRate,
    //     bytes memory _userData
    // ) external virtual;

    function updateFlow(
        address _sender,
        address _receiver,
        int96 _flowRate
    ) external onlySuperToken {
        bytes memory updateFlowCallData = abi.encodeCall(
            cfa.updateFlow,
            (superToken, _receiver, _flowRate, new bytes(0))
        );
        _forwardTokenCall(
            _sender,
            address(cfa),
            updateFlowCallData,
            new bytes(0)
        );
    }

    // function updateFlow(
    //     address _sender,
    //     address _receiver,
    //     int96 _flowRate,
    //     bytes memory _userData
    // ) external onlySuperToken {
    //     bytes memory updateFlowCallData = abi.encodeCall(
    //         cfa.updateFlow,
    //         (superToken, _receiver, _flowRate, new bytes(0))
    //     );
    //     _forwardTokenCall(_sender, address(cfa), updateFlowCallData, _userData);
    // }

    function deleteFlow(address _sender, address _receiver) external virtual;

    // function deleteFlow(
    //     address _sender,
    //     address _receiver,
    //     bytes memory _userData
    // ) external virtual;

    /**
     * @dev Update permissions for flow operator
     * @param _sender The executor of the call (msg.sender)
     * @param flowOperator The address given flow permissions
     * @param allowCreate creation permissions
     * @param allowCreate update permissions
     * @param allowCreate deletion permissions
     * @param flowRateAllowance The allowance provided to flowOperator
     */
    function setFlowPermissions(
        address _sender,
        address flowOperator,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowRateAllowance
    ) external returns (bool) {
        uint8 permissionsBitmask = (allowCreate ? 1 : 0) |
            ((allowUpdate ? 1 : 0) << 1) |
            ((allowDelete ? 1 : 0) << 2);
        bytes memory updateFlowOperatorPermissionsCallData = abi.encodeCall(
            cfa.updateFlowOperatorPermissions,
            (
                superToken,
                flowOperator,
                permissionsBitmask,
                flowRateAllowance,
                new bytes(0)
            )
        );

        _forwardTokenCall(
            _sender,
            address(cfa),
            updateFlowOperatorPermissionsCallData,
            new bytes(0)
        );
        return true;
    }

    function setMaxFlowPermissions(
        address _sender,
        address flowOperator
    ) external returns (bool) {
        bytes memory authorizeFlowOperatorWithFullControlCallData = abi
            .encodeCall(
                cfa.authorizeFlowOperatorWithFullControl,
                (superToken, flowOperator, new bytes(0))
            );

        _forwardTokenCall(
            _sender,
            address(cfa),
            authorizeFlowOperatorWithFullControlCallData,
            new bytes(0)
        );

        return true;
    }

    function revokeFlowPermissions(
        address _sender,
        address flowOperator
    ) external returns (bool) {
        bytes memory revokeFlowOperatorWithFullControlCallData = abi.encodeCall(
            cfa.revokeFlowOperatorWithFullControl,
            (superToken, flowOperator, new bytes(0))
        );

        _forwardTokenCall(
            _sender,
            address(cfa),
            revokeFlowOperatorWithFullControlCallData,
            new bytes(0)
        );
        return true;
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
