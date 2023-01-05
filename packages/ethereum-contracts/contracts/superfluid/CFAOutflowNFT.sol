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
import { CFAInflowNFT } from "./CFAInflowNFT.sol";

contract CFAOutflowNFT is CFANFTBase {
    // mapping from keccak256(abi.encode(sender, receiver)) to FlowData
    mapping(bytes32 => FlowData) internal _flowDataBySenderReceiver;

    // Mapping owner address to token count
    mapping(address => uint256) internal _balances;

    // Mapping from token ID to approved address
    // @note this only allows a single approved address per token
    mapping(uint256 => address) private _tokenApprovals;

    error CFA_OUTFLOW_NFT_ONLY_OWNER_OR_APPROVED();
    error CFA_OUTFLOW_FROM_FLOW_SENDER_MISMATCH();

    constructor(
        ISuperfluid _host,
        ISuperToken _superToken,
        IConstantFlowAgreementV1 _cfa,
        string memory _name,
        string memory _symbol
    ) CFANFTBase(_host, _superToken, _cfa, _name, _symbol) {}

    /// @notice Returns the owner of the outflowing flow with id `_tokenId`
    /// @dev `_tokenId` is the uint256 cast of the keccak256 hash of the sender and receiver addresses
    /// @param _tokenId uint256(keccak256(abi.encode(sender, receiver)))
    /// @return The owner of the outflowing flow NFT with id `_tokenId`
    function ownerOf(
        uint256 _tokenId
    ) external view override returns (address) {
        return _ownerOf(_tokenId);
    }

    /// outflow nft balance
    function balanceOf(
        address _owner
    ) external view override returns (uint256) {
        return _balances[_owner];
    }

    /// @notice Grant create flow ACL permissions to `_to` for the outflowing flow with id `_tokenId`
    /// @dev This grants ACL approval for the amount for the current flow rate of the NFT
    /// @param _to The address you want to provide ACL permissions for
    /// @param _tokenId The address you want to provide ACL permissions for
    function approve(address _to, uint256 _tokenId) external override {
        address owner = _ownerOf(_tokenId);
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
     * @dev Returns the owner of the `tokenId`. Does NOT revert if token doesn't exist
     */
    function _ownerOf(
        uint256 _tokenId
    ) internal view virtual returns (address) {
        return _flowDataBySenderReceiver[bytes32(_tokenId)].sender;
    }

    /// @notice Returns the address of the flow operator for the outflowing flow NFT with id `_tokenId`
    /// @dev Returns address(0) if caller has no permissions, otherwise it returns the msg.sender
    function getApproved(
        uint256 _tokenId
    ) public view override returns (address) {
        _requireMinted(_tokenId);
        return _tokenApprovals[_tokenId];
    }

    /// @notice Explain to an end user what this does
    /// @dev Explain to a developer any extra details
    /// @param _operator The address you want to grant or revoke ACL permissions for
    /// @param _approved Whether you want to grant (true) or revoke (false) permissions
    function setApprovalForAll(
        address _operator,
        bool _approved
    ) external override {
        // set approval for all nfts
        bytes memory callData;
        if (_approved == true) {
            callData = abi.encodeCall(
                cfa.authorizeFlowOperatorWithFullControl,
                (superToken, _operator, new bytes(0))
            );
        } else {
            callData = abi.encodeCall(
                cfa.revokeFlowOperatorWithFullControl,
                (superToken, _operator, new bytes(0))
            );
        }
        _forwardTokenCall(msg.sender, address(cfa), callData, new bytes(0));
    }

    /// @notice Allows transfer of an outflowing flow NFT with id `tokenId` from `_from` to `_to`
    /// @dev This means `_to` is now the owner of the outflowing flow NFT and the sender of the flow is now `_to`
    /// @param _from The address you want to transfer the outflowing flow NFT from
    /// @param _to The address you want to transfer the outflowing flow NFT to
    /// @param _tokenId The id of the outflowing flow NFT you want to transfer
    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external override {
        FlowData memory flowData = _flowDataBySenderReceiver[bytes32(_tokenId)];
        if (_from != flowData.sender) {
            revert CFA_OUTFLOW_FROM_FLOW_SENDER_MISMATCH();
        }

        if (msg.sender != _from && getApproved(_tokenId) != msg.sender) {
            revert CFA_OUTFLOW_NFT_ONLY_OWNER_OR_APPROVED();
        }

        // update the balances
        unchecked {
            _balances[_from] -= 1;
            _balances[_to] += 1;
        }
        _flowDataBySenderReceiver[
            keccak256(abi.encode(_to, flowData.receiver))
        ] = FlowData(_to, flowData.receiver);
        delete _flowDataBySenderReceiver[bytes32(_tokenId)];

        // get flow before deletion
        (, int96 flowRate, , ) = cfa.getFlow(
            superToken,
            flowData.sender,
            flowData.receiver
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

        bytes memory callData;

        // start with the case where we are an operator for the final flow creatod
        if (msg.sender != _to) {
            callData = abi.encodeCall(
                cfa.createFlowByOperator,
                (superToken, _to, flowData.receiver, flowRate, new bytes(0))
            );
        } else {
            callData = abi.encodeCall(
                cfa.createFlow,
                (superToken, flowData.receiver, flowRate, new bytes(0))
            );
        }
        _forwardTokenCall(
            msg.sender,
            address(cfa),
            callData,
            new bytes(0)
        );
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

    function mint(address _sender, address _receiver) external {
        _mint(_sender, _receiver);
    }

    function burn(address _sender, address _receiver) external {
        _burn(_sender, _receiver);
    }

    function _mint(address _sender, address _receiver) internal {
        unchecked {
            _balances[_sender] += 1;
            _balances[_receiver] += 1;
        }
        _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _receiver))
        ] = FlowData(_sender, _receiver);
    }

    function _burn(address _sender, address _receiver) internal {
        unchecked {
            _balances[_sender] -= 1;
            _balances[_receiver] -= 1;
        }

        delete _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _receiver))
        ];
    }

    function handleInflowTransfer(
        address _sender,
        address _oldReceiver,
        address _newReceiver
    ) external {
        unchecked {
            _balances[_oldReceiver] -= 1;
            _balances[_newReceiver] += 1;
        }
        delete _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _oldReceiver))
        ];
        _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _newReceiver))
        ] = FlowData({ sender: _sender, receiver: _newReceiver });
    }

    function getFlowDataByTokenId(
        uint256 _tokenId
    ) external view returns (FlowData memory) {
        return _flowDataBySenderReceiver[bytes32(_tokenId)];
    }

    /**************************************************************************
     * CFAv1 Operations
     *************************************************************************/
    function createFlow(
        address _sender,
        address _receiver,
        int96 _flowRate
    ) external onlySuperToken {
        bytes memory createFlowCallData = abi.encodeCall(
            cfa.createFlow,
            (superToken, _receiver, _flowRate, new bytes(0))
        );
        _forwardTokenCall(
            _sender,
            address(cfa),
            createFlowCallData,
            new bytes(0)
        );

        // mint an outflow nft to the flow sender
        _mint(_sender, _receiver);
    }

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

    function deleteFlow(
        address _sender,
        address _receiver
    ) external onlySuperToken {
        bytes memory deleteFlowCallData = abi.encodeCall(
            cfa.deleteFlow,
            (superToken, _sender, _receiver, new bytes(0))
        );
        _forwardTokenCall(
            _sender,
            address(cfa),
            deleteFlowCallData,
            new bytes(0)
        );

        // burn the outflow nft to the flow sender
        _burn(_sender, _receiver);
    }

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
}
