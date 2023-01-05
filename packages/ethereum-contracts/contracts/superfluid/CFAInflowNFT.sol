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
        return _flowDataBySenderReceiver[bytes32(_tokenId)].receiver;
    }

    function transferFrom(
        address _from,
        address _to,
        uint256 _tokenId
    ) external override {
        // check if msg.sender has approval unless it is equal to _from
        FlowData storage flowData = _flowDataBySenderReceiver[bytes32(_tokenId)];
        (, int96 flowRate, ,) = cfa.getFlow(superToken, flowData.sender, flowData.receiver);
        delete _flowDataBySenderReceiver[bytes32(_tokenId)];

        // update the flowDataBySenderReceiver mapping
        _flowDataBySenderReceiver[keccak256(abi.encode(flowData.sender, _to))] = FlowData({
            sender: flowData.sender,
            receiver: _to,
            startDate: uint64(block.timestamp)
        });
        
        // delete the flow from sender to receiver
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
        // When an inflow NFT is transferred, this contract will, 
        // we also will need to update the _flowDataBySenderReceiver mapping
        // as well as the _balances mapping
        // and finally, we will need to emit the Transfer event
        // TODO: for inflow NFTs, we allow the owner to transfer the NFT whenever they want
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

    function _mint(address _sender, address _receiver) internal override {
        unchecked {
            _balances[_receiver] += 1;
        }
        _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _receiver))
        ] = FlowData(_sender, _receiver, uint64(block.timestamp));
    }

    function _burn(address _sender, address _receiver) internal override {
        unchecked {
            _balances[_receiver] -= 1;
        }
        delete _flowDataBySenderReceiver[
            keccak256(abi.encode(_sender, _receiver))
        ];
    }

    // CFA Functions

    function createFlow(
        address _sender,
        address _receiver,
        int96 _flowRate
    ) external override onlySuperToken {
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
        CFAOutflowNFT(SuperToken(address(superToken)).cfaOutflowNFT()).mint(
            _sender,
            _receiver
        );
        // mint an inflow nft to the flow receiver
        _mint(_sender, _receiver);
    }

    function deleteFlow(
        address _sender,
        address _receiver
    ) external override onlySuperToken {
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
        CFAOutflowNFT(SuperToken(address(superToken)).cfaOutflowNFT()).burn(
            _sender,
            _receiver
        );
        // burn the inflow nft to the flow receiver
        _burn(_sender, _receiver);
    }

    // CFA ACL Functions
}
