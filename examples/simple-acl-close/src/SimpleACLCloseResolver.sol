// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ISuperfluidToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperfluid } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { IResolver } from "./interfaces/IResolver.sol";

/**
 * @title A simple Gelato resolver which utilizes Superfluid's ACL feature to close a single stream.
 * @author Superfluid
 * @dev This only works for a single token and one stream at a time.
 */
contract SimpleACLCloseResolver is IResolver, Ownable {
    IConstantFlowAgreementV1 public cfa;
    ISuperfluidToken public superToken;

    uint256 public endTime;
    address public flowSender;
    address public flowReceiver;

    error InvalidEndTime();
    error InvalidFlowReceiver();
    error InvalidFlowSender();

    event EndTimeUpdated(address indexed currentOwner, uint256 endTime);
    event FlowReceiverUpdated(
        address indexed currentOwner,
        address flowReceiver
    );
    event FlowSenderUpdated(address indexed currentOwner, address flowSender);

    constructor(
        uint256 _endTime,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _superToken,
        address _flowSender,
        address _flowReceiver
    ) {
        if (_endTime < block.timestamp) revert InvalidEndTime();
        endTime = _endTime;
        cfa = _cfa;
        superToken = _superToken;
        flowSender = _flowSender;
        flowReceiver = _flowReceiver;
    }

    function updateEndTime(uint256 _endTime) external onlyOwner {
        if (_endTime < block.timestamp) revert InvalidEndTime();
        endTime = _endTime;

        emit EndTimeUpdated(msg.sender, _endTime);
    }

    function updateFlowReceiver(address _flowReceiver) external onlyOwner {
        if (_flowReceiver == flowSender || _flowReceiver == address(0))
            revert InvalidFlowReceiver();
        flowReceiver = _flowReceiver;

        emit FlowReceiverUpdated(msg.sender, _flowReceiver);
    }

    function updateFlowSender(address _flowSender) external onlyOwner {
        if (_flowSender == flowReceiver || _flowSender == address(0))
            revert InvalidFlowSender();
        flowSender = _flowSender;

        emit FlowSenderUpdated(msg.sender, _flowSender);
    }

    function checker()
        external
        view
        returns (bool canExec, bytes memory execPayload)
    {
        // timestamp == 0 means the flow doesn't exist so it won't try to execute
        (uint256 timestamp, , , ) = cfa.getFlow(
            superToken,
            flowSender,
            flowReceiver
        );

        // NOTE: this can be modified to execute based on different conditions
        // e.g. supertoken balance reaches a specific amount
        canExec = block.timestamp >= endTime && timestamp != 0;

        bytes memory callData = abi.encodeCall(
            cfa.deleteFlowByOperator,
            (
                superToken,
                flowSender,
                flowReceiver,
                new bytes(0)
            )
        );

        // NOTE: this can be modified to execute pretty much any function
        // given the permissions
        // e.g. other host contract functions, supertoken upgrades/downgrades
        execPayload = abi.encodeCall(
            ISuperfluid(superToken.getHost()).callAgreement,
            (
                IConstantFlowAgreementV1(cfa),
                callData,
                "0x"
            )
        );
    }
}
