// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ISuperfluidToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperfluid } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { IResolver } from "./interfaces/IResolver.sol";

contract StreamButlerResolver is IResolver, Ownable {
    IConstantFlowAgreementV1 public cfa;
    uint256 public endTime;
    ISuperfluidToken public token;
    address public flowSender;
    address public flowReceiver;

    constructor(
        uint256 _endTime,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _token,
        address _flowReceiver
    ) {
        endTime = _endTime;
        cfa = _cfa;
        token = _token;
        flowSender = msg.sender;
        flowReceiver = _flowReceiver;
    }

    function checker()
        external
        view
        returns (bool canExec, bytes memory execPayload)
    {
        canExec = block.timestamp >= endTime;

        bytes memory callData = abi.encodeWithSelector(
            IConstantFlowAgreementV1.deleteFlowByOperator.selector,
            token,
            flowSender,
            flowReceiver,
            new bytes(0)
        );

        execPayload = abi.encodeWithSelector(
            ISuperfluid.callAgreement.selector,
            IConstantFlowAgreementV1(cfa),
            callData,
            "0x"
        );
    }
}
