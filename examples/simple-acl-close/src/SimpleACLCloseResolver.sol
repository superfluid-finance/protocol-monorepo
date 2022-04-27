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
    IConstantFlowAgreementV1 internal cfa;
    uint256 internal endTime;
    ISuperfluidToken internal superToken;
    address internal flowSender;
    address internal flowReceiver;
    address internal ops;

    error OpsOnly();

    constructor(
        uint256 _endTime,
        IConstantFlowAgreementV1 _cfa,
        ISuperfluidToken _superToken,
        address _flowReceiver,
        address _ops
    ) {
        endTime = _endTime;
        cfa = _cfa;
        superToken = _superToken;
        flowSender = msg.sender;
        flowReceiver = _flowReceiver;
        ops = _ops;
    }

    modifier opsOnly() {
        if (msg.sender != ops) revert OpsOnly();
        _;
    }

    function checker()
        external
        view
        opsOnly
        returns (bool canExec, bytes memory execPayload)
    {
        canExec = block.timestamp >= endTime;

        bytes memory callData = abi.encodeWithSelector(
            IConstantFlowAgreementV1.deleteFlowByOperator.selector,
            superToken,
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
