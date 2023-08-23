// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity ^0.8.0;

import {
    ISuperfluid, ISuperToken, IConstantFlowAgreementV1, IERC20
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import { IManager } from "../interfaces/IManager.sol";


contract MockReceiverContract {

    IConstantFlowAgreementV1 immutable public cfa;
    ISuperfluid immutable public host;

    constructor(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa
    ) {
        cfa = _cfa;
        host = _host;
    }

    function approve(IERC20 token, address spender, uint256 amount) public {
        token.approve(spender, amount);
    }

    function createFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate
    ) public {
        host.callAgreement(
            cfa,
            abi.encodeWithSelector(
                cfa.createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0)
            ),
            "0x"
        );
    }

    function upgrade(ISuperToken token, uint256 amount) external {
        token.upgrade(amount);
    }

    function createWrapSchedule(
        IManager manager,
        address superToken,
        address strategy,
        address liquidityToken,
        uint64 expiry,
        uint64 lowerLimit,
        uint64 upperLimit
    ) public {
        manager.createWrapSchedule(superToken, strategy, liquidityToken, expiry, lowerLimit, upperLimit);
    }
}
