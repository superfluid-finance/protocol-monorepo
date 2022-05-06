// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "@superfluid-finance/ethereum-contracts/contracts/superfluid/Superfluid.sol";
import "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";

contract SuperfluidTester {

    Superfluid internal host;
    IERC20 internal token;
    ISuperToken internal superToken;
    CFAv1Library.InitData internal cfaLib;
    IDAv1Library.InitData internal idaLib;

    using CFAv1Library for CFAv1Library.InitData;
    using IDAv1Library for IDAv1Library.InitData;

    constructor (
        Superfluid host_,
        IConstantFlowAgreementV1 cfa_,
        IInstantDistributionAgreementV1 ida_,
        IERC20 token_,
        ISuperToken superToken_)
    {
        host = host_;
        token = token_;
        superToken = superToken_;
        cfaLib.host = host_;
        cfaLib.cfa = cfa_;
        idaLib.host = host_;
        idaLib.ida = ida_;
    }

    function upgradeSuperToken(uint256 amount) public {
        token.approve(address(superToken), amount);
        superToken.upgrade(amount);
    }

    function downgradeSuperToken(uint256 amount) public {
        superToken.downgrade(amount);
    }

    function flow(address receiver, int96 flowRate) public {
        cfaLib.flow(receiver, superToken, flowRate);
    }

    function createIndex(uint32 indexId) public {
        idaLib.createIndex(superToken, indexId);
    }

    function updateSubscriptionUnits(uint32 indexId, address subscriber, uint128 units) public {
        idaLib.updateSubscriptionUnits(superToken, indexId, subscriber, units);
    }

    function distribute(uint32 indexId, uint256 amount) public {
        idaLib.distribute(superToken, indexId, amount);
    }

    function approveSubscription(address publisher, uint32 indexId) public {
        idaLib.approveSubscription(superToken, publisher, indexId);
    }

}
