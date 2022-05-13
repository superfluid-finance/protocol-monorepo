// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "@superfluid-finance/ethereum-contracts/contracts/superfluid/Superfluid.sol";
import "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";

contract SuperfluidTester {

    SuperfluidFrameworkDeployer.Framework internal sf;
    IERC20 internal token;
    ISuperToken internal superToken;

    using CFAv1Library for CFAv1Library.InitData;
    using IDAv1Library for IDAv1Library.InitData;

    constructor (
        SuperfluidFrameworkDeployer.Framework memory sf_,
        IERC20 token_,
        ISuperToken superToken_)
    {
        sf = sf_;
        token = token_;
        superToken = superToken_;
    }

    function upgradeSuperToken(uint256 amount) public {
        token.approve(address(superToken), amount);
        superToken.upgrade(amount);
    }

    function downgradeSuperToken(uint256 amount) public {
        superToken.downgrade(amount);
    }

    function flow(address receiver, int96 flowRate) public {
        (, int96 currentFlowRate,,) = sf.cfa.getFlow(superToken, address(this), receiver);
        if (flowRate == 0) {
            sf.cfaLib.deleteFlow(address(this), receiver, superToken);
        } else if (currentFlowRate == 0) {
            sf.cfaLib.createFlow(receiver, superToken, flowRate);
        } else {
            sf.cfaLib.updateFlow(receiver, superToken, flowRate);
        }
    }

    function createIndex(uint32 indexId) public {
        sf.idaLib.createIndex(superToken, indexId);
    }

    function updateSubscriptionUnits(uint32 indexId, address subscriber, uint128 units) public {
        sf.idaLib.updateSubscriptionUnits(superToken, indexId, subscriber, units);
    }

    function distribute(uint32 indexId, uint256 amount) public {
        sf.idaLib.distribute(superToken, indexId, amount);
    }

    function approveSubscription(address publisher, uint32 indexId) public {
        sf.idaLib.approveSubscription(superToken, publisher, indexId);
    }

}
