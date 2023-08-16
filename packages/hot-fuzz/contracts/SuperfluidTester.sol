// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "@superfluid-finance/ethereum-contracts/contracts/superfluid/Superfluid.sol";
import "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import {ISuperfluidPool} from
    "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidPool.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";
import {SuperTokenV1Library} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperTokenV1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";

contract SuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    SuperfluidFrameworkDeployer.Framework internal sf;
    IERC20 internal token;
    ISuperToken internal superToken;

    constructor(SuperfluidFrameworkDeployer.Framework memory sf_, IERC20 token_, ISuperToken superToken_) {
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

    // CFA functions

    function flow(address receiver, int96 flowRate) public {
        (, int96 currentFlowRate,,) = sf.cfa.getFlow(superToken, address(this), receiver);
        if (flowRate == 0) {
            superToken.deleteFlow(address(this), receiver);
        } else if (currentFlowRate == 0) {
            superToken.createFlow(receiver, flowRate);
        } else {
            superToken.updateFlow(receiver, flowRate);
        }
    }

    // IDA functions
    function createIndex(uint32 indexId) public {
        superToken.createIndex(indexId);
    }

    function updateSubscriptionUnits(uint32 indexId, address subscriber, uint128 units) public {
        superToken.updateSubscriptionUnits(indexId, subscriber, units);
    }

    function distribute(uint32 indexId, uint256 amount) public {
        superToken.distribute(indexId, amount);
    }

    function approveSubscription(address publisher, uint32 indexId) public {
        superToken.approveSubscription(publisher, indexId);
    }

    // GDA functions
    function createPool(address admin) public returns (ISuperfluidPool pool) {
        pool = superToken.createPool(admin);
    }

    function connectPool(ISuperfluidPool pool) public {
        superToken.connectPool(pool);
    }

    function disconnectPool(ISuperfluidPool pool) public {
        superToken.disconnectPool(pool);
    }

    function distributeToPool(ISuperfluidPool pool, address from, uint256 requestedAmount) public {
        superToken.distributeToPool(from, pool, requestedAmount);
    }

    function distributeFlow(ISuperfluidPool pool, address from, int96 flowRate) public {
        superToken.distributeFlow(from, pool, flowRate);
    }

    // SuperfluidPool
    function updateMemberUnits(ISuperfluidPool pool, address member, uint128 units) public {
        pool.updateMemberUnits(member, units);
    }

    function claimAll(ISuperfluidPool pool) public {
        pool.claimAll();
    }

    function claimAll(ISuperfluidPool pool, address memberAddress) public {
        pool.claimAll(memberAddress);
    }

    // SuperfluidPool-ERC20
    function transfer(ISuperfluidPool pool, address to, uint256 amount) public {
        pool.transfer(to, amount);
    }

    function transferFrom(ISuperfluidPool pool, address from, address to, uint256 amount) public {
        pool.transferFrom(from, to, amount);
    }

    function increaseAllowance(ISuperfluidPool pool, address spender, uint256 addedValue) public {
        pool.increaseAllowance(spender, addedValue);
    }

    function decreaseAllowance(ISuperfluidPool pool, address spender, uint256 subtractedValue) public {
        pool.decreaseAllowance(spender, subtractedValue);
    }

    function approve(ISuperfluidPool pool, address spender, uint256 amount) public {
        pool.approve(spender, amount);
    }
}
