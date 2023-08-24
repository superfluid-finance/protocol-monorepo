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

    // ERC20 Functions
    // TODO: transfer, approve, etc.
    // SuperToken Functions
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

    function setFlowPermissions(
        address flowOperator,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowRateAllowance
    ) public {
        superToken.setFlowPermissions(flowOperator, allowCreate, allowUpdate, allowDelete, flowRateAllowance);
    }

    function setMaxFlowPermissions(address flowOperator) public {
        superToken.setMaxFlowPermissions(flowOperator);
    }

    function revokeFlowPermissions(address flowOperator) public {
        superToken.revokeFlowPermissions(flowOperator);
    }

    function increaseFlowRateAllowance(address flowOperator, int96 addedFlowRateAllowance) public {
        superToken.increaseFlowRateAllowance(flowOperator, addedFlowRateAllowance);
    }

    function decreaseFlowRateAllowance(address flowOperator, int96 subtractedFlowRateAllowance) public {
        superToken.decreaseFlowRateAllowance(flowOperator, subtractedFlowRateAllowance);
    }

    function increaseFlowRateAllowanceWithPermissions(
        address flowOperator,
        uint8 permissionsToAdd,
        int96 addedFlowRateAllowance
    ) public {
        superToken.increaseFlowRateAllowanceWithPermissions(flowOperator, permissionsToAdd, addedFlowRateAllowance);
    }

    function decreaseFlowRateAllowanceWithPermissions(
        address flowOperator,
        uint8 permissionsToRemove,
        int96 subtractedFlowRateAllowance
    ) public {
        superToken.decreaseFlowRateAllowanceWithPermissions(
            flowOperator,
            permissionsToRemove,
            subtractedFlowRateAllowance
        );
    }

    // IDA functions
    function createIndex(uint32 indexId) public {
        superToken.createIndex(indexId);
    }

    function updateSubscriptionUnits(uint32 indexId, address subscriber, uint128 units) public {
        superToken.updateSubscriptionUnits(indexId, subscriber, units);
    }

    function updateIndex(uint32 indexId, uint128 indexValue) public {
        superToken.updateIndexValue(indexId, indexValue);
    }

    function distribute(uint32 indexId, uint256 amount) public {
        superToken.distribute(indexId, amount);
    }

    function approveSubscription(address publisher, uint32 indexId) public {
        superToken.approveSubscription(publisher, indexId);
    }

    function revokeSubscription(address publisher, uint32 indexId) public {
        superToken.revokeSubscription(publisher, indexId);
    }

    function deleteSubscription(address publisher, uint32 indexId, address subscriber) public {
        superToken.deleteSubscription(publisher, indexId, subscriber);
    }

    function claim(address publisher, uint32 indexId, address subscriber) public {
        superToken.claim(publisher, indexId, subscriber);
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
