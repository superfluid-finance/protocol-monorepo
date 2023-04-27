// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

// solhint-disable not-rely-on-time

import { Initializable } from "@openzeppelin/contracts/proxy/utils/Initializable.sol";
import {
    Time, Value, FlowRate, Unit,
    BasicParticle,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "../SemanticMoney.sol";
import {
    ISuperfluidPool, ISuperfluidPoolAdmin
} from "./ISuperfluidPool.sol";


/**
 * @dev A toy implementation for proportional distribution pool.
 *
 * NOTE: Solidity public getter function for the storage fields do not support structs,
 *       hence their public getter are added manually instead.
 */
contract ToySuperfluidPool is Initializable, ISuperfluidPool {
    address public immutable POOL_ADMIN;

    address public admin;
    PDPoolIndex internal _pdpIndex;
    mapping (address member => PDPoolMember member_data) internal _members;
    mapping (address member => Value claimed_value) internal _claimedValues;
    Unit public pendingUnits;

    constructor () {
        POOL_ADMIN = msg.sender;
    }

    function initialize(address admin_) public initializer() {
        admin = admin_;
    }

    function getIndex() override external view returns (PDPoolIndex memory) {
        return _pdpIndex;
    }

    function getTotalUnits() override external view returns (Unit) {
        return _pdpIndex.total_units;
    }

    function getUnits(address memberAddr) override external view returns (Unit) {
        return _members[memberAddr].owned_units;
    }

    function getDistributionFlowRate() override external view returns (FlowRate) {
        return _pdpIndex.flow_rate_per_unit().mul(_pdpIndex.total_units);
    }

    function getPendingDistributionFlowRate() override external view returns (FlowRate) {
        return _pdpIndex.flow_rate_per_unit().mul(pendingUnits);
    }

    function getMemberFlowRate(address memberAddr) override external view returns (FlowRate) {
        Unit u = _members[memberAddr].owned_units;
        if (Unit.unwrap(u) == 0) return FlowRate.wrap(0);
        else return _pdpIndex.flow_rate_per_unit().mul(u);
    }

    function getPendingDistribution() external view returns (Value) {
        Time t = Time.wrap(uint32(block.timestamp));
        return _pdpIndex.rtb_per_unit(t).mul(pendingUnits);
    }

    function getClaimable(Time t, address memberAddr) override public view returns (Value) {
        return PDPoolMemberMU(_pdpIndex, _members[memberAddr]).rtb(t) - _claimedValues[memberAddr];
    }

    function getClaimable(address memberAddr) override external view returns (Value) {
        Time t = Time.wrap(uint32(block.timestamp));
        return getClaimable(t, memberAddr);
    }

    function updateMember(address memberAddr, Unit unit) override external returns (bool) {
        require(Unit.unwrap(unit) >= 0, "No negative unit amount!");
        require(admin == msg.sender, "Not pool admin!");
        Time t = Time.wrap(uint32(block.timestamp));

        // update pool's pending units
        if (!ISuperfluidPoolAdmin(POOL_ADMIN).isMemberConnected(this, memberAddr)) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units + unit;
        }

        // update pool member's units
        BasicParticle memory p;
        (_pdpIndex, _members[memberAddr], p) = PDPoolMemberMU(_pdpIndex, _members[memberAddr])
            .pool_member_update(p, unit, t);
        assert(ISuperfluidPoolAdmin(POOL_ADMIN).poolAddAdjustmentFlow(p.flow_rate(), t));

        // additional side effects of triggering claimAll
        _claimAll(t, memberAddr);
        return true;
    }

    function claimAll(address memberAddr) override public returns (bool) {
        Time t = Time.wrap(uint32(block.timestamp));
        return _claimAll(t, memberAddr);
    }

    function claimAll() override external returns (bool) {
        Time t = Time.wrap(uint32(block.timestamp));
        return _claimAll(t, msg.sender);
    }

    function _claimAll(Time t, address memberAddr) internal returns (bool) {
        Value c = getClaimable(t, memberAddr);
        assert(ISuperfluidPoolAdmin(POOL_ADMIN).poolSettleClaim(memberAddr, c));
        _claimedValues[memberAddr] = _claimedValues[memberAddr] + c;
        return true;
    }

    function operatorSetIndex(PDPoolIndex calldata index) override external
        returns (bool)
    {
        assert(POOL_ADMIN == msg.sender);

        _pdpIndex = index;
        return true;
    }

    function operatorConnectMember(Time t, address memberAddr, bool doConnect) override external
        returns (bool)
    {
        assert(POOL_ADMIN == msg.sender);

        if (doConnect) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units;
        } else {
            pendingUnits = pendingUnits + _members[memberAddr].owned_units;
        }

        // trigger side effects of triggering claimAll
        _claimAll(t, memberAddr);
        return true;
    }
}
