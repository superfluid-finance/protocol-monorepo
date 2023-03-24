// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

// solhint-disable not-rely-on-time

import {
    Time, Value, FlowRate, Unit,
    BasicParticle, bp_mempty,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    ISuperTokenPool, ISuperTokenPoolAdmin
} from "./ISuperTokenPool.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";


/**
 * @dev A toy implementation for proportional distribution pool.
 *
 * NOTE: Solidity public getter function for the storage fields do not support structs,
 *       hence their public getter are added manually instead.
 */
contract ToySuperTokenPool is Ownable, ISuperTokenPool {
    PDPoolIndex internal _index;
    mapping (address member => PDPoolMember member_data) internal _members;
    mapping (address member => Value claimed_value) internal _claimedValues;
    address public admin;
    Unit public pendingUnits;

    constructor (address admin_)
        Ownable()
    {
        admin = admin_;
    }

    function getIndex() override external view returns (PDPoolIndex memory) {
        return _index;
    }

    function getTotalUnits() override external view returns (Unit) {
        return _index.total_units;
    }

    function getUnits(address memberAddr) override external view returns (Unit) {
        return _members[memberAddr].owned_units;
    }

    function getDistributionFlowRate() override external view returns (FlowRate) {
        return _index.wrapped_particle.flow_rate.mul(_index.total_units);
    }

    function getPendingDistributionFlowRate() override external view returns (FlowRate) {
        return _index.wrapped_particle.flow_rate.mul(pendingUnits);
    }

    function getMemberFlowRate(address memberAddr) override external view returns (FlowRate) {
        Unit u = _members[memberAddr].owned_units;
        if (Unit.unwrap(u) == 0) return FlowRate.wrap(0);
        else return _index.wrapped_particle.flow_rate.mul(u);
    }

    function getPendingDistribution() external view returns (Value) {
        Time t = Time.wrap(uint32(block.timestamp));
        return _index.wrapped_particle.rtb(t).mul(pendingUnits);
    }

    function getClaimable(Time t, address memberAddr) override public view returns (Value) {
        return PDPoolMemberMU(_index, _members[memberAddr]).rtb(t) - _claimedValues[memberAddr];
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
        if (!ISuperTokenPoolAdmin(owner()).isMemberConnected(this, memberAddr)) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units + unit;
        }

        // update pool member's units
        BasicParticle memory p;
        (_index, _members[memberAddr], p) = PDPoolMemberMU(_index, _members[memberAddr])
            .pool_member_update(p, unit, t);
        {
            address[] memory addrs = new address[](1);addrs[0] = admin;
            BasicParticle[] memory ps = new BasicParticle[](1);ps[0] = p;
            assert(ISuperTokenPoolAdmin(owner()).absorbParticlesFromPool(addrs, ps));
        }

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
        // do a shift2 balance from the pool to the member
        {
            address[] memory addrs = new address[](2);addrs[0] = address(this);addrs[1] = memberAddr;
            BasicParticle[] memory ps = new BasicParticle[](2);
            (ps[0], ps[1]) = bp_mempty().shift2(bp_mempty(), c);
            assert(ISuperTokenPoolAdmin(owner()).absorbParticlesFromPool(addrs, ps));
        }
        _claimedValues[memberAddr] = _claimedValues[memberAddr] + c;
        return true;
    }

    function operatorSetIndex(PDPoolIndex calldata index) override external
        onlyOwner returns (bool)
    {
        _index = index;
        return true;
    }

    function operatorConnectMember(Time t, address memberAddr, bool doConnect) override external
        onlyOwner returns (bool)
    {
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
