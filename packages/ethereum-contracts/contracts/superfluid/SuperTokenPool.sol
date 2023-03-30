// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import {
    GeneralDistributionAgreementV1
} from "../agreements/GeneralDistributionAgreementV1.sol";

/**
 * @title SuperTokenPool
 * @author Superfluid
 * @notice A SuperTokenPool which can be used to distribute any SuperToken.
 */
contract SuperTokenPool is Ownable, ISuperTokenPool {
    using SemanticMoney for BasicParticle;

    GeneralDistributionAgreementV1 internal immutable _gda;
    ISuperfluidToken public immutable _superToken;
    PDPoolIndex internal _index;
    address public admin;
    mapping(address member => PDPoolMember member_data) internal _members;
    mapping(address member => Value claimed_value) internal _claimedValues;
    Unit public pendingUnits;

    constructor(
        address admin_,
        GeneralDistributionAgreementV1 gda,
        ISuperfluidToken superToken
    ) Ownable() {
        admin = admin_;
        _gda = gda;
        _superToken = superToken;
    }

    function getIndex() external view returns (PDPoolIndex memory) {
        return _index;
    }

    function getTotalUnits() external view override returns (int128) {
        return Unit.unwrap(_index.total_units);
    }

    function getUnits(
        address memberAddr
    ) external view override returns (int128) {
        return Unit.unwrap(_members[memberAddr].owned_units);
    }

    function getDistributionFlowRate() external view override returns (int96) {
        // @note downcasting from int128 -> int96
        return
            int96(
                FlowRate.unwrap(
                    _index.wrapped_particle.flow_rate.mul(_index.total_units)
                )
            );
    }

    function getPendingDistributionFlowRate()
        external
        view
        override
        returns (int96)
    {
        return int96(FlowRate.unwrap(_index.wrapped_particle.flow_rate.mul(pendingUnits)));
    }

    function getMemberFlowRate(
        address memberAddr
    ) external view override returns (int96) {
        Unit u = _members[memberAddr].owned_units;
        if (Unit.unwrap(u) == 0) return 0;
        else return int96(FlowRate.unwrap(_index.wrapped_particle.flow_rate.mul(u)));
    }

    function getPendingDistribution() external view returns (int256) {
        Time t = Time.wrap(uint32(block.timestamp));
        return Value.unwrap(_index.wrapped_particle.rtb(t).mul(pendingUnits));
    }

    function getClaimable(address memberAddr) override external view returns (int256) {
        return getClaimable(uint32(block.timestamp), memberAddr);
    }

    function getClaimable(
        uint32 time,
        address memberAddr
    ) public view override returns (int256) {
        Time t = Time.wrap(time);
        return
            Value.unwrap(
                PDPoolMemberMU(_index, _members[memberAddr]).rtb(t) -
                    _claimedValues[memberAddr]
            );
    }

    function updateMember(
        address memberAddr,
        int128 unit
    ) external returns (bool) {
        if (unit < 0) revert SUPER_TOKEN_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
        if (admin != msg.sender) revert SUPER_TOKEN_POOL_NOT_POOL_ADMIN();

        uint32 time = uint32(block.timestamp);
        Time t = Time.wrap(time);
        Unit wrappedUnit = Unit.wrap(unit);

        // update pool's pending units
        if (!_gda.isMemberConnected(address(this), memberAddr)) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units + wrappedUnit;
        }

        // update pool member's units
        BasicParticle memory p;
        (_index, _members[memberAddr], p) = PDPoolMemberMU(_index, _members[memberAddr])
            .pool_member_update(p, wrappedUnit, t);
        {
            address[] memory addresses = new address[](1);
            addresses[0] = admin;
            BasicParticle[] memory ps = new BasicParticle[](1);
            ps[0] = p;
            assert(_gda.absorbParticlesFromPool(_superToken, addresses, ps));
        }

        // additional side effects of triggering claimAll
        claimAll(time, memberAddr);

        return true;
    }

    function claimAll() external returns (bool) {
        uint32 time = uint32(block.timestamp);
        return claimAll(time, msg.sender);
    }

    function claimAll(address memberAddr) external returns (bool) {
        uint32 time = uint32(block.timestamp);
        return claimAll(time, memberAddr);
    }

    function claimAll(
        uint32 time,
        address memberAddr
    ) public override returns (bool) {
        int256 claimable = getClaimable(time, memberAddr);
        Value wrappedClaimable = Value.wrap(claimable);
        {
            address[] memory addresses = new address[](2);
            addresses[0] = address(this);
            addresses[1] = memberAddr;
            BasicParticle[] memory ps = new BasicParticle[](2);
            BasicParticle memory mempty;
            (ps[0], ps[1]) = mempty.shift2(mempty, wrappedClaimable);
            assert(_gda.absorbParticlesFromPool(_superToken, addresses, ps));
        }
        _claimedValues[memberAddr] = wrappedClaimable;
        return true;
    }

    function operatorSetIndex(
        PDPoolIndex calldata index
    ) external onlyOwner returns (bool) {
        _index = index;
        return true;
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(
        uint32 time,
        address memberAddr,
        bool doConnect
    ) external onlyOwner returns (bool) {
        if (doConnect) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units;
        } else {
            pendingUnits = pendingUnits + _members[memberAddr].owned_units;
        }
        // trigger side effects of triggering claimAll
        claimAll(time, memberAddr);
        return true;
    }
}
