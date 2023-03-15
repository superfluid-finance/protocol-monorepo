// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import {
    BasicParticle,
    SemanticMoney,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
    Time,
    Unit,
    Value
} from "../libs/SemanticMoney.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import { GeneralDistributionAgreementV1 } from "../agreements/GeneralDistributionAgreementV1.sol";

/**
 * @title SuperTokenPool
 * @author Superfluid
 * @notice A SuperTokenPool which can be used to distribute any SuperToken.
 */
contract SuperTokenPool is Ownable, ISuperTokenPool {
    using SemanticMoney for BasicParticle;

    error SUPER_TOKEN_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
    error SUPER_TOKEN_POOL_NOT_POOL_ADMIN();

    GeneralDistributionAgreementV1 internal immutable _gda;
    ISuperfluidToken internal immutable _superToken;
    PDPoolIndex internal _index;
    address public admin;
    mapping(address member => PDPoolMember member_data) internal _members;
    mapping(address member => Value claimed_value) internal _claimedValues;
    Unit public pendingUnits;

    constructor(address admin_, GeneralDistributionAgreementV1 gda, ISuperfluidToken superToken) Ownable() {
        admin = admin_;
        _gda = gda;
        _superToken = superToken;
    }

    function getPendingDistribution() external view returns (int256) {
        Time t = Time.wrap(uint32(block.timestamp));
        return Value.unwrap(_index.wrapped_particle.rtb(t).mul(pendingUnits));
    }

    function getIndex() external view returns (PDPoolIndex memory) {
        return _index;
    }

    function updateMember(address memberAddr, int128 unit) external returns (bool) {
        if (unit < 0) revert SUPER_TOKEN_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
        if (admin != msg.sender) revert SUPER_TOKEN_POOL_NOT_POOL_ADMIN();
        uint32 time = uint32(block.timestamp);
        Time t = Time.wrap(time);

        // update pool's pending units
        Unit wrappedUnit = Unit.wrap(unit);
        if (!_gda.isMemberConnected(address(this), memberAddr)) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units + wrappedUnit;
        }

        // update pool member's units
        BasicParticle memory p;
        (_index, _members[memberAddr], p) =
            PDPoolMemberMU(_index, _members[memberAddr]).pool_member_update(p, wrappedUnit, t);
        // this absorb particle from pool function is used for updating the universal index
        // it handles the flow rate discrepancy
        assert(_gda.absorbParticleFromPool(_superToken, admin, p));

        // additional side effects of triggering claimAll
        claimAll(time, memberAddr);

        return true;
    }

    function getClaimable(uint32 time, address memberAddr) public view override returns (int256) {
        Time t = Time.wrap(time);
        return Value.unwrap(PDPoolMemberMU(_index, _members[memberAddr]).rtb(t) - _claimedValues[memberAddr]);
    }

    function claimAll(uint32 time, address memberAddr) public override returns (bool) {
        int256 claimable = getClaimable(time, memberAddr);
        assert(_gda.shift(_superToken, address(this), memberAddr, claimable));
        _claimedValues[memberAddr] = Value.wrap(claimable);
        return true;
    }

    function claimAll() external returns (bool) {
        uint32 time = uint32(block.timestamp);
        return claimAll(time, msg.sender);
    }

    function operatorSetIndex(PDPoolIndex calldata index) external onlyOwner returns (bool) {
        _index = index;
        return true;
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(uint32 time, address memberAddr, bool doConnect) external onlyOwner returns (bool) {
        if (doConnect) {
            pendingUnits = pendingUnits - _members[memberAddr].owned_units;
        } else {
            pendingUnits = pendingUnits + _members[memberAddr].owned_units;
        }
        // additional side effects of triggering claimAlloperatorSetIndex
        claimAll(time, memberAddr);
        return true;
    }
}
