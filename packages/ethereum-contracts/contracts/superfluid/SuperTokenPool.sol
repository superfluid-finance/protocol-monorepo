// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import {
    GeneralDistributionAgreementV1
} from "../agreements/GeneralDistributionAgreementV1.sol";
import { BeaconProxiable } from "../upgradability/BeaconProxiable.sol";

/**
 * @title SuperTokenPool
 * @author Superfluid
 * @notice A SuperTokenPool which can be used to distribute any SuperToken.
 */
contract SuperTokenPool is ISuperTokenPool, BeaconProxiable {
    using SemanticMoney for BasicParticle;
    using SafeCast for uint256;
    using SafeCast for int256;

    struct PoolIndexData {
        uint128 totalUnits;
        uint32 wrappedSettledAt;
        int96 wrappedFlowRate;
        int256 wrappedSettledValue;
    }

    struct MemberData {
        uint128 ownedUnits;
        uint32 syncedSettledAt;
        int96 syncedFlowRate;
        int256 syncedSettledValue;
        int256 settledValue;
        int256 claimedValue;
    }

    GeneralDistributionAgreementV1 internal immutable _gda;

    ISuperfluidToken public superToken;
    address public admin;
    PoolIndexData internal _index;
    mapping(address member => MemberData) internal _membersData;
    uint128 public pendingUnits;

    constructor(GeneralDistributionAgreementV1 gda) {
        _gda = gda;
    }

    function initialize(
        address admin_,
        ISuperfluidToken superToken_
    ) external initializer {
        admin = admin_;
        superToken = superToken_;
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.superfluid.SuperTokenPool.implementation"
            );
    }

    function getIndex() external view returns (PoolIndexData memory) {
        return _index;
    }

    function getTotalUnits() external view override returns (uint128) {
        return _index.totalUnits;
    }

    function getUnits(
        address memberAddr
    ) external view override returns (uint128) {
        return _membersData[memberAddr].ownedUnits;
    }

    function getDistributionFlowRate() external view override returns (int96) {
        return
            int96(
                _index.wrappedFlowRate * uint256(_index.totalUnits).toInt256()
            );
    }

    function getPendingDistributionFlowRate()
        external
        view
        override
        returns (int96)
    {
        return int96(_index.wrappedFlowRate * uint256(pendingUnits).toInt256());
    }

    function getMemberFlowRate(
        address memberAddr
    ) external view override returns (int96) {
        uint128 units = _membersData[memberAddr].ownedUnits;
        if (units == 0) return 0;
        else return int96(_index.wrappedFlowRate * uint256(units).toInt256());
    }

    function getPendingDistribution() external view returns (int256) {
        Time t = Time.wrap(uint32(block.timestamp));
        BasicParticle
            memory wrappedParticle = _getWrappedParticleFromPoolIndexData(
                _index
            );
        return
            Value.unwrap(
                wrappedParticle.rtb(t).mul(
                    Unit.wrap(uint256(pendingUnits).toInt256().toInt128())
                )
            );
    }

    function _getWrappedParticleFromPoolIndexData(
        PoolIndexData memory data
    ) internal pure returns (BasicParticle memory wrappedParticle) {
        wrappedParticle = BasicParticle({
            _settled_at: Time.wrap(data.wrappedSettledAt),
            _flow_rate: FlowRate.wrap(int128(data.wrappedFlowRate)),
            _settled_value: Value.wrap(data.wrappedSettledValue)
        });
    }

    function getPDPoolIndexFromPoolIndexData(
        PoolIndexData memory data
    ) public pure returns (PDPoolIndex memory pdPoolIndex) {
        pdPoolIndex = PDPoolIndex({
            total_units: Unit.wrap(
                uint256(data.totalUnits).toInt256().toInt128()
            ),
            _wrapped_particle: _getWrappedParticleFromPoolIndexData(data)
        });
    }

    function getPoolIndexDataFromPDPoolIndex(
        PDPoolIndex memory pdPoolIndex
    ) public pure returns (PoolIndexData memory data) {
        data = PoolIndexData({
            totalUnits: int256(Unit.unwrap(pdPoolIndex.total_units))
                .toUint256()
                .toUint128(),
            wrappedSettledAt: Time.unwrap(
                pdPoolIndex._wrapped_particle._settled_at
            ),
            wrappedFlowRate: int96(
                FlowRate.unwrap(pdPoolIndex._wrapped_particle._flow_rate)
            ),
            wrappedSettledValue: Value.unwrap(
                pdPoolIndex._wrapped_particle._settled_value
            )
        });
    }

    function getPDPoolMemberFromMemberData(
        MemberData memory memberData
    ) public pure returns (PDPoolMember memory pdPoolMember) {
        pdPoolMember = PDPoolMember({
            owned_units: Unit.wrap(
                uint256(memberData.ownedUnits).toInt256().toInt128()
            ),
            _synced_particle: BasicParticle({
                _settled_at: Time.wrap(memberData.syncedSettledAt),
                _flow_rate: FlowRate.wrap(int128(memberData.syncedFlowRate)),
                _settled_value: Value.wrap(memberData.syncedSettledValue)
            }),
            _settled_value: Value.wrap(memberData.settledValue)
        });
    }

    function getMemberDataFromPDPoolMember(
        PDPoolMember memory pdPoolMember
    ) public pure returns (MemberData memory memberData) {
        memberData = MemberData({
            ownedUnits: uint256(int256(Unit.unwrap(pdPoolMember.owned_units)))
                .toUint128(),
            syncedSettledAt: Time.unwrap(
                pdPoolMember._synced_particle._settled_at
            ),
            syncedFlowRate: int96(
                FlowRate.unwrap(pdPoolMember._synced_particle._flow_rate)
            ),
            syncedSettledValue: Value.unwrap(
                pdPoolMember._synced_particle._settled_value
            ),
            settledValue: Value.unwrap(pdPoolMember._settled_value),
            claimedValue: 0
        });
    }

    function getClaimableNow(
        address memberAddr
    )
        external
        view
        override
        returns (int256 claimableBalance, uint256 timestamp)
    {
        return (
            getClaimable(uint32(block.timestamp), memberAddr),
            block.timestamp
        );
    }

    function getClaimable(
        uint32 time,
        address memberAddr
    ) public view override returns (int256) {
        Time t = Time.wrap(time);
        PDPoolIndex memory pdPoolIndex = getPDPoolIndexFromPoolIndexData(
            _index
        );
        PDPoolMember memory pdPoolMember = getPDPoolMemberFromMemberData(
            _membersData[memberAddr]
        );
        return
            Value.unwrap(
                PDPoolMemberMU(pdPoolIndex, pdPoolMember).rtb(t) -
                    Value.wrap(_membersData[memberAddr].claimedValue)
            );
    }

    function updateMember(
        address memberAddr,
        uint128 unit
    ) external returns (bool) {
        if (unit < 0) revert SUPER_TOKEN_POOL_NEGATIVE_UNITS_NOT_SUPPORTED();
        if (admin != msg.sender) revert SUPER_TOKEN_POOL_NOT_POOL_ADMIN();

        uint32 time = uint32(block.timestamp);
        Time t = Time.wrap(time);
        Unit wrappedUnit = Unit.wrap(uint256(unit).toInt256().toInt128());

        // update pool's pending units
        if (!_gda.isMemberConnected(superToken, address(this), memberAddr)) {
            pendingUnits =
                pendingUnits -
                _membersData[memberAddr].ownedUnits +
                unit;
        }

        // update pool member's units
        BasicParticle memory p;
        PDPoolIndex memory pdPoolIndex = getPDPoolIndexFromPoolIndexData(
            _index
        );
        PDPoolMember memory pdPoolMember = getPDPoolMemberFromMemberData(
            _membersData[memberAddr]
        );
        (pdPoolIndex, pdPoolMember, p) = PDPoolMemberMU(
            pdPoolIndex,
            pdPoolMember
        ).pool_member_update(p, wrappedUnit, t);
        _index = getPoolIndexDataFromPDPoolIndex(pdPoolIndex);
        _membersData[memberAddr] = getMemberDataFromPDPoolMember(pdPoolMember);

        {
            address[] memory addresses = new address[](1);
            addresses[0] = admin;
            BasicParticle[] memory ps = new BasicParticle[](1);
            ps[0] = p;
            assert(_gda.absorbParticlesFromPool(superToken, addresses, ps));
        }

        // additional side effects of triggering claimAll
        claimAll(time, memberAddr);

        emit MemberUpdated(memberAddr, unit, block.timestamp);

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

    function claimAll(uint32 time, address memberAddr) public returns (bool) {
        if (time > block.timestamp) revert SUPER_TOKEN_POOL_INVALID_TIME();

        Value claimable = Value.wrap(getClaimable(time, memberAddr));
        {
            address[] memory addresses = new address[](2);
            addresses[0] = address(this);
            addresses[1] = memberAddr;
            BasicParticle[] memory particles = new BasicParticle[](2);
            BasicParticle memory mempty1;
            // @note this does not update the _settled_at field as no settle
            // occurs here
            (particles[0], particles[1]) = mempty1.shift2(mempty1, claimable);
            assert(
                _gda.absorbParticlesFromPool(superToken, addresses, particles)
            );
        }
        MemberData storage memberData = _membersData[memberAddr];
        memberData.claimedValue += Value.unwrap(claimable);

        emit DistributionClaimed(
            memberAddr,
            Value.unwrap(claimable),
            memberData.claimedValue,
            block.timestamp
        );

        return true;
    }

    function operatorSetIndex(
        PDPoolIndex calldata index
    ) external onlyGDA returns (bool) {
        _index = getPoolIndexDataFromPDPoolIndex(index);

        emit PoolIndexUpdated(
            _index.totalUnits,
            _index.wrappedSettledAt,
            _index.wrappedSettledValue,
            _index.wrappedFlowRate
        );

        return true;
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(
        uint32 time,
        address memberAddr,
        bool doConnect
    ) external onlyGDA returns (bool) {
        if (doConnect) {
            pendingUnits = pendingUnits - _membersData[memberAddr].ownedUnits;
        } else {
            pendingUnits = pendingUnits + _membersData[memberAddr].ownedUnits;
        }
        // trigger side effects of triggering claimAll
        claimAll(time, memberAddr);
        return true;
    }

    modifier onlyGDA() {
        if (msg.sender != address(_gda)) revert SUPER_TOKEN_POOL_NOT_GDA();
        _;
    }
}
