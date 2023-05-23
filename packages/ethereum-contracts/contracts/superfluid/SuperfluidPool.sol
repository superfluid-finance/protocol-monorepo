// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {SafeCast} from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {ISuperfluidToken} from "../interfaces/superfluid/ISuperfluidToken.sol";
import {ISuperToken} from "../interfaces/superfluid/ISuperToken.sol";
import {ISuperfluidPool} from "../interfaces/superfluid/ISuperfluidPool.sol";
import {GeneralDistributionAgreementV1} from "../agreements/GeneralDistributionAgreementV1.sol";
import {BeaconProxiable} from "../upgradability/BeaconProxiable.sol";

/**
 * @title SuperfluidPool
 * @author Superfluid
 * @notice A SuperfluidPool which can be used to distribute any SuperToken.
 */
contract SuperfluidPool is ISuperfluidPool, BeaconProxiable {
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

    GeneralDistributionAgreementV1 public immutable GDA;

    ISuperfluidToken public superToken;
    address public admin;
    PoolIndexData internal _index;
    mapping(address => MemberData) internal _membersData;
    /// @dev This is a pseudo member, representing all the disconnected members
    MemberData internal _disconnectedMembers;

    constructor(GeneralDistributionAgreementV1 gda) {
        GDA = gda;
    }

    function initialize(address admin_, ISuperfluidToken superToken_) external initializer {
        admin = admin_;
        superToken = superToken_;
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperfluidPool.implementation");
    }

    function getIndex() external view returns (PoolIndexData memory) {
        return _index;
    }

    /// @inheritdoc ISuperfluidPool
    function getTotalUnits() external view override returns (uint128) {
        return _index.totalUnits;
    }

    /// @inheritdoc ISuperfluidPool
    function getTotalConnectedUnits() external view override returns (uint128) {
        return _index.totalUnits - _disconnectedMembers.ownedUnits;
    }

    /// @inheritdoc ISuperfluidPool
    function getTotalDisconnectedUnits() external view override returns (uint128) {
        return _disconnectedMembers.ownedUnits;
    }

    /// @inheritdoc ISuperfluidPool
    function getUnits(address memberAddr) external view override returns (uint128) {
        return _membersData[memberAddr].ownedUnits;
    }

    /// @inheritdoc ISuperfluidPool
    function getTotalConnectedFlowRate() external view override returns (int96) {
        return (_index.wrappedFlowRate * uint256(_index.totalUnits).toInt256()).toInt96();
    }

    /// @inheritdoc ISuperfluidPool
    function getTotalDisconnectedFlowRate() external view override returns (int96 flowRate) {
        PDPoolIndex memory pdPoolIndex = poolIndexDataToPDPoolIndex(_index);
        PDPoolMember memory disconnectedMembers = _memberDataToPDPoolMember(_disconnectedMembers);

        return int256(FlowRate.unwrap(pdPoolIndex.flow_rate_per_unit().mul(disconnectedMembers.owned_units))).toInt96();
    }

    /// @inheritdoc ISuperfluidPool
    function getDisconnectedBalance(uint32 time) external view override returns (int256 balance) {
        PDPoolIndex memory pdPoolIndex = poolIndexDataToPDPoolIndex(_index);
        PDPoolMember memory pdPoolMember = _memberDataToPDPoolMember(_disconnectedMembers);
        return Value.unwrap(PDPoolMemberMU(pdPoolIndex, pdPoolMember).rtb(Time.wrap(time)));
    }

    /// @inheritdoc ISuperfluidPool
    function getMemberFlowRate(address memberAddr) external view override returns (int96) {
        uint128 units = _membersData[memberAddr].ownedUnits;
        if (units == 0) return 0;
        else return (_index.wrappedFlowRate * uint256(units).toInt256()).toInt96();
    }

    function _poolIndexDataToWrappedParticle(PoolIndexData memory data)
        internal
        pure
        returns (BasicParticle memory wrappedParticle)
    {
        wrappedParticle = BasicParticle({
            _settled_at: Time.wrap(data.wrappedSettledAt),
            _flow_rate: FlowRate.wrap(int128(data.wrappedFlowRate)), // upcast from int96 is safe
            _settled_value: Value.wrap(data.wrappedSettledValue)
        });
    }

    function poolIndexDataToPDPoolIndex(PoolIndexData memory data)
        public
        pure
        returns (PDPoolIndex memory pdPoolIndex)
    {
        pdPoolIndex = PDPoolIndex({
            total_units: Unit.wrap(uint256(data.totalUnits).toInt256().toInt128()),
            _wrapped_particle: _poolIndexDataToWrappedParticle(data)
        });
    }

    function _pdPoolIndexToPoolIndexData(PDPoolIndex memory pdPoolIndex)
        internal
        pure
        returns (PoolIndexData memory data)
    {
        data = PoolIndexData({
            totalUnits: int256(Unit.unwrap(pdPoolIndex.total_units)).toUint256().toUint128(),
            wrappedSettledAt: Time.unwrap(pdPoolIndex.settled_at()),
            wrappedFlowRate: int256(FlowRate.unwrap(pdPoolIndex.flow_rate_per_unit())).toInt96(),
            wrappedSettledValue: Value.unwrap(pdPoolIndex._wrapped_particle._settled_value)
        });
    }

    function _memberDataToPDPoolMember(MemberData memory memberData)
        internal
        pure
        returns (PDPoolMember memory pdPoolMember)
    {
        pdPoolMember = PDPoolMember({
            owned_units: Unit.wrap(uint256(memberData.ownedUnits).toInt256().toInt128()),
            _synced_particle: BasicParticle({
                _settled_at: Time.wrap(memberData.syncedSettledAt),
                _flow_rate: FlowRate.wrap(int128(memberData.syncedFlowRate)), // upcast from int96 is safe
                _settled_value: Value.wrap(memberData.syncedSettledValue)
            }),
            _settled_value: Value.wrap(memberData.settledValue)
        });
    }

    function _pdPoolMemberToMemberData(PDPoolMember memory pdPoolMember, int256 claimedValue)
        internal
        pure
        returns (MemberData memory memberData)
    {
        memberData = MemberData({
            ownedUnits: uint256(int256(Unit.unwrap(pdPoolMember.owned_units))).toUint128(),
            syncedSettledAt: Time.unwrap(pdPoolMember._synced_particle._settled_at),
            syncedFlowRate: int256(FlowRate.unwrap(pdPoolMember._synced_particle._flow_rate)).toInt96(),
            syncedSettledValue: Value.unwrap(pdPoolMember._synced_particle._settled_value),
            settledValue: Value.unwrap(pdPoolMember._settled_value),
            claimedValue: claimedValue
        });
    }

    /// @inheritdoc ISuperfluidPool
    function getClaimableNow(address memberAddr)
        external
        view
        override
        returns (int256 claimableBalance, uint256 timestamp)
    {
        // TODO, GDA.getHost().getTimestamp() should be used in principle
        return (getClaimable(memberAddr, uint32(block.timestamp)), block.timestamp);
    }

    /// @inheritdoc ISuperfluidPool
    function getClaimable(address memberAddr, uint32 time) public view override returns (int256) {
        Time t = Time.wrap(time);
        PDPoolIndex memory pdPoolIndex = poolIndexDataToPDPoolIndex(_index);
        PDPoolMember memory pdPoolMember = _memberDataToPDPoolMember(_membersData[memberAddr]);
        return Value.unwrap(
            PDPoolMemberMU(pdPoolIndex, pdPoolMember).rtb(t) - Value.wrap(_membersData[memberAddr].claimedValue)
        );
    }

    /// @inheritdoc ISuperfluidPool
    function updateMember(address memberAddr, uint128 newUnits) external returns (bool) {
        if (admin != msg.sender) revert SUPERFLUID_POOL_NOT_POOL_ADMIN();

        if (GDA.isPool(superToken, memberAddr)) revert SUPERFLUID_POOL_NO_POOL_MEMBERS();

        // TODO, GDA.getHost().getTimestamp() should be used in principle
        uint32 time = uint32(block.timestamp);
        Time t = Time.wrap(time);
        Unit wrappedUnits = Unit.wrap(uint256(newUnits).toInt256().toInt128());

        PDPoolIndex memory pdPoolIndex = poolIndexDataToPDPoolIndex(_index);
        PDPoolMember memory pdPoolMember = _memberDataToPDPoolMember(_membersData[memberAddr]);
        PDPoolMemberMU memory mu = PDPoolMemberMU(pdPoolIndex, pdPoolMember);

        // update pool's disconnected units
        if (!GDA.isMemberConnected(superToken, address(this), memberAddr)) {
            // trigger the side effect of claiming all if not connected
            int256 claimedAmount = _claimAll(memberAddr, time);

            // update pool's disconnected units
            _shiftDisconnectedUnits(wrappedUnits - mu.m.owned_units, Value.wrap(claimedAmount), t);
        }

        // update pool member's units
        {
            BasicParticle memory p;
            (pdPoolIndex, pdPoolMember, p) = mu.pool_member_update(p, wrappedUnits, t);
            _index = _pdPoolIndexToPoolIndexData(pdPoolIndex);
            int256 claimedValue = _membersData[memberAddr].claimedValue;
            _membersData[memberAddr] = _pdPoolMemberToMemberData(pdPoolMember, claimedValue);
            assert(GDA.appendIndexUpdateByPool(superToken, p, t));
        }
        emit MemberUpdated(memberAddr, newUnits);

        return true;
    }

    function _claimAll(address memberAddr, uint32 time) internal returns (int256 amount) {
        amount = getClaimable(memberAddr, time);
        assert(GDA.poolSettleClaim(superToken, memberAddr, (amount)));
        _membersData[memberAddr].claimedValue += amount;

        emit DistributionClaimed(memberAddr, amount, _membersData[memberAddr].claimedValue);
    }

    /// @inheritdoc ISuperfluidPool
    function claimAll() external returns (bool) {
        return claimAll(msg.sender);
    }

    /// @inheritdoc ISuperfluidPool
    function claimAll(address memberAddr) public returns (bool) {
        bool isConnected = GDA.isMemberConnected(superToken, address(this), memberAddr);
        // TODO, GDA.getHost().getTimestamp() should be used in principle
        uint32 time = uint32(block.timestamp);
        int256 claimedAmount = _claimAll(memberAddr, time);
        if (!isConnected) {
            _shiftDisconnectedUnits(Unit.wrap(0), Value.wrap(claimedAmount), Time.wrap(time));
        }

        return true;
    }

    function operatorSetIndex(PDPoolIndex calldata index) external onlyGDA returns (bool) {
        _index = _pdPoolIndexToPoolIndexData(index);

        return true;
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(address memberAddr, bool doConnect, uint32 time) external onlyGDA returns (bool) {
        int256 claimedAmount = _claimAll(memberAddr, time);
        int128 units = uint256(_membersData[memberAddr].ownedUnits).toInt256().toInt128();
        if (doConnect) {
            _shiftDisconnectedUnits(Unit.wrap(-units), Value.wrap(claimedAmount), Time.wrap(time));
        } else {
            _shiftDisconnectedUnits(Unit.wrap(units), Value.wrap(0), Time.wrap(time));
        }
        return true;
    }

    function _shiftDisconnectedUnits(Unit shiftUnits, Value claimedAmount, Time t) internal {
        PDPoolIndex memory pdPoolIndex = poolIndexDataToPDPoolIndex(_index);
        PDPoolMember memory disconnectedMembers = _memberDataToPDPoolMember(_disconnectedMembers);
        PDPoolMemberMU memory mu = PDPoolMemberMU(pdPoolIndex, disconnectedMembers);
        mu = mu.settle(t);
        mu.m.owned_units = mu.m.owned_units + shiftUnits;
        // offset the claimed amount from the settled value if any
        // TODO Should probably not expose the private _settled_value field.
        //      Alternatively could be a independent field, while the implementer can optimize
        //      it away by merging their storage using monoidal laws again.
        mu.m._settled_value = mu.m._settled_value - claimedAmount;
        _disconnectedMembers = _pdPoolMemberToMemberData(mu.m, 0);
    }

    modifier onlyGDA() {
        if (msg.sender != address(GDA)) revert SUPERFLUID_POOL_NOT_GDA();
        _;
    }
}
