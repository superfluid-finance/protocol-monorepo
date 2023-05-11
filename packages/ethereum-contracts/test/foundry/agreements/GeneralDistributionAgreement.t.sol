// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {SafeCast} from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import {IBeacon} from "@openzeppelin/contracts/proxy/beacon/IBeacon.sol";

import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";

import "../FoundrySuperfluidTester.sol";
import {console} from "forge-std/console.sol";
import {
    GeneralDistributionAgreementV1,
    IGeneralDistributionAgreementV1
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";
import {SuperTokenV1Library} from "../../../contracts/apps/SuperTokenV1Library.sol";
import {SuperToken} from "../../../contracts/utils/SuperTokenDeployer.sol";
import {ISuperfluidToken} from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import {ISuperfluidPool, SuperfluidPool} from "../../../contracts/superfluid/SuperfluidPool.sol";

/// @title GeneralDistributionAgreementV1 Integration Tests
/// @author Superfluid
/// @notice This is a contract that runs integrations tests for the GDAv1
/// It tests interactions between contracts and more complicated interactions
/// with a range of values when applicable and it aims to ensure that the
/// these interactions work as expected.
contract GeneralDistributionAgreementV1Test is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;
    using SafeCast for uint256;
    using SafeCast for int256;

    struct UpdateMemberData {
        address member;
        uint64 newUnits;
    }

    struct ExpectedSuperfluidPoolData {
        uint128 totalUnits;
        uint128 disconnectedUnits;
        int96 connectedFlowRate;
        int96 disconnectedFlowRate;
        int256 disconnectedBalance;
    }

    struct ExpectedMemberData {
        bool isConnected;
        uint128 ownedUnits;
        int96 flowRate;
        int96 netFlowRate;
    }

    SuperfluidPool public pool;
    uint256 public liquidationPeriod;
    mapping(address => ExpectedSuperfluidPoolData) internal _expectedPoolData;

    constructor() FoundrySuperfluidTester(6) {}

    function setUp() public override {
        super.setUp();
        vm.prank(alice);
        pool = SuperfluidPool(address(sf.gda.createPool(alice, superToken)));
        (liquidationPeriod,) = sf.governance.getPPPConfig(sf.host, superToken);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                GDA Setters/Getters Tests
    //////////////////////////////////////////////////////////////////////////*/
    // Universal Index Setters/Getters
    function testSetGetUIndex(address owner, uint32 settledAt, int96 flowRate, int256 settledValue) public {
        bytes memory eff = abi.encode(superToken);
        BasicParticle memory p = BasicParticle({
            _settled_at: Time.wrap(settledAt),
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_value: Value.wrap(settledValue)
        });
        sf.gda.setUIndex(eff, owner, p);
        BasicParticle memory setP = sf.gda.getUIndex(eff, owner);

        assertEq(Time.unwrap(p._settled_at), Time.unwrap(setP._settled_at), "settledAt not equal");
        assertEq(FlowRate.unwrap(p._flow_rate), FlowRate.unwrap(setP._flow_rate), "flowRate not equal");
        assertEq(Value.unwrap(p._settled_value), Value.unwrap(setP._settled_value), "settledValue not equal");
    }

    function testSetGetUIndexData(address owner, uint32 settledAt, int96 flowRate, int256 settledValue) public {
        vm.assume(owner != address(pool));

        bytes memory eff = abi.encode(superToken);
        BasicParticle memory p = BasicParticle({
            _settled_at: Time.wrap(settledAt),
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_value: Value.wrap(settledValue)
        });
        sf.gda.setUIndex(eff, owner, p);
        GeneralDistributionAgreementV1.UniversalIndexData memory setUIndexData = sf.gda.getUIndexData(eff, owner);

        assertEq(settledAt, setUIndexData.settledAt, "settledAt not equal");
        assertEq(flowRate, setUIndexData.flowRate, "flowRate not equal");
        assertEq(settledValue, setUIndexData.settledValue, "settledValue not equal");
        assertEq(0, setUIndexData.totalBuffer, "totalBuffer not equal");
        assertEq(false, setUIndexData.isPool, "isPool not equal");
    }

    // Flow Distribution Data Setters/Getters
    function testSetGetFlowDistributionData(address from, address to, uint32 newFlowRate, uint96 newFlowRateDelta)
        public
    {
        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 lastUpdated = block.timestamp;
        sf.gda.setFlowInfo(
            abi.encode(superToken),
            flowHash,
            from,
            to,
            FlowRate.wrap(int128(uint128(newFlowRate))),
            FlowRate.wrap(int128(uint128(newFlowRateDelta)))
        );

        vm.warp(1000);

        (bool exist, GeneralDistributionAgreementV1.FlowDistributionData memory setFlowDistributionData) =
            sf.gda.getFlowDistributionData(superToken, flowHash);

        assertEq(true, exist, "flow distribution data does not exist");

        assertEq(int96(uint96(newFlowRate)), setFlowDistributionData.flowRate, "flowRate not equal");

        assertEq(lastUpdated, setFlowDistributionData.lastUpdated, "lastUpdated not equal");

        assertEq(0, setFlowDistributionData.buffer, "buffer not equal");
        assertEq(
            int96(FlowRate.unwrap(sf.gda.getFlowRate(abi.encode(superToken), flowHash))),
            int96(uint96(newFlowRate)),
            "_getFlowRate: flow rate not equal"
        );
        assertEq(
            sf.gda.getFlowRate(superToken, from, to), int96(uint96(newFlowRate)), "getFlowRate: flow rate not equal"
        );
    }

    // Pool Member Data Setters/Getters
    function testSetGetPoolMemberData(address poolMember, ISuperfluidPool _pool, uint32 poolID) public {
        vm.assume(poolID > 0);
        vm.assume(address(_pool) != address(0));
        vm.assume(address(poolMember) != address(0));
        bytes32 poolMemberId = sf.gda.getPoolMemberId(poolMember, _pool);

        vm.startPrank(address(sf.gda));
        superToken.updateAgreementData(
            poolMemberId,
            sf.gda.encodePoolMemberData(
                GeneralDistributionAgreementV1.PoolMemberData({poolID: poolID, pool: address(_pool)})
            )
        );
        vm.stopPrank();

        (bool exist, GeneralDistributionAgreementV1.PoolMemberData memory setPoolMemberData) =
            sf.gda.getPoolMemberData(superToken, poolMember, _pool);

        assertEq(true, exist, "pool member data does not exist");
        assertEq(poolID, setPoolMemberData.poolID, "poolID not equal");
        assertEq(address(_pool), setPoolMemberData.pool, "pool not equal");
    }

    // Proportional Distribution Pool Index Setters/Getters
    function testSetGetPDPIndex(
        address owner,
        uint128 totalUnits,
        uint32 wrappedSettledAt,
        int96 wrappedFlowRate,
        int256 wrappedSettledValue
    ) public {
        vm.assume(owner != address(0));
        vm.assume(totalUnits < uint128(type(int128).max));
        bytes memory eff = abi.encode(superToken);
        PDPoolIndex memory pdpIndex = PDPoolIndex({
            total_units: Unit.wrap(int128(totalUnits)),
            _wrapped_particle: BasicParticle({
                _settled_at: Time.wrap(wrappedSettledAt),
                _flow_rate: FlowRate.wrap(wrappedFlowRate),
                _settled_value: Value.wrap(wrappedSettledValue)
            })
        });
        ISuperfluidPool anotherPool = sf.gda.createPool(owner, superToken);

        vm.startPrank(address(sf.gda));
        sf.gda.setPDPIndex(eff, address(anotherPool), pdpIndex);
        vm.stopPrank();
        PDPoolIndex memory setPdpIndex = sf.gda.getPDPIndex(eff, address(anotherPool));

        assertEq(Unit.unwrap(pdpIndex.total_units), Unit.unwrap(setPdpIndex.total_units), "total units not equal");
        assertEq(
            Time.unwrap(pdpIndex._wrapped_particle._settled_at),
            Time.unwrap(setPdpIndex._wrapped_particle._settled_at),
            "settled at not equal"
        );
        assertEq(
            FlowRate.unwrap(pdpIndex._wrapped_particle._flow_rate),
            FlowRate.unwrap(setPdpIndex._wrapped_particle._flow_rate),
            "flow rate not equal"
        );
        assertEq(
            Value.unwrap(pdpIndex._wrapped_particle._settled_value),
            Value.unwrap(setPdpIndex._wrapped_particle._settled_value),
            "settled value not equal"
        );
    }

    // Adjust Buffer => FlowDistributionData modified
    function testAdjustBufferUpdatesFlowDistributionData(address from, address to, int32 oldFlowRate, int32 newFlowRate)
        public
    {
        vm.assume(newFlowRate >= 0);

        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 expectedBuffer = uint256(int256(newFlowRate)) * liquidationPeriod;
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            from,
            flowHash,
            FlowRate.wrap(int128(oldFlowRate)),
            FlowRate.wrap(int128(newFlowRate))
        );

        (bool exist, GeneralDistributionAgreementV1.FlowDistributionData memory flowDistributionData) =
            sf.gda.getFlowDistributionData(superToken, flowHash);
        assertEq(exist, true, "flow distribution data does not exist");
        assertEq(flowDistributionData.buffer, expectedBuffer, "buffer not equal");
        assertEq(flowDistributionData.flowRate, int96(newFlowRate), "buffer not equal");
        assertEq(
            int96(FlowRate.unwrap(sf.gda.getFlowRate(abi.encode(superToken), flowHash))),
            int96(newFlowRate),
            "_getFlowRate: flow rate not equal"
        );
        assertEq(sf.gda.getFlowRate(superToken, from, to), int96(newFlowRate), "getFlowRate: flow rate not equal");
    }

    // Adjust Buffer => UniversalIndexData modified
    function testAdjustBufferUpdatesUniversalIndexData(address from, address to, int32 oldFlowRate, int32 newFlowRate)
        public
    {
        vm.assume(newFlowRate >= 0);

        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 bufferDelta = uint256(int256(newFlowRate)) * liquidationPeriod; // expected buffer == buffer delta because of fresh state
        GeneralDistributionAgreementV1.UniversalIndexData memory fromUindexDataBefore =
            sf.gda.getUIndexData(abi.encode(superToken), from);
        GeneralDistributionAgreementV1.UniversalIndexData memory gdaUindexDataBefore =
            sf.gda.getUIndexData(abi.encode(superToken), address(sf.gda));
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            from,
            flowHash,
            FlowRate.wrap(int128(oldFlowRate)),
            FlowRate.wrap(int128(newFlowRate))
        );

        GeneralDistributionAgreementV1.UniversalIndexData memory fromUindexDataAfter =
            sf.gda.getUIndexData(abi.encode(superToken), from);

        assertEq(
            fromUindexDataBefore.totalBuffer + bufferDelta,
            fromUindexDataAfter.totalBuffer,
            "from total buffer not equal"
        );
        assertEq(
            fromUindexDataBefore.settledValue - int256(bufferDelta),
            fromUindexDataAfter.settledValue,
            "from settled value not shifted to gda"
        );

        GeneralDistributionAgreementV1.UniversalIndexData memory gdaUindexDataAfter =
            sf.gda.getUIndexData(abi.encode(superToken), address(sf.gda));
        assertEq(
            gdaUindexDataBefore.settledValue + int256(bufferDelta),
            gdaUindexDataAfter.settledValue,
            "gda settled value not shifted from 'from'"
        );
    }

    /*//////////////////////////////////////////////////////////////////////////
                                GDA Integration Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testInitializeGDA(IBeacon beacon) public {
        GeneralDistributionAgreementV1 gdaV1 = new GeneralDistributionAgreementV1(sf.host);
        assertEq(address(gdaV1.superTokenPoolBeacon()), address(0), "GDAv1.t: Beacon address not address(0)");
        gdaV1.initialize(beacon);

        assertEq(address(gdaV1.superTokenPoolBeacon()), address(beacon), "GDAv1.t: Beacon address not equal");
    }

    function testRevertReinitializeGDA(IBeacon beacon) public {
        vm.expectRevert("Initializable: contract is already initialized");
        sf.gda.initialize(beacon);
    }

    function testRevertAppendIndexUpdateByPoolByNonPool(BasicParticle memory p, Time t) public {
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        sf.gda.appendIndexUpdateByPool(superToken, p, t);
    }

    function testRevertPoolSettleClaimByNonPool(address claimRecipient, int256 amount) public {
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        sf.gda.poolSettleClaim(superToken, claimRecipient, amount);
    }

    function testProxiableUUIDIsExpectedValue() public {
        assertEq(
            pool.proxiableUUID(), keccak256("org.superfluid-finance.contracts.superfluid.SuperfluidPool.implementation")
        );
    }

    function testCreatePool() public {
        vm.prank(alice);
        SuperfluidPool localPool = SuperfluidPool(address(sf.gda.createPool(alice, superToken)));
        assertTrue(sf.gda.isPool(superToken, address(localPool)), "GDAv1.t: Created pool is not pool");
    }

    function testRevertConnectPoolByNonHost(address notHost) public {
        vm.assume(notHost != address(sf.host));
        vm.startPrank(notHost);
        vm.expectRevert("unauthorized host");
        sf.gda.connectPool(pool, "0x");
        vm.stopPrank();
    }

    function testRevertNonHostDisconnectPool(address notHost) public {
        vm.assume(notHost != address(sf.host));
        vm.startPrank(notHost);
        vm.expectRevert("unauthorized host");
        sf.gda.disconnectPool(pool, "0x");
        vm.stopPrank();
    }

    function testConnectPool(address caller) public {
        _helperConnectPoolAndAssertConnected(caller, superToken, pool);
    }

    function testDisconnectPool(address caller) public {
        _helperConnectPoolAndAssertConnected(caller, superToken, pool);
        _helperDisconnectPoolAndAssertDisconnected(caller, superToken, pool);
    }

    function testRealtimeBalanceOfEmpty(address account) public {
        (int256 accountRTB,,,) = _helperRTB(account);
        assertEq(accountRTB, 0);
    }

    function testRevertDistributeFlowToNonPool(int96 requestedFlowRate) public {
        vm.assume(requestedFlowRate >= 0);
        vm.assume(requestedFlowRate < int96(type(int64).max));
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        _helperDistributeFlow(superToken, alice, alice, ISuperfluidPool(bob), requestedFlowRate);
    }

    function testRevertDistributeFlowWithNegativeFlowRate(int96 requestedFlowRate) public {
        vm.assume(requestedFlowRate < 0);

        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_NO_NEGATIVE_FLOW_RATE.selector);
        _helperDistributeFlow(superToken, alice, alice, pool, requestedFlowRate);
    }

    function testRevertDistributeToNonPool(uint256 requestedAmount) public {
        vm.assume(requestedAmount < uint256(type(uint128).max));
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        _helperDistribute(superToken, alice, alice, ISuperfluidPool(bob), requestedAmount);
    }

    function testRevertDistributeForOthers(address signer, uint256 requestedAmount) public {
        vm.assume(requestedAmount < uint256(type(uint128).max));
        vm.assume(signer != alice);

        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED.selector);
        _helperDistribute(superToken, signer, alice, pool, requestedAmount);
    }

    function testRevertDistributeFlowForOthers(address signer, int32 requestedFlowRate) public {
        vm.assume(requestedFlowRate > 0);
        vm.assume(signer != alice);

        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED.selector);
        _helperDistributeFlow(superToken, signer, alice, pool, requestedFlowRate);
    }

    function testRevertDistributeFlowInsufficientBalance() public {
        uint256 balance = superToken.balanceOf(alice);
        balance /= 4 hours;
        int96 tooBigFlowRate = int96(int256(balance)) + 1;

        _helperConnectPoolAndAssertConnected(bob, superToken, pool);

        _helperUpdateMemberUnitsAndAssertUnits(pool, alice, bob, 1);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_INSUFFICIENT_BALANCE.selector);
        _helperDistributeFlow(superToken, alice, alice, pool, tooBigFlowRate);
    }

    function testRevertLiquidateNonCriticalDistributor(int32 flowRate, int96 units) public {
        vm.assume(flowRate > 0);
        _helperConnectPoolAndAssertConnected(bob, superToken, pool);

        _helperUpdateMemberUnitsAndAssertUnits(pool, alice, bob, uint96(units));

        _helperDistributeFlow(superToken, alice, alice, pool, flowRate);

        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_NON_CRITICAL_SENDER.selector);
        _helperDistributeFlow(superToken, bob, alice, pool, 0);
    }

    function testRevertDistributeInsufficientBalance() public {
        uint256 balance = superToken.balanceOf(alice);

        _helperConnectPoolAndAssertConnected(bob, superToken, pool);

        _helperUpdateMemberUnitsAndAssertUnits(pool, alice, bob, 1);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_INSUFFICIENT_BALANCE.selector);
        _helperDistribute(superToken, alice, alice, pool, balance + 1);
    }

    function testRevertPoolOperatorConnectMember(address notOperator, address member, bool doConnect, uint32 time)
        public
    {
        vm.assume(notOperator != address(sf.gda));
        vm.startPrank(notOperator);
        vm.expectRevert(ISuperfluidPool.SUPERFLUID_POOL_NOT_GDA.selector);
        pool.operatorConnectMember(member, doConnect, time);
        vm.stopPrank();
    }

    function testRevertPoolUpdateMemberThatIsPool(uint128 units) public {
        vm.assume(units < uint128(type(int128).max));

        vm.expectRevert(ISuperfluidPool.SUPERFLUID_POOL_NO_POOL_MEMBERS.selector);
        _helperUpdateMemberUnits(pool, alice, address(pool), units);
    }

    function testDistributeFlowUsesMinDeposit(uint64 distributionFlowRate, uint32 minDepositMultiplier, address member)
        public
    {
        vm.assume(distributionFlowRate < minDepositMultiplier);
        vm.assume(distributionFlowRate > 0);
        vm.assume(member != address(pool));

        vm.startPrank(address(sf.governance.owner()));
        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPoolAndAssertConnected(member, superToken, pool);
        _helperUpdateMemberUnitsAndAssertUnits(pool, alice, member, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, int96(int64(distributionFlowRate)));
        (, uint256 buffer,,) = _helperRTB(alice);
        assertEq(buffer, minimumDeposit, "GDAv1.t: Min buffer should be used");
    }

    function testDistributeFlowIgnoresMinDeposit(
        int32 distributionFlowRate,
        uint32 minDepositMultiplier,
        address member
    ) public {
        vm.assume(uint32(distributionFlowRate) >= minDepositMultiplier);
        vm.assume(distributionFlowRate > 0);
        vm.assume(member != address(pool));
        vm.startPrank(address(sf.governance.owner()));

        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPoolAndAssertConnected(member, superToken, pool);
        _helperUpdateMemberUnitsAndAssertUnits(pool, alice, member, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, int96(distributionFlowRate));
        (, uint256 buffer,,) = _helperRTB(alice);
        assertTrue(buffer >= minimumDeposit, "GDAv1.t: Buffer should be >= minDeposit");
    }

    function testDistributeFlowToConnectedMemberSendingToCFA(int32 flowRate, uint64 units) public {
        vm.assume(flowRate > 0);
        // alice creates pool in setUp()
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        vm.startPrank(alice);
        pool.updateMember(bob, memberUnits);
        vm.stopPrank();

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);

        // bob sends a flow of 1 to carol
        _helperConnectPoolAndAssertConnected(bob, superToken, pool);
        vm.startPrank(bob);
        superToken.createFlow(alice, requestedDistributionFlowRate * 10);
        vm.stopPrank();

        int96 aliceGDANetFlowRate = sf.gda.getNetFlowRate(superToken, alice);
        int96 bobGDANetFlowRate = sf.gda.getNetFlowRate(superToken, bob);
        int96 aliceCFANetFlowRate = sf.cfa.getNetFlow(superToken, alice);
        int96 bobCFANetFlowRate = sf.cfa.getNetFlow(superToken, bob);
        assertEq(
            aliceGDANetFlowRate + bobGDANetFlowRate + aliceCFANetFlowRate + bobCFANetFlowRate,
            0,
            "alice and bob GDA net flow rates !="
        );
    }

    function testDistributeToEmptyPool(int32 flowRate) public {
        vm.assume(flowRate >= 0);
        _helperDistributeFlow(superToken, alice, alice, pool, flowRate);
        int96 distributionFlowRate = sf.gda.getFlowRate(superToken, alice, address(pool));
        assertEq(distributionFlowRate, 0, "GDAv1.t: distributionFlowRate should be 0");
    }

    function testDistributeFlowToConnectedMembers(UpdateMemberData[5] memory members, int32 flowRate, uint16 warpTime)
        public
    {
        vm.assume(flowRate > 0);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperConnectPoolAndAssertConnected(members[i].member, superToken, pool);
            _helperUpdateMemberUnitsAndAssertUnits(pool, alice, members[i].member, members[i].newUnits);
        }

        _helperDistributeFlow(superToken, alice, alice, pool, 100);

        vm.warp(block.timestamp + warpTime);
    }

    function testDistributeFlowToUnconnectedMembers(UpdateMemberData[5] memory members, int32 flowRate, uint16 warpTime)
        public
    {
        vm.assume(flowRate > 0);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperUpdateMemberUnitsAndAssertUnits(pool, alice, members[i].member, members[i].newUnits);
        }

        int96 requestedFlowRate = flowRate;
        _helperDistributeFlow(superToken, alice, alice, pool, requestedFlowRate);
        int96 actualDistributionFlowRate =
            sf.gda.getFlowDistributionActualFlowRate(superToken, alice, pool, requestedFlowRate);

        vm.warp(block.timestamp + warpTime);

        uint128 totalUnits = pool.getTotalUnits();

        for (uint256 i; i < members.length; ++i) {
            address member = members[i].member;
            (int256 memberRTB,,) = sf.gda.realtimeBalanceOf(superToken, member, block.timestamp);
            assertEq(
                pool.getDisconnectedFlowRate(),
                actualDistributionFlowRate,
                "GDAv1.t.sol: pendingDistributionFlowRate != actualDistributionFlowRate"
            );
            (int256 memberClaimable,) = pool.getClaimableNow(member);
            assertEq(
                memberClaimable,
                (actualDistributionFlowRate * int96(int256(uint256(warpTime)))) * int96(uint96(members[i].newUnits))
                    / uint256(totalUnits).toInt256(),
                "GDAv1.t.sol: memberClaimable != (actualDistributionFlowRate * warpTime) / totalUnits"
            );
            assertEq(memberRTB, 0, "GDAv1.t.sol: memberRTB != 0");
            vm.prank(member);
            pool.claimAll();

            (memberRTB,,) = sf.gda.realtimeBalanceOf(superToken, member, block.timestamp);
            assertEq(memberRTB, memberClaimable, "GDAv1.t.sol: memberRTB != memberClaimable");
        }
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/

    function _helperUpdateMemberUnits(ISuperfluidPool _pool, address caller, address member, uint128 newUnits)
        internal
    {
        vm.startPrank(caller);
        _pool.updateMember(member, newUnits);
        vm.stopPrank();
    }

    function _helperUpdateMemberUnitsAndAssertUnits(
        ISuperfluidPool _pool,
        address caller,
        address member,
        uint128 newUnits
    ) internal {
        _helperUpdateMemberUnits(_pool, caller, member, newUnits);
        assertEq(_pool.getUnits(member), newUnits, "GDAv1.t: Units incorrectly set");
    }

    function _helperConnectPool(address caller_, ISuperfluidToken superToken_, ISuperfluidPool pool_) internal {
        vm.startPrank(caller_);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeWithSelector(IGeneralDistributionAgreementV1.connectPool.selector, pool_, ""),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function _helperConnectPoolAndAssertConnected(address caller_, ISuperfluidToken superToken_, ISuperfluidPool pool_)
        internal
    {
        _helperConnectPool(caller_, superToken_, pool_);

        assertEq(sf.gda.isMemberConnected(superToken_, address(pool_), caller_), true, "GDAv1.t: Member not connected");
    }

    function _helperDisconnectPool(address caller_, ISuperfluidToken superToken_, ISuperfluidPool pool_) internal {
        vm.startPrank(caller_);
        sf.host.callAgreement(sf.gda, abi.encodeCall(sf.gda.disconnectPool, (pool_, new bytes(0))), new bytes(0));
        vm.stopPrank();
    }

    function _helperDisconnectPoolAndAssertDisconnected(
        address caller_,
        ISuperfluidToken superToken_,
        ISuperfluidPool pool_
    ) internal {
        _helperDisconnectPool(caller_, superToken_, pool_);

        assertEq(
            sf.gda.isMemberConnected(superToken_, address(pool_), caller_),
            false,
            "GDAv1.t D/C: Member not disconnected"
        );
    }

    function _helperDistribute(
        ISuperfluidToken _superToken,
        address caller,
        address from,
        ISuperfluidPool _pool,
        uint256 requestedAmount
    ) internal {
        vm.startPrank(caller);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distribute, (_superToken, from, _pool, requestedAmount, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function _helperDistributeFlow(
        ISuperfluidToken _superToken,
        address caller,
        address from,
        ISuperfluidPool _pool,
        int96 requestedFlowRate
    ) internal {
        vm.startPrank(caller);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distributeFlow, (_superToken, from, _pool, requestedFlowRate, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function _helperRTB(address account)
        internal
        returns (int256 availableBalance, uint256 buffer, uint256 owedBuffer, uint256 timestamp)
    {
        (int256 availableBalanceA, uint256 bufferA, uint256 owedBufferA) =
            sf.gda.realtimeBalanceOf(superToken, account, block.timestamp);
        (int256 availableBalanceB, uint256 bufferB, uint256 owedBufferB, uint256 timestampB) =
            sf.gda.realtimeBalanceOfNow(superToken, account);
        assertEq(availableBalanceA, availableBalanceB, "GDAv1.t: availableBalance !=");
        assertEq(bufferA, bufferB, "GDAv1.t: buffer funcs !=");
        assertEq(owedBufferA, owedBufferB, "GDAv1.t: owedBuffer funcs !=");
        assertEq(timestampB, block.timestamp, "GDAv1.t: timestamp !=");

        return (availableBalanceA, bufferA, owedBufferA, timestampB);
    }

    struct PoolUpdateStep {
        uint8 u; // which user
        uint8 a; // action types: 0 update units, 1 distribute flow, 2 pool connection, 3 pool claim for
        uint32 v; // action param
        uint16 dt; // time delta
    }

    // @note commented out as it slows down the tests A LOT
    // function testPoolRandomSeqs(PoolUpdateStep[20] memory steps) external {
    //     uint256 N_MEMBERS = 5;

    //     steps[0] = PoolUpdateStep(1, 0, 1, 65533);
    //     steps[1] = PoolUpdateStep(2, 1, 1, 65534);
    //     steps[2] = PoolUpdateStep(2, 1, 0, 0);

    //     for (uint256 i = 0; i < steps.length; ++i) {
    //         emit log_named_uint(">>> STEP", i);
    //         PoolUpdateStep memory s = steps[i];
    //         uint256 action = s.a % 4;
    //         uint256 u = 1 + s.u % N_MEMBERS;
    //         address user = TEST_ACCOUNTS[u];

    //         emit log_named_uint("user", u);
    //         emit log_named_uint("time delta", s.dt);
    //         emit log_named_uint("> timestamp", block.timestamp);
    //         emit log_named_address("tester", user);

    //         if (action == 0) {
    //             emit log_named_string("action", "updateMember");
    //             emit log_named_uint("units", s.v);
    //             vm.startPrank(alice);
    //             assert(pool.updateMember(user, s.v));
    //             vm.stopPrank();
    //         } else if (action == 1) {
    //             emit log_named_string("action", "distributeFlow");
    //             emit log_named_uint("flow rate", s.v);
    //             _helperDistributeFlowAndAssert(superToken, pool, user, int96(uint96(s.v)));
    //         } else if (action == 2) {
    //             address u4 = TEST_ACCOUNTS[1 + (s.v % N_MEMBERS)];
    //             emit log_named_string("action", "claimAll");
    //             emit log_named_address("claim for", u4);
    //             vm.startPrank(user);
    //             assert(pool.claimAll(u4));
    //             vm.stopPrank();
    //         } else if (action == 3) {
    //             bool doConnect = s.v % 2 == 0 ? false : true;
    //             emit log_named_string("action", "doConnectPool");
    //             emit log_named_string("doConnect", doConnect ? "true" : "false");
    //             vm.startPrank(user);
    //             doConnect ? _helperConnectPoolAndAssertConnected(pool) superToken, : _helperDisconnectPoolAndAssertDisconnected(pool);
    //             vm.stopPrank();
    //         } else {
    //             assert(false);
    //         }

    //         {
    //             (int256 own, int256 fromPools, int256 buffer) =
    //                 sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
    //             int96 connectedFlowRate = pool.getConnectedFlowRate();
    //             int96 nr = sf.gda.getNetFlowRate(superToken, address(pool));
    //             emit log_string("> pool before time warp");
    //             emit log_named_int("own", own);
    //             emit log_named_int("fromPoolsBalance", fromPools);
    //             emit log_named_int("buffer", buffer);
    //             emit log_named_int("pool net flow rate", nr);
    //         }

    //         emit log_named_uint("> dt", s.dt);
    //         vm.warp(block.timestamp + s.dt);

    //         {
    //             (int256 own, int256 fromPools, int256 buffer) =
    //                 sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
    //             int96 connectedFlowRate = pool.getConnectedFlowRate();
    //             int96 nr = sf.gda.getNetFlowRate(superToken, address(pool));
    //             emit log_string("> pool before time warp");
    //             emit log_named_int("own", own);
    //             emit log_named_int("fromPoolsBalance", fromPools);
    //             emit log_named_int("buffer", buffer);
    //             emit log_named_int("pool net flow rate", nr);
    //         }
    //     }

    //     int96 flowRatesSum;
    //     {
    //         (int256 own, int256 fromPools, int256 buffer) =
    //             sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
    //         int96 poolDisconnectedRate = pool.getDisconnectedFlowRate();
    //         (,, int96 poolAdjustmentRate) = sf.gda.getPoolAdjustmentFlowInfo(pool);
    //         int96 poolNetFlowRate = sf.gda.getNetFlowRate(superToken, address(pool));
    //         flowRatesSum = flowRatesSum + poolNetFlowRate;
    //     }

    //     for (uint256 i = 1; i <= N_MEMBERS; ++i) {
    //         (int256 own, int256 fromPools, int256 buffer) =
    //             sf.gda.realtimeBalanceVectorAt(superToken, TEST_ACCOUNTS[i], block.timestamp);
    //         int96 flowRate = sf.gda.getNetFlowRate(superToken, TEST_ACCOUNTS[i]);
    //         flowRatesSum = flowRatesSum + flowRate;
    //     }

    //     assertEq(flowRatesSum, 0, "GDAv1.t: flowRatesSum != 0");
    // }
}
