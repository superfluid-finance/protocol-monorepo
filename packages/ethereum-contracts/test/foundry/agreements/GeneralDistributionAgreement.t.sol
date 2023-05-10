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

    constructor() FoundrySuperfluidTester(6) {}

    SuperfluidPool public pool;
    uint256 public liquidationPeriod;

    function setUp() public override {
        super.setUp();
        vm.prank(alice);
        pool = SuperfluidPool(address(sf.gda.createPool(alice, superToken)));
        (liquidationPeriod,) = sf.governance.getPPPConfig(sf.host, superToken);
    }

    // GDA Setters/Getters Tests

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

    // GDA Integration Tests

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

    function testDistributeFlowWithNoConnections() public {
        vm.startPrank(alice);
        _helperDistributeFlow(superToken, alice, pool, 100);
        vm.stopPrank();
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

    function testRevertPoolOperatorConnectMember(address notOperator, address member, bool doConnect, uint32 time) public {
        vm.assume(notOperator != address(sf.gda));
        vm.startPrank(notOperator);
        vm.expectRevert(ISuperfluidPool.SUPERFLUID_POOL_NOT_GDA.selector);
        pool.operatorConnectMember(member, doConnect, time);
        vm.stopPrank();
    }

    function testDistributeFlowUsesMinDeposit(uint64 distributionFlowRate, uint32 minDepositMultiplier) public {
        vm.assume(distributionFlowRate < minDepositMultiplier);
        vm.assume(distributionFlowRate > 0);
        vm.startPrank(address(sf.governance.owner()));
        
        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPoolAndAssertConnected(bob, superToken, pool);
        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        _helperDistributeFlow(superToken, alice, pool, int96(int64(distributionFlowRate)));
        vm.stopPrank();
        (, uint256 buffer, ,) = _helperRTB(alice);
        assertEq(buffer, minimumDeposit, "GDAv1.t: Min buffer should be used");
    }

    function testDistributeFlowIgnoresMinDeposit(int32 distributionFlowRate, uint32 minDepositMultiplier) public {
        vm.assume(uint32(distributionFlowRate) >= minDepositMultiplier);
        vm.assume(distributionFlowRate > 0);
        vm.startPrank(address(sf.governance.owner()));
        
        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPoolAndAssertConnected(bob, superToken, pool);
        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        _helperDistributeFlow(superToken, alice, pool, int96(distributionFlowRate));
        vm.stopPrank();
        (, uint256 buffer, ,) = _helperRTB(alice);
        assertTrue(buffer >= minimumDeposit, "GDAv1.t: Buffer should be >= minDeposit");
    }

    function testDistributeToOneConnectedMember() public {
        _helperConnectPoolAndAssertConnected(bob, superToken, pool);

        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        _helperDistributeFlow(superToken, alice, pool, 100);
        vm.stopPrank();
        vm.warp(block.timestamp + 1);

        (int256 bobRTB,,) = sf.gda.realtimeBalanceOf(superToken, bob, block.timestamp);
        assertTrue(bobRTB > 0, "bobRTB greater than 0");
    }

    struct DistributionData {
        uint256 timeDelta;
        int256[] rtbsBefore;
        int96[] perMemberFlowRate;
    }

    struct Allocation {
        address member;
        uint128 memberUnits;
    }

    function testDistributeFlowToConnectedMemberSendingToCFA() public {
        // alice creates pool in setUp()
        Allocation memory allocation = Allocation({member: bob, memberUnits: 10});
        int96 requestedDistributionFlowRate = 420693300;

        // alice grants bob 10 units
        vm.prank(alice);
        pool.updateMember(allocation.member, allocation.memberUnits);

        // alice does a flow distribution of 420693300 to the pool (bob receives it all)
        int96 actualDistributionFlowRate =
            _helperDistributeFlowAndAssert(superToken, pool, alice, requestedDistributionFlowRate);

        // bob sends a flow of 1 to carol
        _helperConnectPoolAndAssertConnected(bob, superToken, pool);
        vm.startPrank(bob);
        superToken.createFlow(alice, 420693300);
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

    function testDistributeFlowToConnectedMembers() public {
        address[10] memory ACCOUNTS = [
            0x30B125d5Fc58c1b8E3cCB2F1C71a1Cc847f024eE,
            0xc41876DAB61De145093b6aA87417326B24Ae4ECD,
            0x04A4417CBc87009Ba69a730756F4367436E41a5F,
            0x8A29B917d9f652d82D757fF7c24048AC8A1eD185,
            0x8C53b907c856ff11708a8C4cFef4F4643CAf8B8F,
            0x5fA51030cA48D55fAd71e9D22A31A86b777A0933,
            0xd66E40b0c30595bEc72153B502aC1E0c4785991B,
            0x5a6758aEf70A5Df011dB853014cAabC34c6a144d,
            0x1965678C67dA12B2B2D2517886258C59996Cd540,
            0x718FB07d52Ff331CE8004955c26dF1A1c357b4c8
        ];
        uint128 perMemberUnits = 4206933;
        int96 requestedDistributionFlowRate = 420693300;

        Allocation[10] memory allocation;
        uint128 totalUnits;
        for (uint256 i; i < ACCOUNTS.length; i++) {
            allocation[i].member = ACCOUNTS[i];
            allocation[i].memberUnits = perMemberUnits % uint128(i + 1);
            totalUnits += allocation[i].memberUnits;
        }
        _helperUpdateUnitsAndAssert(alice, allocation);

        assertEq(pool.getTotalUnits(), totalUnits, "pool total units not equal.");

        // no distribution flow rate initially
        int96 poolFlowRateBefore = pool.getConnectedFlowRate();
        assertEq(poolFlowRateBefore, 0);

        int96 actualDistributionFlowRate =
            _helperDistributeFlowAndAssert(superToken, pool, alice, requestedDistributionFlowRate);

        (int256 distributorRTBBefore, uint256 deposit,) = superToken.realtimeBalanceOf(alice, block.timestamp);

        assertEq(deposit, uint256(uint96(actualDistributionFlowRate)) * liquidationPeriod);

        _helperConnectToPoolAndAssert(allocation);

        _helperWarpTimeAndAssertBalances(allocation, distributorRTBBefore, actualDistributionFlowRate, 2);

        // update units scenario

        for (uint256 i; i < ACCOUNTS.length; i++) {
            allocation[i].member = ACCOUNTS[i];
            allocation[i].memberUnits = perMemberUnits % uint128(i + 420);
        }
        _helperUpdateUnitsAndAssert(alice, allocation);

        actualDistributionFlowRate = _helperDistributeFlowAndAssert(superToken, pool, alice, 4127346127);
        (distributorRTBBefore, deposit,) = superToken.realtimeBalanceOf(alice, block.timestamp);

        assertEq(deposit, uint256(uint96(actualDistributionFlowRate)) * liquidationPeriod);

        _helperWarpTimeAndAssertBalances(allocation, distributorRTBBefore, actualDistributionFlowRate, 0);
    }

    function testDistributeFlowToUnconnectedMember() public {
        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        pool.updateMember(carol, 1);
        vm.stopPrank();
        int96 actualDistributionFlowRate = _helperDistributeFlowAndAssert(superToken, pool, alice, 100);
        uint256 timeWarped = 2;
        vm.warp(block.timestamp + timeWarped);
        (int256 bobRTB,,) = sf.gda.realtimeBalanceOf(superToken, bob, block.timestamp);
        assertEq(
            pool.getDisconnectedFlowRate(),
            actualDistributionFlowRate,
            "GDAv1.t.sol: pendingDistributionFlowRate != actualDistributionFlowRate"
        );
        uint128 totalUnits = pool.getTotalUnits();
        (int256 bobClaimable,) = pool.getClaimableNow(bob);
        assertEq(
            bobClaimable,
            (actualDistributionFlowRate * int96(int256(timeWarped))) / uint256(totalUnits).toInt256(),
            "GDAv1.t.sol: bobClaimable != (actualDistributionFlowRate * timeWarped) / totalUnits"
        );
        assertEq(bobRTB, 0, "GDAv1.t.sol: bobRTB != 0");
        vm.prank(bob);
        pool.claimAll();

        (bobRTB,,) = sf.gda.realtimeBalanceOf(superToken, bob, block.timestamp);
        assertEq(bobRTB, bobClaimable, "bobRTB != bobClaimable");
    }

    function testAliceBreakage() external {
        vm.prank(alice);
        pool.updateMember(alice, 1);
        vm.warp(block.timestamp + 5);
        _helperDistributeFlowAndAssert(superToken, pool, alice, 1);
        vm.warp(block.timestamp + 5);
        vm.prank(alice);
        pool.updateMember(alice, 0);

        int256 balancesSum;
        int96 flowRatesSum;
        {
            (int256 own, int256 fromPools, int256 buffer) =
                sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
            int96 poolDisconnectedRate = pool.getDisconnectedFlowRate();
            (,, int96 poolAdjustmentRate) = sf.gda.getPoolAdjustmentFlowInfo(pool);
            int96 poolAdjustmentRateDirect = sf.gda.getPoolAdjustmentFlowRate(address(superToken), address(pool));
            assertEq(poolAdjustmentRate, poolAdjustmentRateDirect, "GDAv1.t: pool adjustment rate !=");
            int96 poolNetFlowRate = sf.gda.getNetFlowRate(superToken, address(pool));
            balancesSum = balancesSum + own + fromPools + buffer;
            flowRatesSum = flowRatesSum + poolNetFlowRate;
        }
        emit log_named_int("balancesSum", balancesSum);
        emit log_named_int("flowRatesSum", flowRatesSum);

        {
            (int256 own, int256 fromPools, int256 buffer) =
                sf.gda.realtimeBalanceVectorAt(superToken, alice, block.timestamp);
            int96 flowRate = sf.gda.getNetFlowRate(superToken, alice);
            balancesSum = balancesSum + own + fromPools + buffer;
            flowRatesSum = flowRatesSum + flowRate;
        }
        emit log_named_int("balancesSum", balancesSum);
        emit log_named_int("flowRatesSum", flowRatesSum);

        assertEq(flowRatesSum, 0, "GDAv1.t: flowRatesSum != 0");
    }

    function _helperUpdateUnitsAndAssert(address poolAdmin, Allocation[10] memory allocation) internal {
        vm.startPrank(poolAdmin);
        // allocate units to members
        for (uint256 i; i < allocation.length; i++) {
            pool.updateMember(allocation[i].member, allocation[i].memberUnits);

            // assert per member units were correctly set
            assertEq(pool.getUnits(allocation[i].member), allocation[i].memberUnits, "units incorrectly set");
        }
        vm.stopPrank();
    }

    function _helperConnectToPoolAndAssert(Allocation[10] memory allocation) internal {
        for (uint256 i; i < allocation.length; i++) {
            // assert that no members connected
            bool isConnectedBefore = sf.gda.isMemberConnected(superToken, address(pool), allocation[i].member);
            assertFalse(isConnectedBefore);

            // connect if unconnected
            if (!isConnectedBefore) {
                // all members connect to the pool
                _helperConnectPoolAndAssertConnected(allocation[i].member, superToken, pool);
            }
        }
    }
    function _helperDistributeFlowAndAssert(
        ISuperfluidToken desiredToken,
        SuperfluidPool to,
        address from,
        int96 requestedDistributionFlowRate
    ) internal returns (int96 actualDistributionFlowRate) {
        vm.startPrank(from);

        // actualDistributionFlowRate provided given number of units and requestedFlowRate
        actualDistributionFlowRate =
            sf.gda.getFlowDistributionActualFlowRate(desiredToken, from, to, requestedDistributionFlowRate);

        // distribute flow of requestedDistributionFlowRate to 10 members with perMemberUnits units each
        _helperDistributeFlow(desiredToken, from, to, requestedDistributionFlowRate);

        int96 poolFlowRateAfter = to.getConnectedFlowRate();
        // assert flow rate after is the actualDistributionFlowRate based on helper function
        assertEq(poolFlowRateAfter, actualDistributionFlowRate, "pool flow rate after distribution !=");
        assertTrue(actualDistributionFlowRate >= 0, "actualDistributionFlowRate < 0");

        vm.stopPrank();
    }


    function _helperWarpTimeAndAssertBalances(
        Allocation[10] memory allocation,
        int256 distributorRTBBefore,
        int96 actualDistributionFlowRate,
        uint256 timeDelta
    ) public {
        DistributionData memory dd;
        dd.timeDelta = timeDelta;
        dd.rtbsBefore = new int256[](10);
        dd.perMemberFlowRate = new int96[](10);

        for (uint256 i; i < allocation.length; i++) {
            // get the rtb for all members before
            (int256 rtb,,) = superToken.realtimeBalanceOf(allocation[i].member, block.timestamp);
            dd.rtbsBefore[i] = rtb;

            // get the per member flow rate based on their units and the actualDistributionFlowRate for the pool
            int96 memberFlowRate = pool.getMemberFlowRate(allocation[i].member);
            dd.perMemberFlowRate[i] = memberFlowRate;
        }
        // memberFlowRate is correct

        // move the time forwards by timeDelta
        vm.warp(block.timestamp + dd.timeDelta);

        for (uint256 i; i < allocation.length; i++) {
            // get rtb after timeDelta
            (int256 rtbAfter,,) = superToken.realtimeBalanceOf(allocation[i].member, block.timestamp);

            // assert that the rtb after is equal the to rtb before + the amount flowed
            assertEq(
                rtbAfter,
                dd.rtbsBefore[i] + int256(dd.timeDelta) * int256(dd.perMemberFlowRate[i]),
                "RTB of member != rtbBefore + amount flowed"
            );
        }

        // get rtb of the distributor after time delta
        (int256 distributorRTBAfter,,) = superToken.realtimeBalanceOf(alice, block.timestamp);
        // assert that the rtb before less the amount flowed is equal to the rtb after time delta
        assertEq(
            distributorRTBBefore - actualDistributionFlowRate * int96(int256(dd.timeDelta)),
            distributorRTBAfter,
            "distributor RTB after time warp !="
        );
    }

    function _helperUpdateMemberUnitsAndAssertUnits(ISuperfluidPool _pool, address caller, address member, uint128 newUnits) internal {
        vm.startPrank(caller);
        _pool.updateMember(member, newUnits);
        vm.stopPrank();

        assertEq(_pool.getUnits(member), newUnits, "GDAv1.t: Units incorrectly set");
    }

    function _helperConnectPoolAndAssertConnected(address caller_, ISuperfluidToken superToken_, ISuperfluidPool pool_) internal {
        vm.startPrank(caller_);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeWithSelector(IGeneralDistributionAgreementV1.connectPool.selector, pool_, ""),
            new bytes(0)
        );
        vm.stopPrank();

        assertEq(sf.gda.isMemberConnected(superToken_, address(pool_), caller_), true, "GDAv1.t: Member not connected");
    }

    function _helperDisconnectPoolAndAssertDisconnected(address caller_, ISuperfluidToken superToken_, ISuperfluidPool pool_) internal {
        vm.startPrank(caller_);
        sf.host.callAgreement(sf.gda, abi.encodeCall(sf.gda.disconnectPool, (pool_, new bytes(0))), new bytes(0));
        vm.stopPrank();

        assertEq(sf.gda.isMemberConnected(superToken_, address(pool_), caller_), false, "GDAv1.t D/C: Member not disconnected");
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

    function _helperDistributeFlow(
        ISuperfluidToken _superToken,
        address sender,
        ISuperfluidPool _pool,
        int96 requestedFlowRate
    ) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distributeFlow, (_superToken, sender, _pool, requestedFlowRate, new bytes(0))),
            new bytes(0)
        );
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
