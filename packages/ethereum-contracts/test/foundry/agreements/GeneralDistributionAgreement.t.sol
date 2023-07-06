// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { IBeacon } from "@openzeppelin/contracts/proxy/beacon/IBeacon.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import "../FoundrySuperfluidTester.sol";
import { console } from "forge-std/console.sol";
import {
    GeneralDistributionAgreementV1,
    IGeneralDistributionAgreementV1
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { ISuperToken, SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { ISuperfluidToken } from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperfluidPool, SuperfluidPool } from "../../../contracts/superfluid/SuperfluidPool.sol";
import { SuperfluidPoolStorageLayoutMock } from "../../../contracts/mocks/SuperfluidPoolUpgradabilityMock.sol";
import { IPoolNFTBase } from "../../../contracts/interfaces/superfluid/IPoolNFTBase.sol";
import { IPoolAdminNFT } from "../../../contracts/interfaces/superfluid/IPoolAdminNFT.sol";
import { IPoolMemberNFT } from "../../../contracts/interfaces/superfluid/IPoolMemberNFT.sol";
import { IFlowNFTBase } from "../../../contracts/interfaces/superfluid/IFlowNFTBase.sol";
import { IConstantOutflowNFT } from "../../../contracts/interfaces/superfluid/IConstantOutflowNFT.sol";
import { IConstantInflowNFT } from "../../../contracts/interfaces/superfluid/IConstantInflowNFT.sol";

/// @title GeneralDistributionAgreementV1 Integration Tests
/// @author Superfluid
/// @notice This is a contract that runs integrations tests for the GDAv1
/// It tests interactions between contracts and more complicated interactions
/// with a range of values when applicable and it aims to ensure that the
/// these interactions work as expected.
contract GeneralDistributionAgreementV1Test is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;
    using EnumerableSet for EnumerableSet.AddressSet;
    using SafeCast for uint256;
    using SafeCast for int256;

    struct UpdateMemberData {
        address member;
        uint64 newUnits;
    }

    struct ExpectedSuperfluidPoolData {
        int128 totalUnits;
        int128 connectedUnits;
        int128 disconnectedUnits;
        int96 connectedFlowRate;
        int96 disconnectedFlowRate;
        int256 disconnectedBalance;
    }

    struct ExpectedPoolMemberData {
        bool isConnected;
        uint128 ownedUnits;
        int96 flowRate;
        int96 netFlowRate;
    }

    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);

    SuperfluidPool public pool;
    uint256 public liquidationPeriod;
    mapping(address pool => ExpectedSuperfluidPoolData expectedData) internal _expectedPoolData;
    mapping(address pool => EnumerableSet.AddressSet members) internal _poolMembers;
    mapping(address pool => mapping(address member => ExpectedPoolMemberData expectedData)) internal
        _poolToExpectedMemberData;

    constructor() FoundrySuperfluidTester(6) { }

    function setUp() public override {
        super.setUp();
        vm.startPrank(alice);
        pool = SuperfluidPool(address(superToken.createPool(alice)));
        vm.stopPrank();
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
        (BasicParticle memory setP,) = sf.gda.getUIndexAndUindexData(eff, owner);

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
        (, GeneralDistributionAgreementV1.UniversalIndexData memory setUIndexData) =
            sf.gda.getUIndexAndUindexData(eff, owner);

        assertEq(settledAt, setUIndexData.settledAt, "settledAt not equal");
        assertEq(flowRate, setUIndexData.flowRate, "flowRate not equal");
        assertEq(settledValue, setUIndexData.settledValue, "settledValue not equal");
        assertEq(0, setUIndexData.totalBuffer, "totalBuffer not equal");
        assertEq(false, setUIndexData.isPool, "isPool not equal");
    }

    // Flow Distribution Data Setters/Getters
    function testSetGetFlowDistributionData(
        address from,
        ISuperfluidPool to,
        uint32 newFlowRate,
        uint96 newFlowRateDelta
    ) public {
        bytes32 flowHash = sf.gda.getFlowDistributionId(from, address(to));
        uint256 lastUpdated = block.timestamp;
        sf.gda.setFlowInfo(
            abi.encode(superToken),
            flowHash,
            from,
            address(to),
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
                GeneralDistributionAgreementV1.PoolMemberData({ poolID: poolID, pool: address(_pool) })
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
        ISuperfluidPool anotherPool = sf.gda.createPool(superToken, owner);

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
            address(pool),
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
        assertEq(
            sf.gda.getFlowRate(superToken, from, ISuperfluidPool(to)),
            int96(newFlowRate),
            "getFlowRate: flow rate not equal"
        );
    }

    // Adjust Buffer => UniversalIndexData modified
    function testAdjustBufferUpdatesUniversalIndexData(address from, address to, int32 oldFlowRate, int32 newFlowRate)
        public
    {
        vm.assume(newFlowRate >= 0);

        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 bufferDelta = uint256(int256(newFlowRate)) * liquidationPeriod; // expected buffer == buffer delta
            // because of fresh state
        (, GeneralDistributionAgreementV1.UniversalIndexData memory fromUindexDataBefore) =
            sf.gda.getUIndexAndUindexData(abi.encode(superToken), from);
        (, GeneralDistributionAgreementV1.UniversalIndexData memory gdaUindexDataBefore) =
            sf.gda.getUIndexAndUindexData(abi.encode(superToken), address(sf.gda));
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            address(pool),
            from,
            flowHash,
            FlowRate.wrap(int128(oldFlowRate)),
            FlowRate.wrap(int128(newFlowRate))
        );

        (, GeneralDistributionAgreementV1.UniversalIndexData memory fromUindexDataAfter) =
            sf.gda.getUIndexAndUindexData(abi.encode(superToken), from);

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

        (, GeneralDistributionAgreementV1.UniversalIndexData memory gdaUindexDataAfter) =
            sf.gda.getUIndexAndUindexData(abi.encode(superToken), address(sf.gda));
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
        assertEq(address(gdaV1.superfluidPoolBeacon()), address(0), "GDAv1.t: Beacon address not address(0)");
        gdaV1.initialize(beacon);

        assertEq(address(gdaV1.superfluidPoolBeacon()), address(beacon), "GDAv1.t: Beacon address not equal");
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
        assertEq(pool.proxiableUUID(), keccak256("org.superfluid-finance.contracts.SuperfluidPool.implementation"));
    }

    function testPositiveBalanceIsPatricianPeriodNow(address account) public {
        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, account);
        assertEq(isPatricianPeriod, true);
    }

    function testNegativeBalanceIsPatricianPeriodNowIsTrue() public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        _helperConnectPool(bob, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, bob, 1);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, pool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);
        int96 fr = sf.gda.getFlowRate(superToken, alice, pool);

        uint256 aliceBalance = superToken.balanceOf(alice);

        _helperWarpToCritical(alice, actualDistributionFlowRate, 1);

        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, alice);
        assertEq(isPatricianPeriod, true);
    }

    function testNegativeBalanceIsPatricianPeriodNowIsFalse() public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        _helperConnectPool(bob, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, bob, 1);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, pool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);

        if (actualDistributionFlowRate > 0) {
            _helperWarpToInsolvency(alice, actualDistributionFlowRate, liquidationPeriod, 1);
        }

        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, alice);
        assertEq(isPatricianPeriod, false);
    }

    function testNegativeBalanceIsPatricianPeriodNowIsFalseWithZeroDeposit() public {
        uint256 aliceBalance = superToken.balanceOf(alice);
        uint256 bobBalance = superToken.balanceOf(bob);
        int96 flowRate = aliceBalance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        vm.startPrank(sf.governance.owner());
        sf.governance.setRewardAddress(sf.host, ISuperfluidToken(address(0)), alice);
        vm.stopPrank();

        _helperConnectPool(bob, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, bob, 1);

        (int256 aliceRTB, uint256 deposit,,) = superToken.realtimeBalanceOfNow(alice);

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);
        int96 fr = sf.gda.getFlowRate(superToken, alice, pool);

        vm.warp(block.timestamp + (INIT_SUPER_TOKEN_BALANCE / uint256(uint96(fr))));

        (aliceRTB, deposit,,) = superToken.realtimeBalanceOfNow(alice);

        _helperDistributeFlow(superToken, bob, alice, pool, 0);

        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, alice);
        // TODO
        assertEq(isPatricianPeriod, false, "false patrician period");
    }

    function testCreatePool() public {
        _helperCreatePool(superToken, alice, alice);
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
        _helperConnectPool(caller, superToken, pool);
    }

    function testDisconnectPool(address caller) public {
        _helperConnectPool(caller, superToken, pool);
        _helperDisconnectPool(caller, superToken, pool);
    }

    function testRevertDistributeFlowToNonPool(int96 requestedFlowRate) public {
        vm.assume(requestedFlowRate >= 0);
        vm.assume(requestedFlowRate < int96(type(int64).max));
        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        superToken.distributeFlow(alice, ISuperfluidPool(bob), requestedFlowRate);
        vm.stopPrank();
    }

    function testRevertDistributeFlowWithNegativeFlowRate(int96 requestedFlowRate) public {
        vm.assume(requestedFlowRate < 0);

        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_NO_NEGATIVE_FLOW_RATE.selector);
        _helperDistributeFlow(superToken, alice, alice, pool, requestedFlowRate);
    }

    function testRevertDistributeToNonPool(uint256 requestedAmount) public {
        vm.assume(requestedAmount < uint256(type(uint128).max));

        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        superToken.distributeToPool(alice, ISuperfluidPool(bob), requestedAmount);
        vm.stopPrank();
    }

    function testRevertDistributeForOthers(address signer, uint256 requestedAmount) public {
        vm.assume(requestedAmount < uint256(type(uint128).max));
        vm.assume(signer != alice);

        vm.startPrank(signer);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED.selector);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distribute, (superToken, alice, pool, requestedAmount, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function testRevertDistributeFlowForOthers(address signer, int32 requestedFlowRate) public {
        vm.assume(requestedFlowRate > 0);
        vm.assume(signer != alice);

        vm.startPrank(signer);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED.selector);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distributeFlow, (superToken, alice, pool, requestedFlowRate, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function testRevertDistributeFlowInsufficientBalance() public {
        uint256 balance = superToken.balanceOf(alice);
        balance /= 4 hours;
        int96 tooBigFlowRate = int96(int256(balance)) + 1;

        _helperConnectPool(bob, superToken, pool);

        _helperUpdateMemberUnits(pool, alice, bob, 1);
        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_INSUFFICIENT_BALANCE.selector);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distributeFlow, (superToken, alice, pool, tooBigFlowRate, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function testRevertLiquidateNonCriticalDistributor(int32 flowRate, int96 units) public {
        vm.assume(flowRate > 0);
        _helperConnectPool(bob, superToken, pool);

        _helperUpdateMemberUnits(pool, alice, bob, uint96(units));

        _helperDistributeFlow(superToken, alice, alice, pool, flowRate);

        vm.startPrank(bob);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_NON_CRITICAL_SENDER.selector);
        superToken.distributeFlow(alice, pool, 0);
        vm.stopPrank();
    }

    function testRevertDistributeInsufficientBalance() public {
        uint256 balance = superToken.balanceOf(alice);

        _helperConnectPool(bob, superToken, pool);

        _helperUpdateMemberUnits(pool, alice, bob, 1);

        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_INSUFFICIENT_BALANCE.selector);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distribute, (superToken, alice, pool, balance + 1, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
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
        vm.startPrank(alice);
        pool.updateMember(address(pool), units);
        vm.stopPrank();
    }

    function testSuperfluidPoolStorageLayout() public {
        SuperfluidPoolStorageLayoutMock mock = new SuperfluidPoolStorageLayoutMock(sf.gda);
        mock.validateStorageLayout();
    }

    function testDistributeFlowUsesMinDeposit(uint64 distributionFlowRate, uint32 minDepositMultiplier, address member)
        public
    {
        vm.assume(distributionFlowRate < minDepositMultiplier);
        vm.assume(distributionFlowRate > 0);
        vm.assume(member != address(pool));
        vm.assume(member != address(0));

        vm.startPrank(address(sf.governance.owner()));
        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPool(member, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, member, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, int96(int64(distributionFlowRate)));
        (, uint256 buffer,,) = superToken.realtimeBalanceOfNow(alice);
        assertEq(buffer, minimumDeposit, "GDAv1.t: Min buffer should be used");
    }

    function testDistributeFlowIgnoresMinDeposit(
        int32 distributionFlowRate,
        uint32 minDepositMultiplier,
        address member
    ) public {
        vm.assume(uint32(distributionFlowRate) >= minDepositMultiplier);
        vm.assume(distributionFlowRate > 0);
        vm.assume(member != address(0));
        vm.assume(member != address(pool));
        vm.startPrank(address(sf.governance.owner()));

        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPool(member, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, member, 1);
        _helperDistributeFlow(superToken, alice, alice, pool, int96(distributionFlowRate));
        (, uint256 buffer,,) = superToken.realtimeBalanceOfNow(alice);
        assertTrue(buffer >= minimumDeposit, "GDAv1.t: Buffer should be >= minDeposit");
    }

    function testDistributeFlowToConnectedMemberSendingToCFA(int32 flowRate, uint64 units) public {
        vm.assume(flowRate > 0);
        // alice creates pool in setUp()
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        _helperUpdateMemberUnits(pool, alice, bob, memberUnits);

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);

        // bob sends a flow of 1 to carol
        _helperConnectPool(bob, superToken, pool);
        vm.startPrank(bob);
        superToken.createFlow(alice, requestedDistributionFlowRate * 10);
        vm.stopPrank();

        int96 aliceGDANetFlowRate = sf.gda.getNetFlow(superToken, alice);
        int96 bobGDANetFlowRate = sf.gda.getNetFlow(superToken, bob);
        int96 aliceCFANetFlowRate = sf.cfa.getNetFlow(superToken, alice);
        int96 bobCFANetFlowRate = sf.cfa.getNetFlow(superToken, bob);
        assertEq(
            aliceGDANetFlowRate + bobGDANetFlowRate + aliceCFANetFlowRate + bobCFANetFlowRate,
            0,
            "alice and bob GDA net flow rates !="
        );
    }

    function testDistributeToEmptyPool(uint64 distributionAmount) public {
        _helperDistribute(superToken, alice, alice, pool, distributionAmount);
    }

    function testDistributeFlowToEmptyPool(int32 flowRate) public {
        vm.assume(flowRate >= 0);
        _helperDistributeFlow(superToken, alice, alice, pool, flowRate);
        int96 distributionFlowRate = sf.gda.getFlowRate(superToken, alice, pool);
        assertEq(distributionFlowRate, 0, "GDAv1.t: distributionFlowRate should be 0");
    }

    function testDistributeFlowCriticalLiquidation(uint64 units) public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        _helperConnectPool(bob, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, bob, memberUnits);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, pool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);
        int96 fr = sf.gda.getFlowRate(superToken, alice, pool);

        uint256 aliceBalance = superToken.balanceOf(alice);

        if (actualDistributionFlowRate > 0) {
            _helperWarpToCritical(alice, actualDistributionFlowRate, 1);
            uint256 timeToCritical = aliceBalance / int256(actualDistributionFlowRate).toUint256();
            _helperDistributeFlow(superToken, bob, alice, pool, 0);
        }
    }

    function testDistributeFlowInsolventLiquidation(uint64 units) public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        _helperConnectPool(bob, superToken, pool);
        _helperUpdateMemberUnits(pool, alice, bob, memberUnits);
        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, pool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, pool, requestedDistributionFlowRate);
        int96 fr = sf.gda.getFlowRate(superToken, alice, pool);

        uint256 aliceBalance = superToken.balanceOf(alice);

        if (actualDistributionFlowRate > 0) {
            _helperWarpToInsolvency(alice, actualDistributionFlowRate, liquidationPeriod, 1);
            uint256 timeToCritical = aliceBalance / int256(actualDistributionFlowRate).toUint256();
            _helperDistributeFlow(superToken, bob, alice, pool, 0);
        }
    }

    function testDistributeToDisconnectedMembers(
        UpdateMemberData[5] memory members,
        uint256 distributionAmount,
        uint16 warpTime
    ) public {
        address distributor = alice;
        uint256 distributorBalance = superToken.balanceOf(distributor);

        vm.assume(members.length > 0);
        vm.assume(distributionAmount < distributorBalance);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperUpdateMemberUnits(pool, alice, members[i].member, members[i].newUnits);
        }
        _helperDistribute(superToken, alice, alice, pool, distributionAmount);
    }

    function testDistributeToConnectedMembers(
        UpdateMemberData[5] memory members,
        uint256 distributionAmount,
        uint16 warpTime
    ) public {
        address distributor = alice;
        uint256 distributorBalance = superToken.balanceOf(distributor);

        vm.assume(members.length > 0);
        vm.assume(distributionAmount < distributorBalance);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperConnectPool(members[i].member, superToken, pool);
            _helperUpdateMemberUnits(pool, alice, members[i].member, members[i].newUnits);
        }
        _helperDistribute(superToken, alice, alice, pool, distributionAmount);
    }

    function testDistributeFlowToConnectedMembers(UpdateMemberData[5] memory members, int32 flowRate, uint16 warpTime)
        public
    {
        vm.assume(members.length > 0);
        vm.assume(flowRate > 0);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperConnectPool(members[i].member, superToken, pool);
            _helperUpdateMemberUnits(pool, alice, members[i].member, members[i].newUnits);
        }

        _helperDistributeFlow(superToken, alice, alice, pool, 100);
        assertEq(
            sf.gda.getPoolAdjustmentFlowRate(superToken, address(pool)), 0, "GDAv1.t: Pool adjustment rate is non-zero"
        );
    }

    function testDistributeFlowToUnconnectedMembers(UpdateMemberData[5] memory members, int32 flowRate, uint16 warpTime)
        public
    {
        vm.assume(flowRate > 0);
        vm.assume(members.length > 0);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperUpdateMemberUnits(pool, alice, members[i].member, members[i].newUnits);
        }

        int96 requestedFlowRate = flowRate;
        _helperDistributeFlow(superToken, alice, alice, pool, requestedFlowRate);
        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, pool, requestedFlowRate);

        vm.warp(block.timestamp + warpTime);

        uint128 totalUnits = pool.getTotalUnits();

        for (uint256 i; i < members.length; ++i) {
            address member = members[i].member;
            if (member != address(0)) {
                // @note we test realtimeBalanceOfNow here as well
                (int256 memberRTB,,) = sf.gda.realtimeBalanceOf(superToken, member, block.timestamp);
                (int256 rtbNow,,,) = sf.gda.realtimeBalanceOfNow(superToken, member);
                assertEq(memberRTB, rtbNow, "testDistributeFlowToUnconnectedMembers: rtb != rtbNow");

                assertEq(
                    pool.getTotalDisconnectedFlowRate(),
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
    }

    // Pool ERC20 functions

    function testApproveOnly(address owner, address spender, uint256 amount) public {
        vm.assume(owner != address(0));
        vm.assume(spender != address(0));

        _helperApprove(pool, owner, spender, amount);
    }

    function testIncreaseAllowance(address owner, address spender, uint256 addedValue) public {
        vm.assume(owner != address(0));
        vm.assume(spender != address(0));

        _helperIncreaseAllowance(pool, owner, spender, addedValue);
    }

    function testDecreaseAllowance(address owner, address spender, uint256 addedValue, uint256 subtractedValue)
        public
    {
        vm.assume(owner != address(0));
        vm.assume(spender != address(0));
        vm.assume(addedValue >= subtractedValue);

        _helperIncreaseAllowance(pool, owner, spender, addedValue);
        _helperDecreaseAllowance(pool, owner, spender, subtractedValue);
    }

    function testRevertIfUnitsTransferReceiverIsPool(address from, address to, int96 unitsAmount, int128 transferAmount)
        public
    {
        // @note we use int96 because overflow will happen otherwise
        vm.assume(unitsAmount >= 0);
        vm.assume(transferAmount > 0);
        vm.assume(from != address(0));
        vm.assume(to != address(0));
        vm.assume(from != to);
        vm.assume(transferAmount <= unitsAmount);
        _helperUpdateMemberUnits(pool, alice, from, uint128(int128(unitsAmount)));

        vm.startPrank(from);
        vm.expectRevert(ISuperfluidPool.SUPERFLUID_POOL_NO_POOL_MEMBERS.selector);
        pool.transfer(address(pool), uint256(uint128(transferAmount)));
        vm.stopPrank();
    }

    function testBasicTransfer(address from, address to, int96 unitsAmount, int128 transferAmount) public {
        // @note we use int96 because overflow will happen otherwise
        vm.assume(unitsAmount >= 0);
        vm.assume(transferAmount > 0);
        vm.assume(from != address(0));
        vm.assume(to != address(0));
        vm.assume(from != to);
        vm.assume(transferAmount <= unitsAmount);
        _helperUpdateMemberUnits(pool, alice, from, uint128(int128(unitsAmount)));

        _helperPoolUnitsTransfer(pool, from, to, uint256(uint128(transferAmount)));
    }

    function testApproveAndTransferFrom(address owner, address spender, int128 transferAmount) public {
        vm.assume(transferAmount > 0);
        vm.assume(spender != address(0));
        vm.assume(owner != address(0));
        vm.assume(spender != owner);
        _helperUpdateMemberUnits(pool, alice, owner, uint128(int128(transferAmount)));
        _helperApprove(pool, owner, spender, uint256(uint128(transferAmount)));
        _helperPoolUnitsTransferFrom(pool, spender, owner, spender, uint256(uint128(transferAmount)));
    }

    function testIncreaseAllowanceAndTransferFrom(address owner, address spender, int128 transferAmount) public {
        vm.assume(transferAmount > 0);
        vm.assume(spender != address(0));
        vm.assume(owner != address(0));
        vm.assume(spender != owner);
        _helperUpdateMemberUnits(pool, alice, owner, uint128(int128(transferAmount)));
        _helperIncreaseAllowance(pool, owner, spender, uint256(uint128(transferAmount)));
        _helperPoolUnitsTransferFrom(pool, spender, owner, spender, uint256(uint128(transferAmount)));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/

    function _helperCreatePool(ISuperToken _superToken, address _caller, address _poolAdmin) internal {
        vm.startPrank(_caller);
        SuperfluidPool localPool = SuperfluidPool(address(sf.gda.createPool(_superToken, _poolAdmin)));
        vm.stopPrank();

        assertTrue(sf.gda.isPool(_superToken, address(localPool)), "GDAv1.t: Created pool is not pool");

        IPoolAdminNFT poolAdminNft = SuperToken(address(_superToken)).POOL_ADMIN_NFT();
        uint256 tokenId = poolAdminNft.getTokenId(address(localPool), _poolAdmin);
        assertEq(
            SuperToken(address(_superToken)).POOL_ADMIN_NFT().ownerOf(tokenId),
            _poolAdmin,
            "GDAv1.t: Pool Admin NFT is not owned by pool admin"
        );
    }

    function _helperGetValidDrainFlowRate(int256 balance) internal pure returns (int96) {
        return (balance / type(int32).max).toInt96();
    }

    function _helperWarpToCritical(address account_, int96 netFlowRate_, uint256 secondsCritical_) internal {
        assertTrue(secondsCritical_ > 0, "_helperWarpToCritical: secondsCritical_ must be > 0 to reach critical");
        (int256 ab,,) = superToken.realtimeBalanceOf(account_, block.timestamp);
        int256 timeToZero = ab / netFlowRate_;
        uint256 amountToWarp = timeToZero.toUint256() + secondsCritical_;
        vm.warp(block.timestamp + amountToWarp);
        assertTrue(superToken.isAccountCriticalNow(account_), "_helperWarpToCritical: account is not critical");
    }

    function _helperWarpToInsolvency(
        address account_,
        int96 netFlowRate_,
        uint256 liquidationPeriod_,
        uint256 secondsInsolvent_
    ) internal {
        assertTrue(secondsInsolvent_ > 0, "_helperWarpToInsolvency: secondsInsolvent_ must be > 0 to reach insolvency");
        (int256 ab,,) = superToken.realtimeBalanceOf(account_, block.timestamp);
        int256 timeToZero = ab / netFlowRate_;
        uint256 amountToWarp = timeToZero.toUint256() + liquidationPeriod_ + secondsInsolvent_;
        vm.warp(block.timestamp + amountToWarp);
        assertFalse(superToken.isAccountSolventNow(account_), "_helperWarpToInsolvency: account is still solvent");
    }

    function _helperGetMemberInitialState(ISuperfluidPool pool_, address member_)
        internal
        returns (bool isConnected, int256 oldUnits, int96 oldFlowRate)
    {
        oldUnits = uint256(pool_.getUnits(member_)).toInt256();
        assertEq(
            oldUnits,
            pool_.balanceOf(member_).toInt256(),
            "_helperGetMemberInitialState: member units != member balanceOf"
        );
        isConnected = sf.gda.isMemberConnected(pool_, member_);
        oldFlowRate = pool_.getMemberFlowRate(member_);
    }

    function _helperUpdateMemberUnits(ISuperfluidPool pool_, address caller_, address member_, uint128 newUnits_)
        internal
    {
        ISuperfluidToken poolSuperToken = pool_.superToken();
        if (caller_ == address(0) || member_ == address(0) || sf.gda.isPool(poolSuperToken, member_)) return;

        (bool isConnected, int256 oldUnits, int96 oldFlowRate) = _helperGetMemberInitialState(pool_, member_);

        vm.startPrank(caller_);
        pool_.updateMember(member_, newUnits_);
        vm.stopPrank();
        assertEq(pool_.getUnits(member_), newUnits_, "GDAv1.t: Units incorrectly set");

        int256 unitsDelta = uint256(newUnits_).toInt256() - oldUnits;

        // Update Expected Pool Data
        _expectedPoolData[address(pool_)].totalUnits += unitsDelta.toInt128();
        _expectedPoolData[address(pool_)].connectedUnits += isConnected ? unitsDelta.toInt128() : int128(0);
        _expectedPoolData[address(pool_)].disconnectedUnits += isConnected ? int128(0) : unitsDelta.toInt128();
        // TODO: how do we get the connected/disconnected/adjustment flow rates and connected balance?
        // NOTE: actualFlowRate of all distributors should be totalDistributionFlowRate + adjustmentFlowRate
        // we should not recalculate the adjustment flow rate and other flow rates, but we should keep track
        // of the changes in flow rates and ensure that the global invariants hold for those instead of
        // duplicating the logic here and asserting that the state changes occuring in the code is the same
        // as the state changes replicated in here

        // Update Expected Member Data
        if (newUnits_ > 0) {
            // @note You are only considered a member if you are given units
            _poolMembers[address(pool_)].add(member_);
        }
        // TODO: how does flowRate/netFlowRate for a member get impacted by this?

        // Assert Pool Units are set
        _assertPoolUnits(pool_);

        // Assert Pool Member NFT is minted/burned
        _assertPoolMemberNFT(poolSuperToken, pool_, member_, newUnits_);
    }

    function _helperConnectPool(address caller_, ISuperToken superToken_, ISuperfluidPool pool_) internal {
        (bool isConnected, int256 oldUnits, int96 oldFlowRate) = _helperGetMemberInitialState(pool_, caller_);

        vm.startPrank(caller_);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeWithSelector(IGeneralDistributionAgreementV1.connectPool.selector, pool_, ""),
            new bytes(0)
        );
        vm.stopPrank();

        assertEq(sf.gda.isMemberConnected(superToken_, address(pool_), caller_), true, "GDAv1.t: Member not connected");

        // Update Expected Pool Data
        _expectedPoolData[address(pool_)].connectedUnits += isConnected ? int128(0) : oldUnits.toInt128();
        _expectedPoolData[address(pool_)].disconnectedUnits -= isConnected ? int128(0) : oldUnits.toInt128();
        _expectedPoolData[address(pool_)].connectedFlowRate += isConnected ? int96(0) : oldFlowRate;
        _expectedPoolData[address(pool_)].disconnectedFlowRate -= isConnected ? int96(0) : oldFlowRate;

        // Update Expected Member Data
        // TODO how does the flow rate change here
    }

    function _helperDisconnectPool(address caller_, ISuperToken superToken_, ISuperfluidPool pool_) internal {
        (bool isConnected, int256 oldUnits, int96 oldFlowRate) = _helperGetMemberInitialState(pool_, caller_);

        vm.startPrank(caller_);
        sf.host.callAgreement(sf.gda, abi.encodeCall(sf.gda.disconnectPool, (pool_, new bytes(0))), new bytes(0));
        vm.stopPrank();

        assertEq(
            sf.gda.isMemberConnected(superToken_, address(pool_), caller_),
            false,
            "GDAv1.t D/C: Member not disconnected"
        );

        // Update Expected Pool Data
        _expectedPoolData[address(pool_)].connectedUnits -= isConnected ? oldUnits.toInt128() : int128(0);
        _expectedPoolData[address(pool_)].disconnectedUnits += isConnected ? oldUnits.toInt128() : int128(0);
        _expectedPoolData[address(pool_)].connectedFlowRate -= isConnected ? oldFlowRate : int96(0);
        _expectedPoolData[address(pool_)].disconnectedFlowRate += isConnected ? oldFlowRate : int96(0);

        // Update Expected Member Data
        // TODO how does the flow rate change here
    }

    function _helperDistribute(
        ISuperToken _superToken,
        address caller,
        address from,
        ISuperfluidPool _pool,
        uint256 requestedAmount
    ) internal {
        (int256 fromRTBBefore,,,) = superToken.realtimeBalanceOfNow(from);

        uint256 actualAmount = sf.gda.estimateDistributionActualAmount(superToken, alice, pool, requestedAmount);

        address[] memory members = _poolMembers[address(_pool)].values();
        uint256[] memory memberBalancesBefore = new uint256[](members.length);

        for (uint256 i = 0; i < members.length; ++i) {
            (int256 memberRTB,,,) = superToken.realtimeBalanceOfNow(members[i]);
            memberBalancesBefore[i] = uint256(memberRTB);
        }

        vm.startPrank(caller);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distribute, (_superToken, from, _pool, requestedAmount, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();

        // Assert Distributor RTB
        (int256 fromRTBAfter,,,) = superToken.realtimeBalanceOfNow(from);
        assertEq(fromRTBAfter, fromRTBBefore - int256(actualAmount), "GDAv1.t D: Distributor RTB incorrect");

        if (members.length == 0) return;

        // Assert Members RTB
        uint256 amountPerUnit = actualAmount / _pool.getTotalUnits();
        for (uint256 i = 0; i < members.length; ++i) {
            (int256 memberRTB,,,) = superToken.realtimeBalanceOfNow(members[i]);
            uint256 amountReceived = sf.gda.isMemberConnected(superToken, address(pool), members[i])
                ? uint256(_pool.getUnits(members[i])) * amountPerUnit
                : 0;
            assertEq(uint256(memberRTB), memberBalancesBefore[i] + amountReceived, "GDAv1.t D: Member RTB incorrect");
        }
    }

    function _helperDistributeFlow(
        ISuperToken _superToken,
        address caller,
        address from,
        ISuperfluidPool _pool,
        int96 requestedFlowRate
    ) internal {
        vm.startPrank(caller);
        _superToken.distributeFlow(from, _pool, requestedFlowRate);
        vm.stopPrank();

        // Assert Outflow NFT is minted to distributor
        // Assert Inflow NFT is minted to pool
        _assertFlowNftOnDistributeFlow(_superToken, _pool, from, requestedFlowRate);
    }

    function _helperApprove(ISuperfluidPool _pool, address owner, address spender, uint256 amount) internal {
        vm.startPrank(owner);
        _pool.approve(spender, amount);
        vm.stopPrank();

        _assertPoolAllowance(_pool, owner, spender, amount);
    }

    function _helperIncreaseAllowance(ISuperfluidPool _pool, address owner, address spender, uint256 addedValue)
        internal
    {
        uint256 allowanceBefore = _pool.allowance(owner, spender);

        vm.startPrank(owner);
        _pool.increaseAllowance(spender, addedValue);
        vm.stopPrank();

        _assertPoolAllowance(_pool, owner, spender, allowanceBefore + addedValue);
    }

    function _helperDecreaseAllowance(ISuperfluidPool _pool, address owner, address spender, uint256 subtractedValue)
        internal
    {
        uint256 allowanceBefore = _pool.allowance(owner, spender);

        vm.startPrank(owner);
        _pool.decreaseAllowance(spender, subtractedValue);
        vm.stopPrank();

        _assertPoolAllowance(_pool, owner, spender, allowanceBefore - subtractedValue);
    }

    function _helperPoolUnitsTransfer(ISuperfluidPool _pool, address from, address to, uint256 amount) internal {
        uint256 fromBalanceOfBefore = _pool.balanceOf(from);
        uint256 toBalanceOfBefore = _pool.balanceOf(to);

        vm.startPrank(from);
        _pool.transfer(to, amount);
        vm.stopPrank();

        uint256 fromBalanceOfAfter = _pool.balanceOf(from);
        uint256 toBalanceOfAfter = _pool.balanceOf(to);
        assertEq(fromBalanceOfBefore - amount, fromBalanceOfAfter, "_helperPoolUnitsTransfer: from balance mismatch");
        assertEq(toBalanceOfBefore + amount, toBalanceOfAfter, "_helperPoolUnitsTransfer: to balance mismatch");
    }

    function _helperPoolUnitsTransferFrom(
        ISuperfluidPool _pool,
        address caller,
        address from,
        address to,
        uint256 amount
    ) internal {
        uint256 fromBalanceOfBefore = _pool.balanceOf(from);
        uint256 toBalanceOfBefore = _pool.balanceOf(to);
        uint256 allowanceBefore = _pool.allowance(from, caller);

        vm.startPrank(caller);
        _pool.transferFrom(from, to, amount);
        vm.stopPrank();

        uint256 fromBalanceOfAfter = _pool.balanceOf(from);
        uint256 toBalanceOfAfter = _pool.balanceOf(to);
        uint256 allowanceAfter = _pool.allowance(from, caller);
        assertEq(
            fromBalanceOfBefore - amount, fromBalanceOfAfter, "_helperPoolUnitsTransferFrom: from balance mismatch"
        );
        assertEq(toBalanceOfBefore + amount, toBalanceOfAfter, "_helperPoolUnitsTransferFrom: to balance mismatch");
        assertEq(allowanceBefore - amount, allowanceAfter, "_helperPoolUnitsTransferFrom: allowance mismatch");
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Functions
    //////////////////////////////////////////////////////////////////////////*/

    function _assertGlobalInvariants() internal override {
        super._assertGlobalInvariants();
        // @note we can rename this pool variable to currentPool
        _assertPoolUnits(pool);
    }

    function _assertPoolUnits(ISuperfluidPool _pool) internal {
        _assertPoolTotalUnits(_pool);
        _assertPoolConnectedUnits(_pool);
        _assertPoolDisconnectedUnits(_pool);
    }

    function _assertPoolDisconnectedUnits(ISuperfluidPool _pool) internal {
        int128 disconnectedUnits = uint256(_pool.getTotalDisconnectedUnits()).toInt256().toInt128();
        assertEq(
            _expectedPoolData[address(_pool)].disconnectedUnits,
            disconnectedUnits,
            "_assertPoolDisconnectedUnits: Pool disconnected units incorrect"
        );
    }

    function _assertPoolConnectedUnits(ISuperfluidPool _pool) internal {
        int128 connectedUnits = uint256(_pool.getTotalConnectedUnits()).toInt256().toInt128();

        assertEq(
            _expectedPoolData[address(_pool)].totalUnits - _expectedPoolData[address(_pool)].disconnectedUnits,
            connectedUnits,
            "_assertPoolConnectedUnits: Pool disconnected units incorrect"
        );
    }

    function _assertPoolTotalUnits(ISuperfluidPool _pool) internal {
        int128 totalUnits = uint256(_pool.getTotalUnits()).toInt256().toInt128();
        int128 totalSupply = _pool.totalSupply().toInt256().toInt128();

        assertEq(
            _expectedPoolData[address(_pool)].totalUnits,
            totalUnits,
            "_assertPoolTotalUnits: Pool total units incorrect"
        );
        assertEq(totalUnits, totalSupply, "_assertPoolTotalUnits: Pool total units != total supply");
    }

    function _assertPoolAllowance(ISuperfluidPool _pool, address owner, address spender, uint256 expectedAllowance)
        internal
    {
        assertEq(_pool.allowance(owner, spender), expectedAllowance, "_assertPoolAllowance: allowance mismatch");
    }

    function _assertPoolMemberNFT(
        ISuperfluidToken _superToken,
        ISuperfluidPool _pool,
        address _member,
        uint128 _newUnits
    ) internal {
        IPoolMemberNFT poolMemberNFT = SuperToken(address(_superToken)).POOL_MEMBER_NFT();
        uint256 tokenId = poolMemberNFT.getTokenId(address(_pool), address(_member));
        if (_newUnits > 0) {
            assertEq(poolMemberNFT.ownerOf(tokenId), _member, "_assertPoolMemberNFT: member doesn't own NFT");
        } else {
            vm.expectRevert(IPoolNFTBase.POOL_NFT_INVALID_TOKEN_ID.selector);
            poolMemberNFT.ownerOf(tokenId);
        }
    }

    function _assertFlowNftOnDistributeFlow(
        ISuperfluidToken _superToken,
        ISuperfluidPool _pool,
        address _distributor,
        int96 _newFlowRate
    ) internal {
        IConstantOutflowNFT constantOutflowNFT = SuperToken(address(_superToken)).CONSTANT_OUTFLOW_NFT();
        IConstantInflowNFT constantInflowNFT = SuperToken(address(_superToken)).CONSTANT_INFLOW_NFT();
        uint256 tokenId = constantOutflowNFT.getTokenId(address(_superToken), address(_distributor), address(_pool));
        if (_newFlowRate > 0) {
            assertEq(
                constantOutflowNFT.ownerOf(tokenId),
                _distributor,
                "_assertFlowNftOnDistributeFlow: distributor doesn't own outflow NFT"
            );
            assertEq(
                constantInflowNFT.ownerOf(tokenId),
                address(_pool),
                "_assertFlowNftOnDistributeFlow: distributor doesn't own inflow NFT"
            );
        } else {
            vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
            constantOutflowNFT.ownerOf(tokenId);

            vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
            constantInflowNFT.ownerOf(tokenId);
        }
    }

    struct PoolUpdateStep {
        uint8 u; // which user
        uint8 a; // action types: 0 update units, 1 distribute flow, 2 pool connection, 3 pool claim for, 4 distribute
        uint32 v; // action param
        uint16 dt; // time delta
    }

    function testPoolRandomSeqs(PoolUpdateStep[20] memory steps) external {
        uint256 N_MEMBERS = 5;

        for (uint256 i = 0; i < steps.length; ++i) {
            emit log_named_uint(">>> STEP", i);
            PoolUpdateStep memory s = steps[i];
            uint256 action = s.a % 5;
            uint256 u = 1 + s.u % N_MEMBERS;
            address user = TEST_ACCOUNTS[u];

            emit log_named_uint("user", u);
            emit log_named_uint("time delta", s.dt);
            emit log_named_uint("> timestamp", block.timestamp);
            emit log_named_address("tester", user);

            if (action == 0) {
                emit log_named_string("action", "updateMember");
                emit log_named_uint("units", s.v);
                _helperUpdateMemberUnits(pool, pool.admin(), user, s.v);
            } else if (action == 1) {
                emit log_named_string("action", "distributeFlow");
                emit log_named_uint("flow rate", s.v);
                _helperDistributeFlow(superToken, user, user, pool, int96(uint96(s.v)));
            } else if (action == 2) {
                address u4 = TEST_ACCOUNTS[1 + (s.v % N_MEMBERS)];
                emit log_named_string("action", "claimAll");
                emit log_named_address("claim for", u4);
                vm.startPrank(user);
                assert(pool.claimAll(u4));
                vm.stopPrank();
            } else if (action == 3) {
                bool doConnect = s.v % 2 == 0 ? false : true;
                emit log_named_string("action", "doConnectPool");
                emit log_named_string("doConnect", doConnect ? "true" : "false");
                doConnect ? _helperConnectPool(user, superToken, pool) : _helperDisconnectPool(user, superToken, pool);
            } else if (action == 4) {
                // TODO uncomment this and it should work
                // emit log_named_string("action", "distribute");
                // emit log_named_uint("distributionAmount", s.v);
                // _helperDistribute(superToken, user, user, pool, uint256(s.v));
            } else {
                assert(false);
            }

            {
                (int256 own, int256 fromPools, int256 buffer) =
                    sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
                int96 connectedFlowRate = pool.getTotalConnectedFlowRate();
                int96 nr = sf.gda.getNetFlow(superToken, address(pool));
                emit log_string("> pool before time warp");
                emit log_named_int("own", own);
                emit log_named_int("fromPoolsBalance", fromPools);
                emit log_named_int("buffer", buffer);
                emit log_named_int("pool net flow rate", nr);
            }

            emit log_named_uint("> dt", s.dt);
            vm.warp(block.timestamp + s.dt);

            {
                (int256 own, int256 fromPools, int256 buffer) =
                    sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
                int96 connectedFlowRate = pool.getTotalConnectedFlowRate();
                int96 nr = sf.gda.getNetFlow(superToken, address(pool));
                emit log_string("> pool before time warp");
                emit log_named_int("own", own);
                emit log_named_int("fromPoolsBalance", fromPools);
                emit log_named_int("buffer", buffer);
                emit log_named_int("pool net flow rate", nr);
            }
        }

        int96 flowRatesSum;
        {
            (int256 own, int256 fromPools, int256 buffer) =
                sf.gda.realtimeBalanceVectorAt(superToken, address(pool), block.timestamp);
            int96 poolDisconnectedRate = pool.getTotalDisconnectedFlowRate();
            (,, int96 poolAdjustmentRate) = sf.gda.getPoolAdjustmentFlowInfo(pool);
            int96 poolNetFlowRate = sf.gda.getNetFlow(superToken, address(pool));
            flowRatesSum = flowRatesSum + poolNetFlowRate;
        }

        for (uint256 i = 1; i <= N_MEMBERS; ++i) {
            (int256 own, int256 fromPools, int256 buffer) =
                sf.gda.realtimeBalanceVectorAt(superToken, TEST_ACCOUNTS[i], block.timestamp);
            int96 flowRate = sf.gda.getNetFlow(superToken, TEST_ACCOUNTS[i]);
            flowRatesSum = flowRatesSum + flowRate;
        }

        assertEq(flowRatesSum, 0, "GDAv1.t: flowRatesSum != 0");
    }
}
