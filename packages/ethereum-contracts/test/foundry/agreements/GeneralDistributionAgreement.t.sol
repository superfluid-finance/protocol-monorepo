// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { IBeacon } from "@openzeppelin/contracts/proxy/beacon/IBeacon.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import "../FoundrySuperfluidTester.sol";
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
contract GeneralDistributionAgreementV1IntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;
    using EnumerableSet for EnumerableSet.AddressSet;
    using SafeCast for uint256;
    using SafeCast for int256;

    struct UpdateMemberData {
        address member;
        uint64 newUnits;
    }

    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);

    SuperfluidPool public currentPool;
    uint256 public liquidationPeriod;

    constructor() FoundrySuperfluidTester(6) { }

    function setUp() public override {
        super.setUp();
        vm.startPrank(alice);
        currentPool = SuperfluidPool(address(superToken.createPool(alice)));
        _addAccount(address(currentPool));
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
        vm.assume(owner != address(currentPool));

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
        uint256 lastUpdated = block.timestamp;
        sf.gda.setFlowInfo(
            abi.encode(superToken),
            from,
            address(to),
            FlowRate.wrap(int128(uint128(newFlowRate))),
            FlowRate.wrap(int128(uint128(newFlowRateDelta)))
        );

        vm.warp(1000);

        (bool exist, GeneralDistributionAgreementV1.FlowDistributionData memory setFlowDistributionData) =
            sf.gda.getFlowDistributionData(superToken, from, address(to));

        assertEq(true, exist, "flow distribution data does not exist");

        assertEq(int96(uint96(newFlowRate)), setFlowDistributionData.flowRate, "flowRate not equal");

        assertEq(lastUpdated, setFlowDistributionData.lastUpdated, "lastUpdated not equal");

        assertEq(0, setFlowDistributionData.buffer, "buffer not equal");
        assertEq(
            int96(FlowRate.unwrap(sf.gda.getFlowRate(abi.encode(superToken), from, address(to)))),
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
        (, PDPoolIndex memory setPdpIndex) = sf.gda.setAndGetPDPIndex(eff, address(anotherPool), pdpIndex);
        vm.stopPrank();

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
    function testAdjustBufferUpdatesFlowDistributionData(address from, int32 oldFlowRate, int32 newFlowRate) public {
        vm.assume(newFlowRate >= 0);

        uint256 expectedBuffer = uint256(int256(newFlowRate)) * liquidationPeriod;
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            address(currentPool),
            from,
            FlowRate.wrap(int128(oldFlowRate)),
            FlowRate.wrap(int128(newFlowRate))
        );

        (bool exist, GeneralDistributionAgreementV1.FlowDistributionData memory flowDistributionData) =
            sf.gda.getFlowDistributionData(superToken, from, address(currentPool));
        assertEq(exist, true, "flow distribution data does not exist");
        assertEq(flowDistributionData.buffer, expectedBuffer, "buffer not equal");
        assertEq(flowDistributionData.flowRate, int96(newFlowRate), "buffer not equal");
        assertEq(
            int96(FlowRate.unwrap(sf.gda.getFlowRate(abi.encode(superToken), from, address(currentPool)))),
            int96(newFlowRate),
            "_getFlowRate: flow rate not equal"
        );
        assertEq(
            sf.gda.getFlowRate(superToken, from, ISuperfluidPool(currentPool)),
            int96(newFlowRate),
            "getFlowRate: flow rate not equal"
        );
    }

    // Adjust Buffer => UniversalIndexData modified
    function testAdjustBufferUpdatesUniversalIndexData(address from, int32 oldFlowRate, int32 newFlowRate) public {
        vm.assume(newFlowRate >= 0);

        uint256 bufferDelta = uint256(int256(newFlowRate)) * liquidationPeriod; // expected buffer == buffer delta
            // because of fresh state
        (, GeneralDistributionAgreementV1.UniversalIndexData memory fromUindexDataBefore) =
            sf.gda.getUIndexAndUindexData(abi.encode(superToken), from);
        (, GeneralDistributionAgreementV1.UniversalIndexData memory gdaUindexDataBefore) =
            sf.gda.getUIndexAndUindexData(abi.encode(superToken), address(sf.gda));
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            address(currentPool),
            from,
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
        assertEq(
            currentPool.proxiableUUID(), keccak256("org.superfluid-finance.contracts.SuperfluidPool.implementation")
        );
    }

    function testPositiveBalanceIsPatricianPeriodNow(address account) public {
        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, account);
        assertEq(isPatricianPeriod, true);
    }

    function testNegativeBalanceIsPatricianPeriodNowIsTrue() public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        _helperConnectPool(bob, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, bob, 1);

        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);

        _helperWarpToCritical(superToken, alice, 1);

        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, alice);
        assertEq(isPatricianPeriod, true);
    }

    function testNegativeBalanceIsPatricianPeriodNowIsFalse() public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        _helperConnectPool(bob, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, bob, 1);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, currentPool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);

        if (actualDistributionFlowRate > 0) {
            _helperWarpToInsolvency(superToken, alice, liquidationPeriod, 1);
        }

        (bool isPatricianPeriod,) = sf.gda.isPatricianPeriodNow(superToken, alice);
        assertEq(isPatricianPeriod, false);
    }

    function testNegativeBalanceIsPatricianPeriodNowIsFalseWithZeroDeposit() public {
        uint256 aliceBalance = superToken.balanceOf(alice);
        int96 flowRate = aliceBalance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        vm.startPrank(sf.governance.owner());
        sf.governance.setRewardAddress(sf.host, ISuperfluidToken(address(0)), alice);
        vm.stopPrank();

        _helperConnectPool(bob, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, bob, 1);

        (int256 aliceRTB, uint256 deposit,,) = superToken.realtimeBalanceOfNow(alice);

        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);
        int96 fr = sf.gda.getFlowRate(superToken, alice, currentPool);

        vm.warp(block.timestamp + (INIT_SUPER_TOKEN_BALANCE / uint256(uint96(fr))));

        (aliceRTB, deposit,,) = superToken.realtimeBalanceOfNow(alice);

        _helperDistributeFlow(superToken, bob, alice, currentPool, 0);

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
        sf.gda.connectPool(currentPool, "0x");
        vm.stopPrank();
    }

    function testRevertNonHostDisconnectPool(address notHost) public {
        vm.assume(notHost != address(sf.host));
        vm.startPrank(notHost);
        vm.expectRevert("unauthorized host");
        sf.gda.disconnectPool(currentPool, "0x");
        vm.stopPrank();
    }

    function testConnectPool(address caller) public {
        _helperConnectPool(caller, superToken, currentPool);
    }

    function testDisconnectPool(address caller) public {
        _helperConnectPool(caller, superToken, currentPool);
        _helperDisconnectPool(caller, superToken, currentPool);
    }

    function testRevertDistributeFlowToNonPool(int96 requestedFlowRate) public {
        vm.assume(requestedFlowRate >= 0);
        vm.assume(requestedFlowRate < int96(type(int64).max));
        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_ONLY_SUPER_TOKEN_POOL.selector);
        superToken.distributeFlow(alice, ISuperfluidPool(bob), requestedFlowRate);
        vm.stopPrank();
    }

    // function testRevertIfDistributeFlowToZeroDoesNotExist() public {
    //     vm.startPrank(alice);
    //     vm.expectRevert(IGeneralDistributionAgreementV1.GDA_FLOW_DOES_NOT_EXIST.selector);
    //     superToken.distributeFlow(alice, currentPool, 0);
    //     vm.stopPrank();
    // }

    function testRevertDistributeFlowWithNegativeFlowRate(int96 requestedFlowRate) public {
        vm.assume(requestedFlowRate < 0);

        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_NO_NEGATIVE_FLOW_RATE.selector);
        superToken.distributeFlow(alice, currentPool, requestedFlowRate);
        vm.stopPrank();
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
            abi.encodeCall(sf.gda.distribute, (superToken, alice, currentPool, requestedAmount, new bytes(0))),
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
            abi.encodeCall(sf.gda.distributeFlow, (superToken, alice, currentPool, requestedFlowRate, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function testRevertDistributeFlowInsufficientBalance() public {
        uint256 balance = superToken.balanceOf(alice);
        balance /= 4 hours;
        int96 tooBigFlowRate = int96(int256(balance)) + 1;

        _helperConnectPool(bob, superToken, currentPool);

        _helperUpdateMemberUnits(currentPool, alice, bob, 1);
        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_INSUFFICIENT_BALANCE.selector);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distributeFlow, (superToken, alice, currentPool, tooBigFlowRate, new bytes(0))),
            new bytes(0)
        );
        vm.stopPrank();
    }

    function testRevertLiquidateNonCriticalDistributor(int32 flowRate, int96 units) public {
        vm.assume(flowRate > 0);
        _helperConnectPool(bob, superToken, currentPool);

        _helperUpdateMemberUnits(currentPool, alice, bob, uint96(units));

        _helperDistributeFlow(superToken, alice, alice, currentPool, flowRate);

        vm.startPrank(bob);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_NON_CRITICAL_SENDER.selector);
        superToken.distributeFlow(alice, currentPool, 0);
        vm.stopPrank();
    }

    function testRevertDistributeInsufficientBalance() public {
        uint256 balance = superToken.balanceOf(alice);

        _helperConnectPool(bob, superToken, currentPool);

        _helperUpdateMemberUnits(currentPool, alice, bob, 1);

        vm.startPrank(alice);
        vm.expectRevert(IGeneralDistributionAgreementV1.GDA_INSUFFICIENT_BALANCE.selector);
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.distribute, (superToken, alice, currentPool, balance + 1, new bytes(0))),
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
        currentPool.operatorConnectMember(member, doConnect, time);
        vm.stopPrank();
    }

    function testRevertPoolUpdateMemberThatIsPool(uint128 units) public {
        vm.assume(units < uint128(type(int128).max));

        vm.expectRevert(ISuperfluidPool.SUPERFLUID_POOL_NO_POOL_MEMBERS.selector);
        vm.startPrank(alice);
        currentPool.updateMemberUnits(address(currentPool), units);
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
        vm.assume(member != address(currentPool));
        vm.assume(member != address(0));

        vm.startPrank(address(sf.governance.owner()));
        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPool(member, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, member, 1);
        _helperDistributeFlow(superToken, alice, alice, currentPool, int96(int64(distributionFlowRate)));
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
        vm.assume(member != address(currentPool));
        vm.startPrank(address(sf.governance.owner()));

        uint256 minimumDeposit = 4 hours * uint256(minDepositMultiplier);
        sf.governance.setSuperTokenMinimumDeposit(sf.host, superToken, minimumDeposit);
        vm.stopPrank();

        _helperConnectPool(member, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, member, 1);
        _helperDistributeFlow(superToken, alice, alice, currentPool, int96(distributionFlowRate));
        (, uint256 buffer,,) = superToken.realtimeBalanceOfNow(alice);
        assertTrue(buffer >= minimumDeposit, "GDAv1.t: Buffer should be >= minDeposit");
    }

    function testDistributeFlowToConnectedMemberSendingToCFA(int32 flowRate, uint64 units) public {
        vm.assume(flowRate > 0);
        // alice creates currentPool in setUp()
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        _helperUpdateMemberUnits(currentPool, alice, bob, memberUnits);

        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);

        // bob sends a flow of 1 to carol
        _helperConnectPool(bob, superToken, currentPool);
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
        _helperDistributeViaGDA(superToken, alice, alice, currentPool, distributionAmount);
    }

    function testDistributeFlowToEmptyPool(int32 flowRate) public {
        vm.assume(flowRate >= 0);
        _helperDistributeFlow(superToken, alice, alice, currentPool, flowRate);
        assertEq(sf.gda.getFlowRate(superToken, alice, currentPool), 0, "GDAv1.t: distributionFlowRate should be 0");
    }

    function testDistributeFlowCriticalLiquidation(uint64 units) public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        _helperConnectPool(bob, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, bob, memberUnits);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, currentPool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);

        if (actualDistributionFlowRate > 0) {
            _helperWarpToCritical(superToken, alice, 1);
            _helperDistributeFlow(superToken, bob, alice, currentPool, 0);
        }
    }

    function testDistributeFlowInsolventLiquidation(uint64 units) public {
        uint256 balance = superToken.balanceOf(alice);
        int96 flowRate = balance.toInt256().toInt96() / type(int32).max;
        int96 requestedDistributionFlowRate = int96(flowRate);

        uint128 memberUnits = uint128(units);

        _helperConnectPool(bob, superToken, currentPool);
        _helperUpdateMemberUnits(currentPool, alice, bob, memberUnits);
        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);

        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, currentPool, requestedDistributionFlowRate);

        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedDistributionFlowRate);

        if (actualDistributionFlowRate > 0) {
            _helperWarpToInsolvency(superToken, alice, liquidationPeriod, 1);
            _helperDistributeFlow(superToken, bob, alice, currentPool, 0);
        }
    }

    function testDistributeToDisconnectedMembers(
        UpdateMemberData[5] memory members,
        uint256 distributionAmount,
        uint16 /* warpTime */
    ) public {
        address distributor = alice;
        uint256 distributorBalance = superToken.balanceOf(distributor);

        vm.assume(members.length > 0);
        vm.assume(distributionAmount < distributorBalance);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperUpdateMemberUnits(currentPool, alice, members[i].member, members[i].newUnits);
        }
        _helperDistributeViaGDA(superToken, alice, alice, currentPool, distributionAmount);
    }

    function testDistributeToConnectedMembers(
        UpdateMemberData[5] memory members,
        uint256 distributionAmount,
        uint16 /* warpTime */
    ) public {
        address distributor = alice;
        uint256 distributorBalance = superToken.balanceOf(distributor);

        vm.assume(members.length > 0);
        vm.assume(distributionAmount < distributorBalance);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperConnectPool(members[i].member, superToken, currentPool);
            _helperUpdateMemberUnits(currentPool, alice, members[i].member, members[i].newUnits);
        }
        _helperDistributeViaGDA(superToken, alice, alice, currentPool, distributionAmount);
    }

    function testDistributeFlowToConnectedMembers(UpdateMemberData[5] memory members, int32 flowRate, uint16 /* warpTime */)
        public
    {
        vm.assume(members.length > 0);
        vm.assume(flowRate > 0);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperConnectPool(members[i].member, superToken, currentPool);
            _helperUpdateMemberUnits(currentPool, alice, members[i].member, members[i].newUnits);
        }

        _helperDistributeFlow(superToken, alice, alice, currentPool, 100);
        assertEq(
            sf.gda.getPoolAdjustmentFlowRate(superToken, address(currentPool)),
            0,
            "GDAv1.t: Pool adjustment rate is non-zero"
        );
    }

    function testDistributeFlowToDisconnectedMember(UpdateMemberData memory member, int32 flowRate) public {
        vm.assume(flowRate > 0);
        
        _helperUpdateMemberUnits(currentPool, alice, member.member, 1);

        _helperDistributeFlow(superToken, alice, alice, currentPool, 1);
    }

    function testDistributeFlowToUnconnectedMembers(UpdateMemberData[5] memory members, int32 flowRate, uint16 warpTime)
        public
    {
        vm.assume(flowRate > 0);
        vm.assume(members.length > 0);

        for (uint256 i = 0; i < members.length; ++i) {
            _helperUpdateMemberUnits(currentPool, alice, members[i].member, members[i].newUnits);
        }

        int96 requestedFlowRate = flowRate;
        _helperDistributeFlow(superToken, alice, alice, currentPool, requestedFlowRate);
        (int96 actualDistributionFlowRate,) =
            sf.gda.estimateFlowDistributionActualFlowRate(superToken, alice, currentPool, requestedFlowRate);

        vm.warp(block.timestamp + warpTime);

        uint128 totalUnits = currentPool.getTotalUnits();

        for (uint256 i; i < members.length; ++i) {
            address member = members[i].member;
            if (member != address(0)) {
                // @note we test realtimeBalanceOfNow here as well
                (int256 memberRTB,,) = sf.gda.realtimeBalanceOf(superToken, member, block.timestamp);
                (int256 rtbNow,,,) = sf.gda.realtimeBalanceOfNow(superToken, member);
                assertEq(memberRTB, rtbNow, "testDistributeFlowToUnconnectedMembers: rtb != rtbNow");

                assertEq(
                    currentPool.getTotalDisconnectedFlowRate(),
                    actualDistributionFlowRate,
                    "testDistributeFlowToUnconnectedMembers: pendingDistributionFlowRate != actualDistributionFlowRate"
                );
                (int256 memberClaimable,) = currentPool.getClaimableNow(member);
                assertEq(
                    memberClaimable,
                    totalUnits > 0
                        ? (actualDistributionFlowRate * int96(int256(uint256(warpTime))))
                            * int96(uint96(members[i].newUnits)) / uint256(totalUnits).toInt256()
                        : int256(0),
                    "testDistributeFlowToUnconnectedMembers: memberClaimable != (actualDistributionFlowRate * warpTime) / totalUnits"
                );
                assertEq(memberRTB, 0, "testDistributeFlowToUnconnectedMembers: memberRTB != 0");
                vm.prank(member);
                currentPool.claimAll();

                (memberRTB,,) = sf.gda.realtimeBalanceOf(superToken, member, block.timestamp);
                assertEq(
                    memberRTB, memberClaimable, "testDistributeFlowToUnconnectedMembers: memberRTB != memberClaimable"
                );
            }
        }
    }

    // Pool ERC20 functions

    function testApproveOnly(address owner, address spender, uint256 amount) public {
        vm.assume(owner != address(0));
        vm.assume(spender != address(0));

        _helperSuperfluidPoolApprove(currentPool, owner, spender, amount);
    }

    function testIncreaseAllowance(address owner, address spender, uint256 addedValue) public {
        vm.assume(owner != address(0));
        vm.assume(spender != address(0));

        _helperSuperfluidPoolIncreaseAllowance(currentPool, owner, spender, addedValue);
    }

    function testDecreaseAllowance(address owner, address spender, uint256 addedValue, uint256 subtractedValue)
        public
    {
        vm.assume(owner != address(0));
        vm.assume(spender != address(0));
        vm.assume(addedValue >= subtractedValue);

        _helperSuperfluidPoolIncreaseAllowance(currentPool, owner, spender, addedValue);
        _helperSuperfluidPoolDecreaseAllowance(currentPool, owner, spender, subtractedValue);
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
        _helperUpdateMemberUnits(currentPool, alice, from, uint128(int128(unitsAmount)));

        vm.startPrank(from);
        vm.expectRevert(ISuperfluidPool.SUPERFLUID_POOL_NO_POOL_MEMBERS.selector);
        currentPool.transfer(address(currentPool), uint256(uint128(transferAmount)));
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
        _helperUpdateMemberUnits(currentPool, alice, from, uint128(int128(unitsAmount)));

        _helperSuperfluidPoolUnitsTransfer(currentPool, from, to, uint256(uint128(transferAmount)));
    }

    function testApproveAndTransferFrom(address owner, address spender, int128 transferAmount) public {
        vm.assume(transferAmount > 0);
        vm.assume(spender != address(0));
        vm.assume(owner != address(0));
        vm.assume(spender != owner);
        _helperUpdateMemberUnits(currentPool, alice, owner, uint128(int128(transferAmount)));
        _helperSuperfluidPoolApprove(currentPool, owner, spender, uint256(uint128(transferAmount)));
        _helperSuperfluidPoolUnitsTransferFrom(currentPool, spender, owner, spender, uint256(uint128(transferAmount)));
    }

    function testIncreaseAllowanceAndTransferFrom(address owner, address spender, int128 transferAmount) public {
        vm.assume(transferAmount > 0);
        vm.assume(spender != address(0));
        vm.assume(owner != address(0));
        vm.assume(spender != owner);
        _helperUpdateMemberUnits(currentPool, alice, owner, uint128(int128(transferAmount)));
        _helperSuperfluidPoolIncreaseAllowance(currentPool, owner, spender, uint256(uint128(transferAmount)));
        _helperSuperfluidPoolUnitsTransferFrom(currentPool, spender, owner, spender, uint256(uint128(transferAmount)));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Functions
    //////////////////////////////////////////////////////////////////////////*/

    struct PoolUpdateStep {
        uint8 u; // which user
        uint8 a; // action types: 0 update units, 1 distribute flow, 2 currentPool connection, 3 currentPool claim for,
            // 4 distribute
        uint32 v; // action param
        uint16 dt; // time delta
    }

    function testPoolRandomSeqs(PoolUpdateStep[20] memory steps) external {
        uint256 N_MEMBERS = 5;

        for (uint256 i = 0; i < steps.length; ++i) {
            emit log_named_string("", "");
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
                _helperUpdateMemberUnits(currentPool, currentPool.admin(), user, s.v);
            } else if (action == 1) {
                emit log_named_string("action", "distributeFlow");
                emit log_named_uint("flow rate", s.v);
                if (sf.gda.getFlowRate(superToken, user, currentPool) == 0) {
                    vm.assume(s.v > 0);
                }
                _helperDistributeFlow(superToken, user, user, currentPool, int96(uint96(s.v)));
            } else if (action == 2) {
                address u4 = TEST_ACCOUNTS[1 + (s.v % N_MEMBERS)];
                emit log_named_string("action", "claimAll");
                emit log_named_address("claim for", u4);
                vm.startPrank(user);
                assert(currentPool.claimAll(u4));
                vm.stopPrank();
            } else if (action == 3) {
                bool doConnect = s.v % 2 == 0 ? false : true;
                emit log_named_string("action", "doConnectPool");
                emit log_named_string("doConnect", doConnect ? "true" : "false");
                doConnect
                    ? _helperConnectPool(user, superToken, currentPool)
                    : _helperDisconnectPool(user, superToken, currentPool);
            } else if (action == 4) {
                emit log_named_string("action", "distribute");
                emit log_named_uint("distributionAmount", s.v);
                _helperDistributeViaGDA(superToken, user, user, currentPool, uint256(s.v));
            } else {
                assert(false);
            }

            {
                (int256 own, int256 fromPools, int256 buffer) =
                    sf.gda.realtimeBalanceVectorAt(superToken, address(currentPool), block.timestamp);
                int96 nr = sf.gda.getNetFlow(superToken, address(currentPool));
                emit log_string("> currentPool before time warp");
                emit log_named_int("own", own);
                emit log_named_int("fromPoolsBalance", fromPools);
                emit log_named_int("buffer", buffer);
                emit log_named_int("currentPool net flow rate", nr);
            }

            emit log_named_uint("> dt", s.dt);
            vm.warp(block.timestamp + s.dt);

            {
                (int256 own, int256 fromPools, int256 buffer) =
                    sf.gda.realtimeBalanceVectorAt(superToken, address(currentPool), block.timestamp);
                int96 nr = sf.gda.getNetFlow(superToken, address(currentPool));
                emit log_string("> currentPool before time warp");
                emit log_named_int("own", own);
                emit log_named_int("fromPoolsBalance", fromPools);
                emit log_named_int("buffer", buffer);
                emit log_named_int("currentPool net flow rate", nr);
            }
        }

        int96 flowRatesSum;
        {
            int96 poolNetFlowRate = sf.gda.getNetFlow(superToken, address(currentPool));
            flowRatesSum = flowRatesSum + poolNetFlowRate;
        }

        for (uint256 i = 1; i <= N_MEMBERS; ++i) {
            int96 flowRate = sf.gda.getNetFlow(superToken, TEST_ACCOUNTS[i]);
            flowRatesSum = flowRatesSum + flowRate;
        }

        assertEq(flowRatesSum, 0, "GDAv1.t: flowRatesSum != 0");
    }
}
