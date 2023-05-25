// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";

import {
    SuperfluidFrameworkDeployer,
    TestResolver,
    SuperfluidLoader
} from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { ERC1820RegistryCompiled } from "../../contracts/libs/ERC1820RegistryCompiled.sol";
import { ISETH } from "../../contracts/interfaces/tokens/ISETH.sol";
import { SuperTokenDeployer } from "../../contracts/utils/SuperTokenDeployer.sol";
import { Superfluid } from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { UUPSProxy } from "../../contracts/upgradability/UUPSProxy.sol";
import { SuperTokenV1Library } from "../../contracts/apps/SuperTokenV1Library.sol";
import { TestToken } from "../../contracts/utils/SuperTokenDeployer.sol";
import { ISuperToken, SuperToken } from "../../contracts/superfluid/SuperToken.sol";

/// @title FoundrySuperfluidTester
/// @dev A contract that can be inherited from to test Superfluid agreements
/// Types of SuperToken to test:
/// - (0) | WRAPPER_SUPER_TOKEN: has underlying ERC20 token (e.g. USDC)
/// - (1) | NATIVE_ASSET_SUPER_TOKEN: underlying asset is the native gas token (e.g. ETH)
/// - (2) | PURE_SUPER_TOKEN: has no underlying AND is purely a SuperToken
/// - (3) | CUSTOM_WRAPPER_SUPER_TOKEN: has underlying ERC20 token (e.g. USDC) AND has custom SuperToken logic
/// - (4) | CUSTOM_PURE_SUPER_TOKEN: has no underlying AND is purely a SuperToken AND has custom SuperToken logic
/// TODO: write more documentation about this file
contract FoundrySuperfluidTester is Test {
    using SuperTokenV1Library for ISuperToken;
    using EnumerableSet for EnumerableSet.Bytes32Set;
    using SafeCast for uint256;
    using SafeCast for int256;

    struct RealtimeBalance {
        int256 availableBalance;
        uint256 deposit;
        uint256 owedDeposit;
        uint256 timestamp;
    }

    struct FlowInfo {
        uint256 timestamp;
        int96 flowRate;
        uint256 deposit;
        uint256 owedDeposit;
    }

    struct IDASubscriptionParams {
        ISuperToken superToken;
        address publisher;
        address subscriber;
        uint32 indexId;
    }

    error INVALID_TEST_SUPER_TOKEN_TYPE();

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperTokenDeployer internal immutable superTokenDeployer;
    int8 internal immutable testSuperTokenType;

    uint256 internal constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 internal constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    string internal constant DEFAULT_TEST_TOKEN_TYPE = "WRAPPER_SUPER_TOKEN";
    string internal constant TOKEN_TYPE_ENV_KEY = "TOKEN_TYPE";

    address internal constant admin = address(0x420);
    address internal constant alice = address(0x421);
    address internal constant bob = address(0x422);
    address internal constant carol = address(0x423);
    address internal constant dan = address(0x424);
    address internal constant eve = address(0x425);
    address internal constant frank = address(0x426);
    address internal constant grace = address(0x427);
    address internal constant heidi = address(0x428);
    address internal constant ivan = address(0x429);
    address[] internal TEST_ACCOUNTS = [admin, alice, bob, carol, dan, eve, frank, grace, heidi, ivan];

    uint256 internal immutable N_TESTERS;

    SuperfluidFrameworkDeployer.Framework internal sf;

    /// @dev The current underlying token being tested (applies only to wrapper super tokens)
    TestToken internal token;

    /// @dev The current super token being tested
    ISuperToken internal superToken;

    /// @dev The expected total supply of the current super token being tested
    uint256 private _expectedTotalSupply;

    /// @notice A mapping from super token to account to realtime balance data snapshot
    /// @dev Used for validating that balances are correct
    mapping(ISuperToken => mapping(address account => RealtimeBalance balanceSnapshot)) internal _balanceSnapshots;

    /// @notice A mapping from super token to account to flowIDs of outflows from the account for the CFA
    mapping(ISuperToken => mapping(address account => EnumerableSet.Bytes32Set flowIDs)) internal _outflows;

    /// @notice A mapping from super token to account to flowIDs of inflows to the account for the CFA
    mapping(ISuperToken => mapping(address account => EnumerableSet.Bytes32Set flowIDs)) internal _inflows;

    /// @notice A mapping from super token to account to indexIDs of indexes of the account for the IDA
    mapping(ISuperToken => mapping(address account => EnumerableSet.Bytes32Set indexIDs)) internal _indexIDs;

    /// @notice A mapping from super token to subId to sub.indexValue for the IDA
    mapping(ISuperToken => mapping(bytes32 subId => uint128 indexValue)) internal _lastUpdatedSubIndexValues;

    constructor(uint8 nTesters) {
        // etch erc1820
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);

        // deploy SuperfluidFrameworkDeployer
        // which deploys in its constructor:
        // - TestGovernance
        // - Host
        // - CFA
        // - IDA
        // - ConstantOutflowNFT logic
        // - ConstantInflowNFT logic
        // - SuperToken logic
        // - SuperTokenFactory
        // - Resolver
        // - SuperfluidLoader
        // - CFAv1Forwarder
        sfDeployer = new SuperfluidFrameworkDeployer();
        sf = sfDeployer.getFramework();

        // deploy SuperTokenDeployer
        superTokenDeployer = new SuperTokenDeployer(
            address(sf.superTokenFactory),
            address(sf.resolver)
        );

        // add superTokenDeployer as admin to the resolver so it can register the SuperTokens
        sf.resolver.addAdmin(address(superTokenDeployer));

        require(nTesters <= TEST_ACCOUNTS.length, "too many testers");
        N_TESTERS = nTesters;

        // Set the token type being tested
        string memory tokenType = vm.envOr(TOKEN_TYPE_ENV_KEY, DEFAULT_TEST_TOKEN_TYPE);
        bytes32 hashedTokenType = keccak256(abi.encode(tokenType));

        testSuperTokenType = hashedTokenType == keccak256(abi.encode("WRAPPER_SUPER_TOKEN"))
            ? int8(0)
            : hashedTokenType == keccak256(abi.encode("NATIVE_ASSET_SUPER_TOKEN"))
                ? int8(1)
                : hashedTokenType == keccak256(abi.encode("PURE_SUPER_TOKEN"))
                    ? int8(2)
                    : hashedTokenType == keccak256(abi.encode("CUSTOM_WRAPPER_SUPER_TOKEN"))
                        ? int8(3)
                        : hashedTokenType == keccak256(abi.encode("CUSTOM_PURE_SUPER_TOKEN")) ? int8(4) : int8(-1);

        if (testSuperTokenType == -1) revert INVALID_TEST_SUPER_TOKEN_TYPE();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Test Setup/Harness
    //////////////////////////////////////////////////////////////////////////*/
    function setUp() public virtual {
        _setUpSuperToken();
    }

    function _setUpWrapperSuperToken() internal {
        (token, superToken) = superTokenDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        for (uint256 i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);

            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }
    }

    function _setUpNativeAssetSuperToken() internal {
        (superToken) = superTokenDeployer.deployNativeAssetSuperToken("Super ETH", "ETHx");
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            vm.startPrank(TEST_ACCOUNTS[i]);
            vm.deal(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            ISETH(address(superToken)).upgradeByETH{ value: INIT_SUPER_TOKEN_BALANCE }();
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }
    }

    function _setUpPureSuperToken() internal {
        uint256 initialSupply = INIT_SUPER_TOKEN_BALANCE * N_TESTERS;
        (superToken) = superTokenDeployer.deployPureSuperToken("Super MR", "MRx", initialSupply);
        _expectedTotalSupply = initialSupply;
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            superToken.transfer(TEST_ACCOUNTS[i], INIT_SUPER_TOKEN_BALANCE);
        }
    }

    function _setUpSuperToken() internal {
        if (testSuperTokenType == 0) {
            _setUpWrapperSuperToken();
        } else if (testSuperTokenType == 1) {
            _setUpNativeAssetSuperToken();
        } else if (testSuperTokenType == 2) {
            _setUpPureSuperToken();
        } else if (testSuperTokenType == 3) {
            // _setUpCustomWrapperSuperToken();
        } else if (testSuperTokenType == 4) {
            // _setUpCustomPureSuperToken();
        } else {
            revert("invalid test token type");
        }
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Invariant Definitions
    //////////////////////////////////////////////////////////////////////////*/

    /// @notice Liquidity Sum Invariant definition
    /// @dev Liquidity Sum Invariant: Expected Total Supply === Liquidity Sum
    /// Liquidity Sum = sum of available balance, deposit and owed deposit for all users
    /// @return bool Liquidity Sum Invariant holds true
    function _definitionLiquiditySumInvariant() internal view returns (bool) {
        int256 liquiditySum = _helperGetSuperTokenLiquiditySum(superToken);

        return int256(_expectedTotalSupply) == liquiditySum;
    }

    function _definitionAumGtEqRtbSumInvariant() internal view returns (bool) {
        uint256 aum = _helperGetSuperTokenAum(superToken);
        int256 rtbSum = _helperGetSuperTokenLiquiditySum(superToken);

        return aum >= uint256(rtbSum);
    }

    function _defintionAumGtEqSuperTokenTotalSupplyInvariant() internal view returns (bool) {
        uint256 aum = _helperGetSuperTokenAum(superToken);
        uint256 totalSupply = superToken.totalSupply();

        return aum >= totalSupply;
    }

    /// @notice Net Flow Rate Sum Invariant definition
    /// @dev Net Flow Rate Sum Invariant: Sum of all net flow rates === 0
    /// @return bool Net Flow Rate Sum Invariant holds true
    function _definitionNetFlowRateSumInvariant() internal view returns (bool) {
        int96 netFlowRateSum = _helperGetNetFlowRateSum(superToken);
        return netFlowRateSum == 0;
    }

    function _assertGlobalInvariants() internal {
        _assertInvariantLiquiditySum();
        _assertInvariantNetFlowRateSum();
        _assertInvariantAumGtEqRtbSum();
        _assertInvariantAumGtEqSuperTokenTotalSupply();
    }

    function _assertInvariantLiquiditySum() internal {
        assertTrue(_definitionLiquiditySumInvariant(), "Invariant: Liquidity Sum Invariant");
    }

    function _assertInvariantNetFlowRateSum() internal {
        assertTrue(_definitionNetFlowRateSumInvariant(), "Invariant: Net Flow Rate Sum Invariant");
    }

    function _assertInvariantAumGtEqRtbSum() internal {
        assertTrue(_definitionAumGtEqRtbSumInvariant(), "Invariant: AUM > RTB Sum");
    }

    function _assertInvariantAumGtEqSuperTokenTotalSupply() internal {
        assertTrue(_defintionAumGtEqSuperTokenTotalSupplyInvariant(), "Invariant: AUM > SuperToken Total Supply");
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assume Helpers
    //////////////////////////////////////////////////////////////////////////*/
    /// @notice Assume a valid flow rate
    /// @dev Flow rate must be greater than 0 and less than or equal to int32.max
    function _assumeValidFlowRate(int96 desiredFlowRate) internal pure returns (int96 flowRate) {
        vm.assume(desiredFlowRate > 0);
        vm.assume(desiredFlowRate <= int96(type(int32).max));
        flowRate = int96(int32(desiredFlowRate));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/

    // Getter Helpers
    function _helperGetSuperTokenAum(ISuperToken superToken_) internal view returns (uint256) {
        return testSuperTokenType == 0
            ? _helperGetWrapperSuperTokenAUM(superToken_)
            : testSuperTokenType == 1
                ? _helperGetNativeAssetSuperTokenAUM(superToken_)
                : testSuperTokenType == 2 ? _helperGetPureSuperTokenAUM(superToken_) : 0;
    }

    function _helperGetWrapperSuperTokenAUM(ISuperToken superToken_) internal view returns (uint256) {
        return ISuperToken(superToken_.getUnderlyingToken()).balanceOf(address(superToken_));
    }

    function _helperGetNativeAssetSuperTokenAUM(ISuperToken superToken_) internal view returns (uint256) {
        return address(superToken_).balance;
    }

    function _helperGetPureSuperTokenAUM(ISuperToken superToken_) internal view returns (uint256) {
        return superToken_.totalSupply();
    }

    function _helperGetSuperTokenLiquiditySum(ISuperToken superToken_) internal view returns (int256 liquiditySum) {
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            (int256 availableBalance, uint256 deposit, uint256 owedDeposit,) =
                superToken_.realtimeBalanceOfNow(address(TEST_ACCOUNTS[i]));

            liquiditySum += availableBalance + int256(deposit) - int256(owedDeposit);
        }
    }

    function _helperGetNetFlowRateSum(ISuperToken superToken_) internal view returns (int96 netFlowRateSum) {
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            netFlowRateSum += superToken_.getNetFlowRate(address(TEST_ACCOUNTS[i]));
        }
    }

    /// @notice A helper function which tries to get an IDA subscription
    /// @dev This is needed in tests because it reverts if the subscriptio does not exist
    function _helperTryGetSubscription(ISuperToken superToken_, bytes32 subId)
        internal
        view
        returns (bool approved, uint128 units, uint256 pendingDistribution)
    {
        try sf.ida.getSubscriptionByID(superToken_, subId) returns (
            address, uint32, bool isApproved, uint128 subUnits, uint256 pending
        ) {
            approved = isApproved;
            units = subUnits;
            pendingDistribution = pending;
        } catch { }
    }

    function _helperGetAllFlowInfo(ISuperToken superToken_, address sender, address receiver)
        internal
        returns (
            FlowInfo memory flowInfoBefore,
            FlowInfo memory senderFlowInfoBefore,
            FlowInfo memory receiverFlowInfoBefore
        )
    {
        flowInfoBefore = _helperGetFlowInfo(superToken_, sender, receiver);
        senderFlowInfoBefore = _helperGetAccountFlowInfo(superToken_, sender);
        receiverFlowInfoBefore = _helperGetAccountFlowInfo(superToken_, receiver);
    }

    function _helperGetFlowInfo(ISuperToken superToken_, address sender, address receiver)
        internal
        view
        returns (FlowInfo memory flowInfo)
    {
        (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit) =
            superToken_.getFlowInfo(sender, receiver);
        flowInfo = FlowInfo(timestamp, flowRate, deposit, owedDeposit);
    }

    function _helperGetAccountFlowInfo(ISuperToken superToken_, address account)
        internal
        view
        returns (FlowInfo memory flowInfo)
    {
        (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit) =
            sf.cfa.getAccountFlowInfo(superToken_, account);
        flowInfo = FlowInfo(timestamp, flowRate, deposit, owedDeposit);
    }

    // Time Warp Helpers
    function _helperWarpToCritical(address account, int96 netFlowRate, uint256 secondsCritical) internal {
        assertTrue(secondsCritical > 0, "_helperWarpToCritical: secondsCritical must be > 0 to reach critical");
        (int256 ab,,) = superToken.realtimeBalanceOf(account, block.timestamp);
        int256 timeToZero = ab / netFlowRate;
        uint256 amountToWarp = timeToZero.toUint256() + secondsCritical;
        vm.warp(block.timestamp + amountToWarp);
        assertTrue(superToken.isAccountCriticalNow(account), "_helperWarpToCritical: account is not critical");
    }

    function _helperWarpToInsolvency(
        address account,
        int96 netFlowRate,
        uint256 liquidationPeriod,
        uint256 secondsInsolvent
    ) internal {
        assertTrue(secondsInsolvent > 0, "_helperWarpToInsolvency: secondsInsolvent must be > 0 to reach insolvency");
        (int256 ab,,) = superToken.realtimeBalanceOf(account, block.timestamp);
        int256 timeToZero = ab / netFlowRate;
        uint256 amountToWarp = timeToZero.toUint256() + liquidationPeriod + secondsInsolvent;
        vm.warp(block.timestamp + amountToWarp);
        assertFalse(superToken.isAccountSolventNow(account), "_helperWarpToInsolvency: account is still solvent");
    }

    function _generateFlowId(address sender, address receiver) private pure returns (bytes32 id) {
        return keccak256(abi.encode(sender, receiver));
    }

    function _generateFlowOperatorId(address sender, address flowOperator) private pure returns (bytes32 id) {
        return keccak256(abi.encode("flowOperator", sender, flowOperator));
    }

    function _generatePublisherId(address publisher, uint32 indexId) private pure returns (bytes32 iId) {
        return keccak256(abi.encodePacked("publisher", publisher, indexId));
    }

    function _generateSubscriptionId(address subscriber, bytes32 iId) private pure returns (bytes32 sId) {
        return keccak256(abi.encodePacked("subscription", subscriber, iId));
    }

    // Write Helpers
    // Write Helpers - Testing State Changes
    function _helperTakeBalanceSnapshot(ISuperToken superToken_, address account) internal {
        (int256 avb, uint256 deposit, uint256 owedDeposit, uint256 time) = superToken_.realtimeBalanceOfNow(account);
        RealtimeBalance memory balanceSnapshot = RealtimeBalance(avb, deposit, owedDeposit, time);
        _balanceSnapshots[superToken_][account] = balanceSnapshot;
    }

    function _helperAddInflowsAndOutflowsToTestState(address sender, address receiver) internal {
        bytes32 flowId = _generateFlowId(sender, receiver);

        EnumerableSet.Bytes32Set storage outflows = _outflows[superToken][sender];
        if (!outflows.contains(flowId)) {
            outflows.add(flowId);
        }

        EnumerableSet.Bytes32Set storage inflows = _inflows[superToken][receiver];
        if (!inflows.contains(flowId)) {
            inflows.add(flowId);
        }
    }

    function _helperRemoveInflowsAndOutflowsFromTestState(address sender, address receiver) internal {
        bytes32 flowId = _generateFlowId(sender, receiver);

        EnumerableSet.Bytes32Set storage outflows = _outflows[superToken][sender];
        if (outflows.contains(flowId)) {
            outflows.remove(flowId);
        }

        EnumerableSet.Bytes32Set storage inflows = _inflows[superToken][receiver];
        if (inflows.contains(flowId)) {
            inflows.remove(flowId);
        }
    }

    // Write Helpers - ConstantFlowAgreementV1
    function _helperCreateFlow(address sender, address receiver, int96 flowRate) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (FlowInfo memory flowInfoBefore, FlowInfo memory senderFlowInfoBefore, FlowInfo memory receiverFlowInfoBefore) =
            _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(sender);
        superToken.createFlow(receiver, flowRate);
        vm.stopPrank();

        _helperAddInflowsAndOutflowsToTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowInfo(sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);
    }

    function _helperUpdateFlow(address sender, address receiver, int96 flowRate) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (FlowInfo memory flowInfoBefore, FlowInfo memory senderFlowInfoBefore, FlowInfo memory receiverFlowInfoBefore) =
            _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(sender);
        superToken.updateFlow(receiver, flowRate);
        vm.stopPrank();

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowInfo(sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);
    }

    function _helperDeleteFlow(address caller, address sender, address receiver) internal {
        (FlowInfo memory flowInfoBefore, FlowInfo memory senderFlowInfoBefore, FlowInfo memory receiverFlowInfoBefore) =
            _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(caller);
        superToken.deleteFlow(sender, receiver);
        vm.stopPrank();

        _helperRemoveInflowsAndOutflowsFromTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = -flowInfoBefore.flowRate;

        _assertFlowInfoIsEmpty(sender, receiver);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);
    }

    function _helperCreateFlowFrom(
        ISuperToken superToken_,
        address operator,
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (FlowInfo memory flowInfoBefore, FlowInfo memory senderFlowInfoBefore, FlowInfo memory receiverFlowInfoBefore) =
            _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(operator);
        superToken_.createFlowFrom(sender, receiver, flowRate);
        vm.stopPrank();

        _helperAddInflowsAndOutflowsToTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowInfo(sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        // TODO
        // Assert that flow rate allowance has been deducted accordingly
    }

    function _helperUpdateFlowFrom(
        ISuperToken superToken_,
        address operator,
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (FlowInfo memory flowInfoBefore, FlowInfo memory senderFlowInfoBefore, FlowInfo memory receiverFlowInfoBefore) =
            _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(operator);
        superToken_.updateFlowFrom(sender, receiver, flowRate);
        vm.stopPrank();

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowInfo(sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        // TODO
        // Assert that flow rate allowance has been deducted accordingly (if flow rate is increased by delta amount)
    }

    function _helperDeleteFlowFrom(ISuperToken superToken_, address operator, address sender, address receiver)
        internal
    {
        (FlowInfo memory flowInfoBefore, FlowInfo memory senderFlowInfoBefore, FlowInfo memory receiverFlowInfoBefore) =
            _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(operator);
        superToken_.deleteFlowFrom(sender, receiver);
        vm.stopPrank();

        _helperRemoveInflowsAndOutflowsFromTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = -flowInfoBefore.flowRate;

        _assertFlowInfoIsEmpty(sender, receiver);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);
    }

    // Write Helpers - InstantDistributionAgreementV1

    function _helperCreateIndex(ISuperToken superToken_, address publisher, uint32 indexId) internal {
        vm.startPrank(publisher);
        superToken_.createIndex(indexId);
        vm.stopPrank();

        _assertIndexData(superToken_, publisher, indexId, true, 0, 0, 0);

        _indexIDs[superToken_][publisher].add(_generatePublisherId(publisher, indexId));
    }

    function _helperUpdateIndexValue(ISuperToken superToken_, address publisher, uint32 indexId, uint128 newIndexValue)
        internal
    {
        (, uint128 indexValueBefore, uint128 totalUnitsApprovedBefore, uint128 totalUnitsPendingBefore) =
            superToken_.getIndex(publisher, indexId);

        (int256 publisherAvbBefore, uint256 publisherDepositBefore,,) = superToken_.realtimeBalanceOfNow(publisher);

        uint128 indexValueDelta = newIndexValue - indexValueBefore;
        int256 distributionAmount =
            uint256(indexValueDelta * (totalUnitsApprovedBefore + totalUnitsPendingBefore)).toInt256();
        uint256 depositDelta = indexValueDelta * totalUnitsPendingBefore;

        vm.startPrank(publisher);
        superToken_.updateIndexValue(indexId, newIndexValue);
        vm.stopPrank();

        _assertIndexData(
            superToken_, publisher, indexId, true, newIndexValue, totalUnitsApprovedBefore, totalUnitsPendingBefore
        );
        (int256 publisherAvbAfter, uint256 publisherDepositAfter,,) = superToken_.realtimeBalanceOfNow(publisher);
        assertEq(publisherAvbAfter, publisherAvbBefore - distributionAmount, "Update Index: Publisher AVB");
        assertEq(publisherDepositAfter, publisherDepositBefore + depositDelta, "Update Index: Publisher Deposit");

        // TODO we could actually save all the subscribers of an index and loop over them down the line
        // Assert that balance for subscriber has been updated (dependent on approval status)
    }

    function _helperDistribute(ISuperToken superToken_, address publisher, uint32 indexId, uint256 amount) internal {
        (, uint128 indexValueBefore, uint128 totalUnitsApprovedBefore, uint128 totalUnitsPendingBefore) =
            superToken_.getIndex(publisher, indexId);

        (int256 publisherAvbBefore, uint256 publisherDepositBefore,,) = superToken_.realtimeBalanceOfNow(publisher);

        (uint256 actualAmount, uint128 newIndexValue) = superToken_.calculateDistribution(publisher, indexId, amount);

        uint128 indexValueDelta = newIndexValue - indexValueBefore;
        int256 distributionAmount =
            uint256(indexValueDelta * (totalUnitsApprovedBefore + totalUnitsPendingBefore)).toInt256();

        assertEq(actualAmount, distributionAmount.toUint256(), "Distribute: Distribution Amount");
        uint256 depositDelta = indexValueDelta * totalUnitsPendingBefore;

        vm.startPrank(publisher);
        superToken_.distribute(indexId, amount);
        vm.stopPrank();

        _assertIndexData(
            superToken_, publisher, indexId, true, newIndexValue, totalUnitsApprovedBefore, totalUnitsPendingBefore
        );
        (int256 publisherAvbAfter, uint256 publisherDepositAfter,,) = superToken_.realtimeBalanceOfNow(publisher);
        assertEq(publisherAvbAfter, publisherAvbBefore - distributionAmount, "Distribute: Publisher AVB");
        assertEq(publisherDepositAfter, publisherDepositBefore + depositDelta, "Distribute: Publisher Deposit");

        // TODO we could actually save all the subscribers of an index and loop over them down the line
        // Assert that balance for subscriber has been updated (dependent on approval status)
    }

    function _helperUpdateSubscriptionUnits(IDASubscriptionParams memory params, uint128 units) internal {
        bytes32 subId =
            _generateSubscriptionId(params.subscriber, _generatePublisherId(params.publisher, params.indexId));
        (, uint128 indexValue, uint128 totalUnitsApprovedBefore, uint128 totalUnitsPendingBefore) =
            params.superToken.getIndex(params.publisher, params.indexId);

        (bool approved,,) = _helperTryGetSubscription(params.superToken, subId);

        vm.startPrank(params.publisher);
        params.superToken.updateSubscriptionUnits(params.indexId, params.subscriber, units);
        vm.stopPrank();

        uint128 expectedTotalUnitsApproved = approved ? totalUnitsApprovedBefore + units : totalUnitsApprovedBefore;
        uint128 expectedTotalUnitsPending = approved ? totalUnitsPendingBefore : totalUnitsPendingBefore + units;

        _assertIndexData(
            params.superToken,
            params.publisher,
            params.indexId,
            true,
            indexValue,
            expectedTotalUnitsApproved,
            expectedTotalUnitsPending
        );

        // subIndexValue here is equivalent because we update the subscriber
        // without updating the indexValue here.
        uint256 subIndexValue = indexValue;
        _lastUpdatedSubIndexValues[params.superToken][subId] = indexValue;
        uint256 pending = approved ? 0 : indexValue - subIndexValue * units;

        _assertSubscriptionData(params.superToken, subId, approved, units, pending);
    }

    function _helperApproveSubscription(IDASubscriptionParams memory params) internal {
        bytes32 subId =
            _generateSubscriptionId(params.subscriber, _generatePublisherId(params.publisher, params.indexId));

        // Get Balance Data Before
        (int256 publisherAvbBefore, uint256 publisherDepositBefore,,) =
            params.superToken.realtimeBalanceOfNow(params.publisher);
        (int256 subscriberAvbBefore,,,) = params.superToken.realtimeBalanceOfNow(params.subscriber);

        // Get Index/Subscription Data
        (, uint128 indexValue, uint128 totalUnitsApprovedBefore, uint128 totalUnitsPendingBefore) =
            params.superToken.getIndex(params.publisher, params.indexId);
        (, uint128 unitsBefore,) = _helperTryGetSubscription(params.superToken, subId);

        uint128 subIndexValueDelta = indexValue - _lastUpdatedSubIndexValues[params.superToken][subId];
        int256 balanceDelta = uint256(subIndexValueDelta * unitsBefore).toInt256();

        // Assert Subscription Data Before
        _assertSubscriptionData(params.superToken, subId, false, unitsBefore, balanceDelta.toUint256());

        // Execute Approve Subscription
        {
            vm.startPrank(params.subscriber);
            params.superToken.approveSubscription(params.publisher, params.indexId);
            vm.stopPrank();
        }

        // Assert Publisher Balance Data
        {
            (int256 publisherAvbAfter, uint256 publisherDepositAfter,,) =
                params.superToken.realtimeBalanceOfNow(params.publisher);
            assertEq(publisherAvbAfter, publisherAvbBefore, "Approve: Publisher AVB");
            assertEq(
                publisherDepositAfter,
                (publisherDepositBefore.toInt256() - balanceDelta).toUint256(),
                "Approve: Publisher Deposit"
            );
        }

        // Assert Subscription Balance Data
        {
            (int256 subscriberAvbAfter,,,) = params.superToken.realtimeBalanceOfNow(params.subscriber);
            assertEq(subscriberAvbAfter, subscriberAvbBefore + balanceDelta, "Approve: Subscriber AVB");
        }

        // Assert Subscription and Index Data
        {
            _assertSubscriptionData(params.superToken, subId, true, unitsBefore, 0);
            _assertIndexData(
                params.superToken,
                params.publisher,
                params.indexId,
                true,
                indexValue,
                totalUnitsApprovedBefore + unitsBefore,
                totalUnitsPendingBefore - unitsBefore
            );
        }

        _lastUpdatedSubIndexValues[params.superToken][subId] = indexValue;
    }

    function _helperRevokeSubscription(IDASubscriptionParams memory params) internal {
        bytes32 subId =
            _generateSubscriptionId(params.subscriber, _generatePublisherId(params.publisher, params.indexId));

        // Get Balance Data Before
        (int256 publisherAvbBefore,,,) = params.superToken.realtimeBalanceOfNow(params.publisher);
        (int256 subscriberAvbBefore,,,) = params.superToken.realtimeBalanceOfNow(params.subscriber);

        // Get Index/Subscription Data
        (, uint128 indexValue, uint128 totalUnitsApprovedBefore, uint128 totalUnitsPendingBefore) =
            params.superToken.getIndex(params.publisher, params.indexId);
        (, uint128 unitsBefore,) = _helperTryGetSubscription(params.superToken, subId);

        uint128 subIndexValueDelta = indexValue - _lastUpdatedSubIndexValues[params.superToken][subId];
        int256 balanceDelta = uint256(subIndexValueDelta * unitsBefore).toInt256();

        // Assert Subscription Data Before
        _assertSubscriptionData(params.superToken, subId, true, unitsBefore, 0);

        // Execute Revoke Subscription
        {
            vm.startPrank(params.subscriber);
            params.superToken.revokeSubscription(params.publisher, params.indexId);
            vm.stopPrank();
        }

        // Assert Publisher Balance Data
        {
            (int256 publisherAvbAfter,,,) = params.superToken.realtimeBalanceOfNow(params.publisher);
            assertEq(publisherAvbAfter, publisherAvbBefore, "Revoke: Publisher AVB");
        }

        // Assert Subscription Balance Data
        {
            (int256 subscriberAvbAfter,,,) = params.superToken.realtimeBalanceOfNow(params.subscriber);
            assertEq(subscriberAvbAfter, subscriberAvbBefore + balanceDelta, "Revoke: Subscriber AVB");
        }

        // Assert Subscription and Index Data
        {
            _assertSubscriptionData(params.superToken, subId, false, unitsBefore, 0);
            _assertIndexData(
                params.superToken,
                params.publisher,
                params.indexId,
                true,
                indexValue,
                totalUnitsApprovedBefore - unitsBefore,
                totalUnitsPendingBefore + unitsBefore
            );
        }

        _lastUpdatedSubIndexValues[params.superToken][subId] = indexValue;
    }

    function _helperDeleteSubscription(IDASubscriptionParams memory params) internal {
        bytes32 subId =
            _generateSubscriptionId(params.subscriber, _generatePublisherId(params.publisher, params.indexId));

        // Get Balance Data Before
        (int256 publisherAvbBefore, uint256 publisherDepositBefore,,) =
            params.superToken.realtimeBalanceOfNow(params.publisher);
        (int256 subscriberAvbBefore,,,) = params.superToken.realtimeBalanceOfNow(params.subscriber);

        // Get Index/Subscription Data
        (, uint128 indexValue, uint128 totalUnitsApprovedBefore, uint128 totalUnitsPendingBefore) =
            params.superToken.getIndex(params.publisher, params.indexId);
        (bool approvedBefore, uint128 unitsBefore,) = _helperTryGetSubscription(params.superToken, subId);

        uint128 subIndexValueDelta = indexValue - _lastUpdatedSubIndexValues[params.superToken][subId];
        int256 balanceDelta = uint256(subIndexValueDelta * unitsBefore).toInt256();

        // Assert Subscription Data Before
        _assertSubscriptionData(
            params.superToken, subId, approvedBefore, unitsBefore, approvedBefore ? 0 : balanceDelta.toUint256()
        );

        // Execute Delete Subscription
        {
            vm.startPrank(params.publisher);
            params.superToken.deleteSubscription(params.publisher, params.indexId, params.subscriber);
            vm.stopPrank();
        }

        // Assert Publisher Balance Data
        {
            (int256 publisherAvbAfter, uint256 publisherDeposit,,) =
                params.superToken.realtimeBalanceOfNow(params.publisher);
            assertEq(publisherAvbAfter, publisherAvbBefore, "Delete: Publisher AVB");
            assertEq(publisherDeposit, publisherDepositBefore - balanceDelta.toUint256(), "Delete: Publisher Deposit");
        }

        // Assert Subscription Balance Data
        {
            (int256 subscriberAvbAfter,,,) = params.superToken.realtimeBalanceOfNow(params.subscriber);
            assertEq(subscriberAvbAfter, subscriberAvbBefore + balanceDelta, "Delete: Subscriber AVB");
        }

        // Assert Subscription and Index Data
        {
            _assertSubscriptionData(params.superToken, subId, false, 0, 0);
            _assertIndexData(
                params.superToken,
                params.publisher,
                params.indexId,
                true,
                indexValue,
                totalUnitsApprovedBefore - unitsBefore,
                totalUnitsPendingBefore - unitsBefore
            );
        }

        _lastUpdatedSubIndexValues[params.superToken][subId] = 0;
    }

    function _helperClaim(
        ISuperToken superToken_,
        address caller,
        address publisher,
        uint32 indexId,
        address subscriber
    ) internal {
        bytes32 subId = _generateSubscriptionId(subscriber, _generatePublisherId(publisher, indexId));

        // Get Balance Data Before
        (, uint256 publisherDepositBefore,,) = superToken_.realtimeBalanceOfNow(publisher);
        (int256 subscriberAvbBefore,,,) = superToken_.realtimeBalanceOfNow(subscriber);

        // Get Index/Subscription Data
        (, uint128 indexValue,,) = superToken_.getIndex(publisher, indexId);
        (, uint128 unitsBefore,) = _helperTryGetSubscription(superToken_, subId);

        uint128 subIndexValueDelta = indexValue - _lastUpdatedSubIndexValues[superToken_][subId];
        int256 pendingDistribution = uint256(subIndexValueDelta * unitsBefore).toInt256();

        // Execute Claim
        vm.startPrank(caller);
        superToken_.claim(publisher, indexId, subscriber);
        vm.stopPrank();

        // Assert Publisher Balance Data
        {
            (, uint256 publisherDeposit,,) = superToken_.realtimeBalanceOfNow(publisher);
            assertEq(
                publisherDeposit, publisherDepositBefore - pendingDistribution.toUint256(), "Claim: Publisher Deposit"
            );
        }

        // Assert Subscription Balance Data
        {
            (int256 subscriberAvbAfter,,,) = superToken_.realtimeBalanceOfNow(subscriber);
            assertEq(subscriberAvbAfter, subscriberAvbBefore + pendingDistribution, "Claim: Subscriber AVB");
        }

        _lastUpdatedSubIndexValues[superToken_][subId] = indexValue;
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/

    // ConstantFlowAgreement Assertions

    /// @dev Asserts that a single flow has been updated as expected
    function _assertFlowInfo(
        address sender,
        address receiver,
        int96 expectedFlowRate,
        uint256 expectedLastUpdated,
        uint256 expectedOwedDeposit
    ) internal {
        (uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit) =
            superToken.getFlowInfo(sender, receiver);

        uint256 expectedDeposit = superToken.getBufferAmountByFlowRate(expectedFlowRate);

        assertEq(flowRate, expectedFlowRate, "FlowInfo: flow rate");
        assertEq(lastUpdated, expectedLastUpdated, "FlowInfo: last updated");
        assertEq(deposit, expectedDeposit, "FlowInfo: deposit");
        assertEq(owedDeposit, expectedOwedDeposit, "FlowInfo: owed deposit");
    }

    /// @dev Asserts that a single flow has been removed on deletion
    function _assertFlowInfoIsEmpty(address sender, address receiver) internal {
        _assertFlowInfo(sender, receiver, 0, 0, 0);
    }

    function _assertAccountFlowInfo(address account, int96 flowRateDelta, FlowInfo memory flowInfoBefore, bool isSender)
        internal
    {
        (uint256 lastUpdated, int96 netFlowRate, uint256 deposit, uint256 owedDeposit) =
            sf.cfa.getAccountFlowInfo(superToken, account);

        int96 expectedNetFlowRate = flowInfoBefore.flowRate + (isSender ? -flowRateDelta : flowRateDelta);
        uint256 depositDelta = superToken.getBufferAmountByFlowRate(flowRateDelta < 0 ? -flowRateDelta : flowRateDelta);
        uint256 expectedDeposit = flowInfoBefore.deposit + (isSender ? depositDelta : 0);
        // TODO: we may need to pass expectedTimestamp at some point
        assertEq(lastUpdated, block.timestamp, "AccountFlowInfo: lastUpdated");
        assertEq(netFlowRate, expectedNetFlowRate, "AccountFlowInfo: net flow rate");
        assertEq(deposit, expectedDeposit, "AccountFlowInfo: deposit");
        // TODO: we may need to pass expectedOwedDeposit at some point
        assertEq(owedDeposit, 0, "AccountFlowInfo: owed deposit");
    }

    function _assertIndexData(
        ISuperToken superToken_,
        address publisher,
        uint32 indexId,
        bool expectedExist,
        uint128 expectedIndexValue,
        uint128 expectedTotalUnitsApproved,
        uint128 expectedTotalUnitsPending
    ) internal {
        (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending) =
            superToken_.getIndex(publisher, indexId);

        assertEq(exist, expectedExist, "IndexData: exist");
        assertEq(indexValue, expectedIndexValue, "IndexData: index value");
        assertEq(totalUnitsApproved, expectedTotalUnitsApproved, "IndexData: total units approved");
        assertEq(totalUnitsPending, expectedTotalUnitsPending, "IndexData: total units pending");
    }

    function _assertSubscriptionData(
        ISuperToken superToken_,
        bytes32 subscriptionId,
        bool expectedApproved,
        uint128 expectedUnits,
        uint256 expectedPending
    ) internal {
        (,, bool approved, uint128 units, uint256 pending) = superToken_.getSubscriptionByID(subscriptionId);
        assertEq(approved, expectedApproved, "SubscriptionData: approved");
        assertEq(units, expectedUnits, "SubscriptionData: units");
        assertEq(pending, expectedPending, "SubscriptionData: pending");
    }
}
