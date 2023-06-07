// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";

import { ERC1820RegistryCompiled } from "../../contracts/libs/ERC1820RegistryCompiled.sol";
import {
    SuperfluidFrameworkDeployer,
    TestResolver,
    SuperfluidLoader
} from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { Superfluid } from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { ISETH } from "../../contracts/interfaces/tokens/ISETH.sol";
import { UUPSProxy } from "../../contracts/upgradability/UUPSProxy.sol";
import { ConstantFlowAgreementV1 } from "../../contracts/agreements/ConstantFlowAgreementV1.sol";
import { SuperTokenV1Library } from "../../contracts/apps/SuperTokenV1Library.sol";
import { ISuperToken, SuperToken } from "../../contracts/superfluid/SuperToken.sol";
import { TestToken } from "../../contracts/utils/TestToken.sol";

/// @title FoundrySuperfluidTester
/// @dev A contract that can be inherited from to test Superfluid agreements
///
/// Types of SuperToken to test:
/// - (0) | WRAPPER_SUPER_TOKEN: has underlying ERC20 token (e.g. USDC)
/// - (1) | NATIVE_ASSET_SUPER_TOKEN: underlying asset is the native gas token (e.g. ETH)
/// - (2) | PURE_SUPER_TOKEN: has no underlying AND is purely a SuperToken
/// - (3) | CUSTOM_WRAPPER_SUPER_TOKEN: has underlying ERC20 token (e.g. USDC) AND has custom SuperToken logic
/// - (4) | CUSTOM_PURE_SUPER_TOKEN: has no underlying AND is purely a SuperToken AND has custom SuperToken logic
///
/// Definitions
/// - AUM: Assets Under Management
///     - Wrapper: underlyingToken.balanceOf(address(superToken))
///     - Native: address(superToken).balance
///     - Pure: superToken.totalSupply()
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

    struct IDASubscriptionParams {
        ISuperToken superToken;
        address publisher;
        address subscriber;
        uint32 indexId;
    }

    error INVALID_TEST_SUPER_TOKEN_TYPE();

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
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
        // deploy ERC1820 registry
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
        sfDeployer.deployTestFramework();
        sf = sfDeployer.getFramework();

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

    /// @notice Deploys a Wrapper SuperToken with an underlying test token and gives tokens to the test accounts
    function _setUpWrapperSuperToken() internal {
        (token, superToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        for (uint256 i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);

            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
            _helperTakeBalanceSnapshot(superToken, TEST_ACCOUNTS[i]);
        }
    }

    /// @notice Deploys a Native Asset SuperToken and gives tokens to the test accounts
    /// @dev We use vm.deal to give each account a starting amount of ether
    function _setUpNativeAssetSuperToken() internal {
        (superToken) = sfDeployer.deployNativeAssetSuperToken("Super ETH", "ETHx");
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            vm.startPrank(TEST_ACCOUNTS[i]);
            vm.deal(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            ISETH(address(superToken)).upgradeByETH{ value: INIT_SUPER_TOKEN_BALANCE }();
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
            _helperTakeBalanceSnapshot(superToken, TEST_ACCOUNTS[i]);
        }
    }

    /// @notice Deploys a Pure SuperToken and gives tokens to the test accounts
    /// @dev This contract receives the total supply then doles it out to the test accounts
    function _setUpPureSuperToken() internal {
        uint256 initialSupply = INIT_SUPER_TOKEN_BALANCE * N_TESTERS;
        (superToken) = sfDeployer.deployPureSuperToken("Super MR", "MRx", initialSupply);
        _expectedTotalSupply = initialSupply;
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            superToken.transfer(TEST_ACCOUNTS[i], INIT_SUPER_TOKEN_BALANCE);
            _helperTakeBalanceSnapshot(superToken, TEST_ACCOUNTS[i]);
        }
    }

    /// @notice Deploys a SuperToken based on the testSuperTokenType selected
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

    /// @notice AUM-RTB Accounting Invariant
    /// @dev AUM-RTB Accounting Invariant: AUM >= RTB Sum
    /// This invariant ensures that the RTB accounting is correct.
    /// @return bool AUM-RTB Accounting Invariant holds true
    function _definitionAumGtEqRtbSumInvariant() internal view returns (bool) {
        uint256 aum = _helperGetSuperTokenAum(superToken);
        int256 rtbSum = _helperGetSuperTokenLiquiditySum(superToken);

        return aum >= uint256(rtbSum);
    }

    /// @notice Protocol Solvency Invariant definition
    /// @dev Protocol Solvency Invariant: AUM >= Total Supply
    /// This invariant ensures that if all users want to downgrade their SuperToken, they can
    /// @return bool Protocol Solvency Invariant holds true
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

    /// @notice Asserts that the global invariants hold true
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

    /// @notice Gets the AUM for a SuperToken
    /// @dev This is dependent on the testSuperTokenType selected
    /// @param superToken_ The SuperToken to get the AUM for
    /// @return uint256 The AUM for the SuperToken
    function _helperGetSuperTokenAum(ISuperToken superToken_) internal view returns (uint256) {
        return testSuperTokenType == 0
            ? _helperGetWrapperSuperTokenAUM(superToken_)
            : testSuperTokenType == 1
                ? _helperGetNativeAssetSuperTokenAUM(superToken_)
                : testSuperTokenType == 2 ? _helperGetPureSuperTokenAUM(superToken_) : 0;
    }

    /// @notice Gets the AUM for Wrapper SuperToken's
    /// @param superToken_ The SuperToken to get the AUM for
    /// @return uint256 The AUM for the SuperToken
    function _helperGetWrapperSuperTokenAUM(ISuperToken superToken_) internal view returns (uint256) {
        return ISuperToken(superToken_.getUnderlyingToken()).balanceOf(address(superToken_));
    }

    /// @notice Gets the AUM for Native Asset SuperToken's
    /// @param superToken_ The SuperToken to get the AUM for
    /// @return uint256 The AUM for the SuperToken
    function _helperGetNativeAssetSuperTokenAUM(ISuperToken superToken_) internal view returns (uint256) {
        return address(superToken_).balance;
    }

    /// @notice Gets the AUM for Pure SuperToken's
    /// @param superToken_ The SuperToken to get the AUM for
    /// @return uint256 The AUM for the SuperToken
    function _helperGetPureSuperTokenAUM(ISuperToken superToken_) internal view returns (uint256) {
        return superToken_.totalSupply();
    }

    /// @notice Gets the liquidity sum for a SuperToken
    /// @param superToken_ The SuperToken to get the liquidity sum for
    /// @return liquiditySum The liquidity sum for the SuperToken
    function _helperGetSuperTokenLiquiditySum(ISuperToken superToken_) internal view returns (int256 liquiditySum) {
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            (int256 availableBalance, uint256 deposit, uint256 owedDeposit,) =
                superToken_.realtimeBalanceOfNow(TEST_ACCOUNTS[i]);

            liquiditySum += availableBalance + int256(deposit) - int256(owedDeposit);
        }
    }

    /// @notice Gets the net flow rate sum for a SuperToken
    /// @dev This is the sum of net flow rates of the test accounts
    /// @param superToken_ The SuperToken to get the net flow rate sum for
    /// @return netFlowRateSum The net flow rate sum for the SuperToken
    function _helperGetNetFlowRateSum(ISuperToken superToken_) internal view returns (int96 netFlowRateSum) {
        for (uint256 i = 0; i < N_TESTERS; ++i) {
            netFlowRateSum += superToken_.getNetFlowRate(TEST_ACCOUNTS[i]);
        }
    }

    /// @notice Tries to get an IDA subscription
    /// @dev This is needed in tests because it reverts if the subscriptio does not exist
    /// @param superToken_ The SuperToken to get the subscription for
    /// @param subId The ID of the subscription to get
    /// @return approved Whether the subscription exists
    /// @return units The units of the subscription
    /// @return pendingDistribution The pending distribution of the subscription
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

    /// @notice Gets flow info and account flow info for a sender and receiver
    /// @param superToken_ The SuperToken to get the flow info for
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @return flowInfo The flow info between a sender-receiver
    /// @return senderFlowInfo The account flow info for a sender
    /// @return receiverFlowInfo The account flow info for a receiver
    function _helperGetAllFlowInfo(ISuperToken superToken_, address sender, address receiver)
        internal
        returns (
            ConstantFlowAgreementV1.FlowData memory flowInfo,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfo,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfo
        )
    {
        flowInfo = _helperGetFlowInfo(superToken_, sender, receiver);
        senderFlowInfo = _helperGetAccountFlowInfo(superToken_, sender);
        receiverFlowInfo = _helperGetAccountFlowInfo(superToken_, receiver);
    }

    /// @notice Gets flow info for a sender and receiver
    /// @param superToken_ The SuperToken to get the flow info for
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @return flowInfo The flow info between a sender-receiver
    function _helperGetFlowInfo(ISuperToken superToken_, address sender, address receiver)
        internal
        view
        returns (ConstantFlowAgreementV1.FlowData memory flowInfo)
    {
        (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit) =
            superToken_.getFlowInfo(sender, receiver);
        flowInfo = ConstantFlowAgreementV1.FlowData(timestamp, flowRate, deposit, owedDeposit);
    }

    /// @notice Gets account flow info for an account
    /// @param superToken_ The SuperToken to get the account flow info for
    /// @param account The account to get the account flow info for
    /// @return flowInfo The account flow info for an account
    function _helperGetAccountFlowInfo(ISuperToken superToken_, address account)
        internal
        view
        returns (ConstantFlowAgreementV1.FlowData memory flowInfo)
    {
        (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit) =
            sf.cfa.getAccountFlowInfo(superToken_, account);
        flowInfo = ConstantFlowAgreementV1.FlowData(timestamp, flowRate, deposit, owedDeposit);
    }

    // Time Warp Helpers
    /// @notice Warps to a timestamp where account is critical
    /// @param superToken_ The SuperToken
    /// @param account The account whose balance we want to "drain"
    /// @param secondsCritical The number of seconds to warp to after the account is critical
    function _helperWarpToCritical(ISuperToken superToken_, address account, uint256 secondsCritical) internal {
        int96 netFlowRate = superToken_.getNetFlowRate(account);
        assertTrue(netFlowRate < 0, "_helperWarpToCritical: netFlowRate must be less than 0 to reach critical");
        assertTrue(secondsCritical > 0, "_helperWarpToCritical: secondsCritical must be > 0 to reach critical");
        (int256 ab,,) = superToken_.realtimeBalanceOf(account, block.timestamp);
        int256 timeToZero = ab / netFlowRate;
        uint256 amountToWarp = timeToZero.toUint256() + secondsCritical;
        vm.warp(block.timestamp + amountToWarp);
        assertTrue(superToken_.isAccountCriticalNow(account), "_helperWarpToCritical: account is not critical");
    }

    /// @notice Warps to a timestamp where account is insolvent
    /// @param superToken_ The SuperToken
    /// @param account The account whose balance we want to "drain"
    /// @param liquidationPeriod The liquidation period of the SuperToken
    /// @param secondsInsolvent The number of seconds to warp to after the account is insolvent
    function _helperWarpToInsolvency(
        ISuperToken superToken_,
        address account,
        uint256 liquidationPeriod,
        uint256 secondsInsolvent
    ) internal {
        int96 netFlowRate = superToken_.getNetFlowRate(account);
        assertTrue(netFlowRate < 0, "_helperWarpToCritical: netFlowRate must be less than 0 to reach critical");
        assertTrue(secondsInsolvent > 0, "_helperWarpToInsolvency: secondsInsolvent must be > 0 to reach insolvency");
        (int256 ab,,) = superToken_.realtimeBalanceOf(account, block.timestamp);
        int256 timeToZero = ab / netFlowRate;
        uint256 amountToWarp = timeToZero.toUint256() + liquidationPeriod + secondsInsolvent;
        vm.warp(block.timestamp + amountToWarp);
        assertFalse(superToken_.isAccountSolventNow(account), "_helperWarpToInsolvency: account is still solvent");
    }

    // ID generator helpers
    /// @notice Generates a flow ID for a sender-receiver
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @return id The flow ID for a sender-receiver
    function _generateFlowId(address sender, address receiver) private pure returns (bytes32 id) {
        return keccak256(abi.encode(sender, receiver));
    }

    /// @notice Generates a flowOperator ID for a sender-flowOperator
    /// @param sender The sender of the flow
    /// @param flowOperator The flowOperator of the flow
    /// @return id The flowOperator ID for a sender-flowOperator
    function _generateFlowOperatorId(address sender, address flowOperator) private pure returns (bytes32 id) {
        return keccak256(abi.encode("flowOperator", sender, flowOperator));
    }

    /// @notice Generates a publisher ID for a publisher-indexId
    /// @param publisher The publisher of the index
    /// @param indexId The indexId of the index
    /// @return iId The publisher ID for a publisher-indexId
    function _generatePublisherId(address publisher, uint32 indexId) private pure returns (bytes32 iId) {
        return keccak256(abi.encodePacked("publisher", publisher, indexId));
    }

    /// @notice Generates a subscription ID for a subscriber-publisherId
    /// @param subscriber The subscriber of the index
    /// @param iId The publisherId of the index
    /// @return sId The subscription ID for a subscriber-publisherId
    function _generateSubscriptionId(address subscriber, bytes32 iId) private pure returns (bytes32 sId) {
        return keccak256(abi.encodePacked("subscription", subscriber, iId));
    }

    // Write Helpers
    // Write Helpers - Testing State Changes
    /// @notice Takes a RTB snapshot of a SuperToken for an account
    /// @dev This must be called in the tests where a function is caled which settles balances in the protocol
    /// @param superToken_ The SuperToken to take the RTB snapshot for
    /// @param account The account to take the RTB snapshot for
    function _helperTakeBalanceSnapshot(ISuperToken superToken_, address account) internal {
        (int256 avb, uint256 deposit, uint256 owedDeposit, uint256 time) = superToken_.realtimeBalanceOfNow(account);
        RealtimeBalance memory balanceSnapshot = RealtimeBalance(avb, deposit, owedDeposit, time);
        _balanceSnapshots[superToken_][account] = balanceSnapshot;
    }

    /// @notice Adds inflows and outflows to the test state
    /// @dev This must be called whenever a flow is created
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
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

    /// @notice Removes inflows and outflows from the test state
    /// @dev This must be called whenever a flow is deleted
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
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
    /// @notice Creates a flow between a sender and receiver at a given flow rate
    /// @dev This helper assumes a valid flow rate with vm.assume and asserts that state has updated as expected.
    /// We assert:
    /// - The flow info is properly set
    /// - The account flow info has been updated as expected for sender and receiver
    /// - The balance of the sender and receiver has been updated as expected
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @param flowRate The desired flow rate
    function _helperCreateFlow(ISuperToken superToken_, address sender, address receiver, int96 flowRate) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (
            ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfoBefore
        ) = _helperGetAllFlowInfo(superToken_, sender, receiver);

        vm.startPrank(sender);
        superToken_.createFlow(receiver, flowRate);
        vm.stopPrank();

        _helperAddInflowsAndOutflowsToTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken_, sender);
        _helperTakeBalanceSnapshot(superToken_, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowData(superToken_, sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        _assertRealTimeBalances(superToken_);
    }

    /// @notice Updates a flow between a sender and receiver at a given flow rate
    /// @dev This helper assumes a valid flow rate with vm.assume and asserts that state has updated as expected.
    /// We assert:
    /// - The flow info is properly set
    /// - The account flow info has been updated as expected for sender and receiver
    /// - The balance of the sender and receiver has been updated as expected
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @param flowRate The desired flow rate
    function _helperUpdateFlow(ISuperToken superToken_, address sender, address receiver, int96 flowRate) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (
            ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfoBefore
        ) = _helperGetAllFlowInfo(superToken_, sender, receiver);

        vm.startPrank(sender);
        superToken_.updateFlow(receiver, flowRate);
        vm.stopPrank();

        _helperTakeBalanceSnapshot(superToken_, sender);
        _helperTakeBalanceSnapshot(superToken_, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowData(superToken_, sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        _assertRealTimeBalances(superToken_);
    }

    /// @notice Deletes a flow between a sender and receiver
    /// @dev This helper assumes a valid flow rate with vm.assume and asserts that state has updated as expected.
    /// We assert:
    /// - The flow info is properly set
    /// - The account flow info has been updated as expected for sender and receiver
    /// - The balance of the sender and receiver has been updated as expected
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    function _helperDeleteFlow(ISuperToken superToken_, address caller, address sender, address receiver) internal {
        (
            ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfoBefore
        ) = _helperGetAllFlowInfo(superToken_, sender, receiver);

        vm.startPrank(caller);
        superToken_.deleteFlow(sender, receiver);
        vm.stopPrank();

        _helperRemoveInflowsAndOutflowsFromTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken_, sender);
        _helperTakeBalanceSnapshot(superToken_, receiver);

        int96 flowRateDelta = -flowInfoBefore.flowRate;

        _assertFlowDataIsEmpty(superToken_, sender, receiver);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        _assertRealTimeBalances(superToken_);
    }

    /// @notice Creates an ACL flow by the opeartor between a sender and receiver at a given flow rate
    /// @dev This helper assumes a valid flow rate with vm.assume and asserts that state has updated as expected.
    /// We assert:
    /// - The flow info is properly set
    /// - The account flow info has been updated as expected for sender and receiver
    /// - The balance of the sender and receiver has been updated as expected
    /// - The flow rate allowance has been deducted accordingly
    /// @param operator The flow operator
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @param flowRate The desired flow rate
    function _helperCreateFlowFrom(
        ISuperToken superToken_,
        address operator,
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (
            ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfoBefore
        ) = _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(operator);
        superToken_.createFlowFrom(sender, receiver, flowRate);
        vm.stopPrank();

        _helperAddInflowsAndOutflowsToTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowData(superToken_, sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        _assertRealTimeBalances(superToken_);

        // TODO
        // Assert that flow rate allowance has been deducted accordingly
    }

    /// @notice Updates an ACL flow by the opeartor between a sender and receiver at a given flow rate
    /// @dev This helper assumes a valid flow rate with vm.assume and asserts that state has updated as expected.
    /// We assert:
    /// - The flow info is properly set
    /// - The account flow info has been updated as expected for sender and receiver
    /// - The balance of the sender and receiver has been updated as expected
    /// - The flow rate allowance has been deducted accordingly
    /// @param operator The flow operator
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    /// @param flowRate The desired flow rate
    function _helperUpdateFlowFrom(
        ISuperToken superToken_,
        address operator,
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        flowRate = _assumeValidFlowRate(flowRate);

        (
            ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfoBefore
        ) = _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(operator);
        superToken_.updateFlowFrom(sender, receiver, flowRate);
        vm.stopPrank();

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = flowRate - flowInfoBefore.flowRate;

        _assertFlowData(superToken_, sender, receiver, flowRate, block.timestamp, 0);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        _assertRealTimeBalances(superToken_);

        // TODO
        // Assert that flow rate allowance has been deducted accordingly (if flow rate is increased by delta amount)
    }

    /// @notice Deletes an ACL flow by the opeartor between a sender and receiver
    /// @dev This helper assumes a valid flow rate with vm.assume and asserts that state has updated as expected.
    /// We assert:
    /// - The flow info is properly set
    /// - The account flow info has been updated as expected for sender and receiver
    /// - The balance of the sender and receiver has been updated as expected
    /// - The flow rate allowance has been deducted accordingly
    /// @param operator The flow operator
    /// @param sender The sender of the flow
    /// @param receiver The receiver of the flow
    function _helperDeleteFlowFrom(ISuperToken superToken_, address operator, address sender, address receiver)
        internal
    {
        (
            ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory senderFlowInfoBefore,
            ConstantFlowAgreementV1.FlowData memory receiverFlowInfoBefore
        ) = _helperGetAllFlowInfo(superToken, sender, receiver);

        vm.startPrank(operator);
        superToken_.deleteFlowFrom(sender, receiver);
        vm.stopPrank();

        _helperRemoveInflowsAndOutflowsFromTestState(sender, receiver);

        _helperTakeBalanceSnapshot(superToken, sender);
        _helperTakeBalanceSnapshot(superToken, receiver);

        int96 flowRateDelta = -flowInfoBefore.flowRate;

        _assertFlowDataIsEmpty(superToken_, sender, receiver);
        _assertAccountFlowInfo(sender, flowRateDelta, senderFlowInfoBefore, true);
        _assertAccountFlowInfo(receiver, flowRateDelta, receiverFlowInfoBefore, false);

        _assertRealTimeBalances(superToken_);
    }

    // Write Helpers - InstantDistributionAgreementV1

    /// @notice Creates an index as a publisher with index id
    /// @dev We assert:
    ///     - the index was created and is empty
    /// We also add the publisher id to the list of index id's belonging to the publisher
    /// @param superToken_ The SuperToken to create the index for
    /// @param publisher The publisher of the index
    /// @param indexId The index id to create
    function _helperCreateIndex(ISuperToken superToken_, address publisher, uint32 indexId) internal {
        vm.startPrank(publisher);
        superToken_.createIndex(indexId);
        vm.stopPrank();

        _assertIndexData(superToken_, publisher, indexId, true, 0, 0, 0);

        _indexIDs[superToken_][publisher].add(_generatePublisherId(publisher, indexId));
    }

    /// @notice Updates the index value of an index which distributes tokens to subscribers
    /// @dev We assert:
    ///     - The index data has been updated as expected
    ///     - the publisher's balance and deposit has been updated as expected
    /// @param superToken_ The SuperToken to update the index value for
    /// @param publisher The publisher of the index
    /// @param indexId The indexId to update
    /// @param newIndexValue The new index value to update to
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

        _helperTakeBalanceSnapshot(superToken, publisher);

        _assertIndexData(
            superToken_, publisher, indexId, true, newIndexValue, totalUnitsApprovedBefore, totalUnitsPendingBefore
        );
        (int256 publisherAvbAfter, uint256 publisherDepositAfter,,) = superToken_.realtimeBalanceOfNow(publisher);
        assertEq(publisherAvbAfter, publisherAvbBefore - distributionAmount, "Update Index: Publisher AVB");
        assertEq(publisherDepositAfter, publisherDepositBefore + depositDelta, "Update Index: Publisher Deposit");

        // TODO we could actually save all the subscribers of an index and loop over them down the line
        // Assert that balance for subscriber has been updated (dependent on approval status)
    }

    /// @notice Distributes tokens to subscribers
    /// @dev We assert:
    ///     - The index data has been updated as expected
    ///     - the publisher's balance and deposit has been updated as expected
    /// @param superToken_ The SuperToken to update the index value for
    /// @param publisher The publisher of the index
    /// @param indexId The indexId to update
    /// @param amount The new index value to update to
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

        _helperTakeBalanceSnapshot(superToken, publisher);

        _assertIndexData(
            superToken_, publisher, indexId, true, newIndexValue, totalUnitsApprovedBefore, totalUnitsPendingBefore
        );
        (int256 publisherAvbAfter, uint256 publisherDepositAfter,,) = superToken_.realtimeBalanceOfNow(publisher);
        assertEq(publisherAvbAfter, publisherAvbBefore - distributionAmount, "Distribute: Publisher AVB");
        assertEq(publisherDepositAfter, publisherDepositBefore + depositDelta, "Distribute: Publisher Deposit");

        // TODO we could actually save all the subscribers of an index and loop over them down the line
        // Assert that balance for subscriber has been updated (dependent on approval status)
    }

    /// @notice Updates subscription units for a subscriber
    /// @dev We assert:
    ///     - The index data has been updated as expected
    ///     - The subscription data has been updated as expected
    /// @param params The params for IDA subscription function
    /// @param units The desired units
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

    /// @notice Approves a subscription
    /// @dev We assert:
    ///     - The index data has been updated as expected
    ///     - The subscription data has been updated as expected
    ///     - The subscriber's balance has been updated as expected
    ///     - The publisher's balance has been updated as expected
    /// @param params The params for IDA subscription function
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

        // Take Balance Snapshot
        {
            _helperTakeBalanceSnapshot(superToken, params.publisher);
            _helperTakeBalanceSnapshot(superToken, params.subscriber);
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

    /// @notice Revokes a subscription
    /// @dev We assert:
    ///     - The index data has been updated as expected
    ///     - The subscription data has been updated as expected
    ///     - The subscriber's balance has been updated as expected
    ///     - The publisher's balance has been updated as expected
    /// @param params The params for IDA subscription function
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

        // Take Balance Snapshot
        {
            _helperTakeBalanceSnapshot(superToken, params.publisher);
            _helperTakeBalanceSnapshot(superToken, params.subscriber);
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

    /// @notice Deletes a subscription
    /// @dev We assert:
    ///     - The index data has been updated as expected
    ///     - The subscription data has been updated as expected
    ///     - The subscriber's balance has been updated as expected
    ///     - The publisher's balance has been updated as expected
    /// @param params The params for IDA subscription function
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

        // Take Balance Snapshot
        {
            _helperTakeBalanceSnapshot(superToken, params.publisher);
            _helperTakeBalanceSnapshot(superToken, params.subscriber);
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

    /// @notice Executes a claim for a subscription
    /// @dev We assert:
    ///     - The subscriber's balance has been updated as expected
    ///     - The publisher's balance has been updated as expected
    /// @param superToken_ The SuperToken to claim
    /// @param caller The caller of the claim function
    /// @param publisher The publisher of the subscription
    /// @param indexId The index ID of the index
    /// @param subscriber The subscriber of the subscription
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

        // Take Balance Snapshot
        {
            _helperTakeBalanceSnapshot(superToken, publisher);
            _helperTakeBalanceSnapshot(superToken, subscriber);
        }

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
    function _assertFlowData(
        ISuperToken superToken_,
        address sender,
        address receiver,
        int96 expectedFlowRate,
        uint256 expectedLastUpdated,
        uint256 expectedOwedDeposit
    ) internal {
        (uint256 lastUpdated, int96 flowRate, uint256 deposit, uint256 owedDeposit) =
            superToken_.getFlowInfo(sender, receiver);

        uint256 expectedDeposit = superToken_.getBufferAmountByFlowRate(expectedFlowRate);

        assertEq(flowRate, expectedFlowRate, "FlowData: flow rate");
        assertEq(lastUpdated, expectedLastUpdated, "FlowData: last updated");
        assertEq(deposit, expectedDeposit, "FlowData: deposit");
        assertEq(owedDeposit, expectedOwedDeposit, "FlowData: owed deposit");
    }

    /// @dev Asserts that a single flow has been removed on deletion
    function _assertFlowDataIsEmpty(ISuperToken superToken_, address sender, address receiver) internal {
        _assertFlowData(superToken_, sender, receiver, 0, 0, 0);
    }

    function _assertFlowOperatorData(
        ISuperToken superToken_,
        address sender,
        address flowOperator,
        int96 expectedFlowRateAllowance,
        uint8 expectedPermissionsBitmask
    ) internal {
        (bool canCreate, bool canUpdate, bool canDelete, int96 allowance) =
            superToken_.getFlowPermissions(sender, flowOperator);

        bool expectedAllowCreate = expectedPermissionsBitmask & 1 == 1 ? true : false;
        bool expectedAllowUpdate = expectedPermissionsBitmask >> 1 & 1 == 1 ? true : false;
        bool expectedAllowDelete = expectedPermissionsBitmask >> 2 & 1 == 1 ? true : false;

        assertEq(canCreate, expectedAllowCreate, "FlowOperatorData: create permissions");
        assertEq(canUpdate, expectedAllowUpdate, "FlowOperatorData: update permissions");
        assertEq(canDelete, expectedAllowDelete, "FlowOperatorData: delete permissions");
        assertEq(allowance, expectedFlowRateAllowance, "FlowOperatorData: flow rate allowance");
    }

    function _assertFlowOperatorDataIsEmpty(ISuperToken superToken_, address sender, address flowOperator)
        internal
    {
        _assertFlowOperatorData(superToken_, sender, flowOperator, 0, 0);
    }

    /// @dev Asserts that account flow info has been updated as expected
    /// @param account The account to check
    /// @param flowRateDelta The delta of the flow rate
    /// @param flowInfoBefore The flow info before the update
    /// @param isSender Whether the account is the sender
    function _assertAccountFlowInfo(
        address account,
        int96 flowRateDelta,
        ConstantFlowAgreementV1.FlowData memory flowInfoBefore,
        bool isSender
    ) internal {
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

    /// @dev Asserts that the index data has been updated as expected
    /// @param superToken_ The SuperToken to check
    /// @param publisher The publisher of the index
    /// @param indexId The index ID of the index
    /// @param expectedExist Whether the index should exist
    /// @param expectedIndexValue The expected index value
    /// @param expectedTotalUnitsApproved The expected total units approved
    /// @param expectedTotalUnitsPending The expected total units pending
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

    /// @dev Asserts that the subscription data has been updated as expected
    /// @param superToken_ The SuperToken to check
    /// @param subscriptionId The subscription ID of the subscription
    /// @param expectedApproved Whether the subscription should be approved
    /// @param expectedUnits The expected units
    /// @param expectedPending The expected pending
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

    /// @notice Asserts that the real time balances for all active test accounts are expected
    /// @dev We also take a balance snapshot after each assertion
    /// @param superToken_ The SuperToken to check
    function _assertRealTimeBalances(ISuperToken superToken_) internal {
        for (uint i; i < N_TESTERS; ++i) {
            address account = TEST_ACCOUNTS[i];
            RealtimeBalance memory balanceSnapshot = _balanceSnapshots[superToken_][account];
            (int256 avb, uint256 deposit, uint256 owedDeposit, uint256 currentTime) =
                superToken_.realtimeBalanceOfNow(account);

            int96 netFlowRate = superToken_.getNetFlowRate(account);
            int256 amountFlowedSinceSnapshot = (currentTime - balanceSnapshot.timestamp).toInt256() * netFlowRate;
            int256 expectedAvb = balanceSnapshot.availableBalance + amountFlowedSinceSnapshot;

            assertEq(balanceSnapshot.deposit, deposit, "Real Time Balances: deposit");
            assertEq(balanceSnapshot.owedDeposit, owedDeposit, "Real Time Balances: owed deposit");
            assertEq(avb, expectedAvb, "Real Time Balances: available balance");

            _helperTakeBalanceSnapshot(superToken_, account);
        }
    }
}
