// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Test } from "forge-std/Test.sol";

import {
    SuperfluidFrameworkDeployer,
    TestResolver,
    SuperfluidLoader
} from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { ERC1820RegistryCompiled } from "../../contracts/libs/ERC1820RegistryCompiled.sol";
import { SuperTokenDeployer } from "../../contracts/utils/SuperTokenDeployer.sol";
import { CFAv1Library, IDAv1Library, Superfluid } from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { UUPSProxy } from "../../contracts/upgradability/UUPSProxy.sol";
import { SuperTokenV1Library } from "../../contracts/apps/SuperTokenV1Library.sol";
import { TestToken, SuperToken } from "../../contracts/utils/SuperTokenDeployer.sol";

contract FoundrySuperfluidTester is Test {
    using SuperTokenV1Library for SuperToken;

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperTokenDeployer internal immutable superTokenDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;

    uint internal constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint internal constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;

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

    TestToken internal token;
    SuperToken internal superToken;

    uint256 private _expectedTotalSupply;

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
    }

    function setUp() public virtual {
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

    /*//////////////////////////////////////////////////////////////////////////
                                Invariant Definitions
    //////////////////////////////////////////////////////////////////////////*/
    /// @notice Superfluid Global Invariants
    /// @dev Superfluid Global Invariants:
    /// - Liquidity Sum Invariant
    /// - Net Flow Rate Sum Invariant
    /// @return bool Superfluid Global Invariants holds true
    function _definitionGlobalInvariants() internal view returns (bool) {
        return _definitionLiquiditySumInvariant() && _definitionNetFlowRateSumInvariant();
    }

    /// @notice Liquidity Sum Invariant definition
    /// @dev Liquidity Sum Invariant: Expected Total Supply === Liquidity Sum
    /// Liquidity Sum = sum of available balance, deposit and owed deposit for all users
    /// @return bool Liquidity Sum Invariant holds true
    function _definitionLiquiditySumInvariant() internal view returns (bool) {
        int256 liquiditySum;

        for (uint256 i = 0; i < TEST_ACCOUNTS.length; ++i) {
            (int256 availableBalance, uint256 deposit, uint256 owedDeposit,) =
                superToken.realtimeBalanceOfNow(address(TEST_ACCOUNTS[i]));

            liquiditySum += availableBalance + int256(deposit) - int256(owedDeposit);
        }

        return int256(_expectedTotalSupply) == liquiditySum;
    }

    /// @notice Net Flow Rate Sum Invariant definition
    /// @dev Net Flow Rate Sum Invariant: Sum of all net flow rates === 0
    /// @return bool Net Flow Rate Sum Invariant holds true
    function _definitionNetFlowRateSumInvariant() internal view returns (bool) {
        int96 netFlowRateSum;
        for (uint256 i = 0; i < TEST_ACCOUNTS.length; ++i) {
            netFlowRateSum += sf.cfa.getNetFlow(superToken, address(TEST_ACCOUNTS[i]));
        }
        return netFlowRateSum == 0;
    }

    function _assertGlobalInvariants() internal {
        assertTrue(_definitionGlobalInvariants());
    }

    function _assertLiquiditySumInvariant() internal {
        assertTrue(_definitionLiquiditySumInvariant());
    }

    function _assertNetFlowRateSumInvariant() internal {
        assertTrue(_definitionNetFlowRateSumInvariant());
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function _assertModifyFlowAndNetFlowIsExpected(
        address flowSender,
        address flowReceiver,
        int96 flowRateDelta,
        int96 senderNetFlowBefore,
        int96 receiverNetFlowBefore
    ) internal {
        int96 senderNetFlowAfter = superToken.getNetFlowRate(flowSender);
        int96 receiverNetFlowAfter = superToken.getNetFlowRate(flowReceiver);

        assertEq(senderNetFlowAfter, senderNetFlowBefore - flowRateDelta, "sender net flow after");
        assertEq(receiverNetFlowAfter, receiverNetFlowBefore + flowRateDelta, "receiver net flow after");
    }

    function _assertModifyFlowAndFlowInfoIsExpected(
        address flowSender,
        address flowReceiver,
        int96 expectedFlowRate,
        uint256 expectedLastUpdated,
        uint256 expectedOwedDeposit
    ) internal {
        (uint256 lastUpdated, int96 _flowRate, uint256 deposit, uint256 owedDeposit) =
            superToken.getFlowInfo(flowSender, flowReceiver);

        uint256 expectedDeposit = superToken.getBufferAmountByFlowRate(expectedFlowRate);

        assertEq(_flowRate, expectedFlowRate, "flow rate");
        assertEq(lastUpdated, expectedLastUpdated, "last updated");
        assertEq(deposit, expectedDeposit, "deposit");
        assertEq(owedDeposit, expectedOwedDeposit, "owed deposit");
    }

    function _assertFlowInfoIsEmpty(address flowSender, address flowReceiver) internal {
        _assertModifyFlowAndFlowInfoIsExpected(flowSender, flowReceiver, 0, 0, 0);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assume Helpers
    //////////////////////////////////////////////////////////////////////////*/
    /// @notice Assume a valid flow rate
    /// @dev Flow rate must be greater than 0 and less than or equal to int32.max
    function _assumeValidFlowRate(uint32 a) internal pure returns (int96 flowRate) {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        flowRate = int96(int32(a));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/

    function _helperCreateFlowAndAssertGlobalInvariants(address flowSender, address flowReceiver, uint32 _flowRate)
        internal
        returns (int96 absoluteFlowRate)
    {
        int96 flowRate = _assumeValidFlowRate(_flowRate);

        absoluteFlowRate = flowRate;

        int96 senderNetFlowRateBefore = superToken.getNetFlowRate(flowSender);
        int96 receiverNetFlowRateBefore = superToken.getNetFlowRate(flowReceiver);

        vm.startPrank(flowSender);
        superToken.createFlow(flowReceiver, flowRate);
        vm.stopPrank();

        _assertModifyFlowAndNetFlowIsExpected(
            flowSender, flowReceiver, flowRate, senderNetFlowRateBefore, receiverNetFlowRateBefore
        );

        _assertModifyFlowAndFlowInfoIsExpected(flowSender, flowReceiver, flowRate, block.timestamp, 0);

        _assertGlobalInvariants();
    }
}
