// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    Superfluid,
    ConstantFlowAgreementV1,
    InstantDistributionAgreementV1,
    SuperfluidFrameworkDeployer,
    CFAv1Library,
    IDAv1Library,
    TestResolver
} from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import {
    TestToken,
    SuperToken
} from "../../contracts/utils/SuperTokenDeployer.sol";
import { DeployerBaseTest } from "./DeployerBase.t.sol";

contract FoundrySuperfluidTester is DeployerBaseTest {
    uint internal constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint internal constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address internal constant alice = address(0x421);
    address internal constant bob = address(0x422);
    address internal constant carol = address(0x423);
    address internal constant dan = address(0x424);
    address internal constant eve = address(0x425);
    address internal constant frank = address(0x426);
    address internal constant grace = address(0x427);
    address internal constant heidi = address(0x428);
    address internal constant ivan = address(0x429);
    address[] internal TEST_ACCOUNTS = [admin,alice,bob,carol,dan,eve,frank,grace,heidi,ivan];

    uint internal immutable N_TESTERS;

    TestToken internal token;
    SuperToken internal superToken;

    uint256 private _expectedTotalSupply;

    constructor(uint8 nTesters) {
        require(nTesters <= TEST_ACCOUNTS.length, "too many testers");
        N_TESTERS = nTesters;
    }

    function setUp() public virtual override {
        (token, superToken) = superTokenDeployer.deployWrapperSuperToken(
            "FTT",
            "FTT",
            18,
            type(uint256).max
        );

        for (uint i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);

            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assume Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
        uint32 _flowRate
    ) public {
        vm.assume(_flowRate > 0);
        vm.assume(_flowRate <= uint32(type(int32).max));
        int96 flowRate = int96(int32(_flowRate));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Invariant Definitions
    //////////////////////////////////////////////////////////////////////////*/
    /// @notice Superfluid Global Invariants
    /// @dev Superfluid Global Invariants:
    /// - Liquidity Sum Invariant
    /// - Net Flow Rate Sum Invariant
    /// @return bool Superfluid Global Invariants hold true
    function definition_Global_Invariants() public view returns (bool) {
        return
            definition_Liquidity_Sum_Invariant() &&
            definition_Net_Flow_Rate_Sum_Invariant();
    }

    /// @notice Liquidity Sum Invariant definition
    /// @dev Liquidity Sum Invariant: Expected Total Supply === Liquidity Sum
    /// Liquidity Sum = sum of available balance, deposit and owed deposit for all users
    /// @return bool Liquidity Sum Invariant holds true
    function definition_Liquidity_Sum_Invariant() public view returns (bool) {
        int256 liquiditySum;

        for (uint i = 0; i < TEST_ACCOUNTS.length; ++i) {
            (
                int256 availableBalance,
                uint256 deposit,
                uint256 owedDeposit,

            ) = superToken.realtimeBalanceOfNow(address(TEST_ACCOUNTS[i]));

            liquiditySum +=
                availableBalance +
                int256(deposit) -
                int256(owedDeposit);
        }
        return int256(_expectedTotalSupply) == liquiditySum;
    }

    /// @notice Net Flow Rate Sum Invariant definition
    /// @dev Net Flow Rate Sum Invariant: Sum of all net flow rates === 0
    /// @return bool Net Flow Rate Sum Invariant holds true
    function definition_Net_Flow_Rate_Sum_Invariant()
        public
        view
        returns (bool)
    {
        int96 netFlowRateSum;
        for (uint i = 0; i < TEST_ACCOUNTS.length; ++i) {
            netFlowRateSum += sf.cfa.getNetFlow(
                superToken,
                address(TEST_ACCOUNTS[i])
            );
        }
        return netFlowRateSum == 0;
    }

    function assert_Global_Invariants() public {
        assert_Liquidity_Sum_Invariant();
        assert_Net_Flow_Rate_Sum_Invariant();
    }

    function assert_Liquidity_Sum_Invariant() public {
        assertTrue(definition_Liquidity_Sum_Invariant());
    }

    function assert_Net_Flow_Rate_Sum_Invariant() public {
        assertTrue(definition_Net_Flow_Rate_Sum_Invariant());
    }
}
