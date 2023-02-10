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
import { UUPSProxy } from "../../contracts/upgradability/UUPSProxy.sol";
import {
    SuperTokenV1Library
} from "../../contracts/apps/SuperTokenV1Library.sol";
import {
    TestToken,
    SuperToken
} from "../../contracts/utils/SuperTokenDeployer.sol";
import { DeployerBaseTest } from "./DeployerBase.t.sol";
import { ConstantOutflowNFTMock, ConstantInflowNFTMock } from "./superfluid/CFAv1NFTMock.t.sol";

contract FoundrySuperfluidTester is DeployerBaseTest {
    using SuperTokenV1Library for SuperToken;

    string constant OUTFLOW_NFT_NAME_TEMPLATE = " Constant Outflow NFT";
    string constant OUTFLOW_NFT_SYMBOL_TEMPLATE = "COF";
    string constant INFLOW_NFT_NAME_TEMPLATE = " Constant Inflow NFT";
    string constant INFLOW_NFT_SYMBOL_TEMPLATE = "CIF";


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
    address internal constant defaultRewardAddress = address(69);
    address[] internal TEST_ACCOUNTS = [admin,alice,bob,carol,dan,eve,frank,grace,heidi,ivan,defaultRewardAddress];

    uint internal immutable N_TESTERS;

    TestToken internal token;
    SuperToken internal superToken;

    ConstantOutflowNFTMock public constantOutflowNFTLogic;
    ConstantInflowNFTMock public constantInflowNFTLogic;

    ConstantOutflowNFTMock public constantOutflowNFTProxy;
    ConstantInflowNFTMock public constantInflowNFTProxy;

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

        // deploy NFT contracts and set in state
        (
            constantOutflowNFTLogic,
            constantOutflowNFTProxy,
            constantInflowNFTLogic,
            constantInflowNFTProxy
        ) = helper_Deploy_NFT_Contracts_And_Set_Address_In_Super_Token();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Invariant Definitions
    //////////////////////////////////////////////////////////////////////////*/
    /// @notice Superfluid Global Invariants
    /// @dev Superfluid Global Invariants:
    /// - Liquidity Sum Invariant
    /// - Net Flow Rate Sum Invariant
    /// @return bool Superfluid Global Invariants holds true
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
        assertTrue(definition_Global_Invariants());
    }

    function assert_Liquidity_Sum_Invariant() public {
        assertTrue(definition_Liquidity_Sum_Invariant());
    }

    function assert_Net_Flow_Rate_Sum_Invariant() public {
        assertTrue(definition_Net_Flow_Rate_Sum_Invariant());
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function assert_Modify_Flow_And_Net_Flow_Is_Expected(
        address flowSender,
        address flowReceiver,
        int96 flowRateDelta,
        int96 senderNetFlowBefore,
        int96 receiverNetFlowBefore
    ) internal {
        int96 senderFlowAfter = superToken.getNetFlowRate(flowSender);
        int96 receiverFlowAfter = superToken.getNetFlowRate(flowReceiver);

        assertEq(
            senderFlowAfter,
            senderNetFlowBefore - flowRateDelta,
            "sender net flow after"
        );
        assertEq(
            receiverFlowAfter,
            receiverNetFlowBefore + flowRateDelta,
            "receiver net flow after"
        );
    }

    function assert_Modify_Flow_And_Flow_Info_Is_Expected(
        address flowSender,
        address flowReceiver,
        int96 expectedFlowRate,
        uint256 expectedLastUpdated,
        uint256 expectedOwedDeposit
    ) internal {
        (
            uint256 lastUpdated,
            int96 _flowRate,
            uint256 deposit,
            uint256 owedDeposit
        ) = superToken.getFlowInfo(flowSender, flowReceiver);

        uint256 expectedDeposit = superToken.getBufferAmountByFlowRate(
            expectedFlowRate
        );

        assertEq(_flowRate, expectedFlowRate, "flow rate");
        assertEq(lastUpdated, expectedLastUpdated, "last updated");
        assertEq(deposit, expectedDeposit, "deposit");
        assertEq(owedDeposit, expectedOwedDeposit, "owed deposit");
    }

    function assert_Flow_Info_Is_Empty(
        address flowSender,
        address flowReceiver
    ) internal {
        assert_Modify_Flow_And_Flow_Info_Is_Expected(
            flowSender,
            flowReceiver,
            0,
            0,
            0
        );
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assume Helpers
    //////////////////////////////////////////////////////////////////////////*/
    /// @notice Assume a valid flow rate
    /// @dev Flow rate must be greater than 0 and less than or equal to int32.max
    function assume_Valid_Flow_Rate(
        uint32 a
    ) internal returns (int96 flowRate) {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        flowRate = int96(int32(a));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/

    function helper_Deploy_Constant_Outflow_NFT()
        public
        returns (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy
        )
    {
        _constantOutflowNFTLogic = new ConstantOutflowNFTMock();
        UUPSProxy proxy = new UUPSProxy();
        proxy.initializeProxy(address(_constantOutflowNFTLogic));

        _constantOutflowNFTProxy = ConstantOutflowNFTMock(address(proxy));
        string memory symbol = superToken.symbol();
        _constantOutflowNFTProxy.initialize(
            superToken,
            string.concat(symbol, OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function helper_Deploy_Constant_Inflow_NFT()
        public
        returns (
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        )
    {
        _constantInflowNFTLogic = new ConstantInflowNFTMock();
        UUPSProxy proxy = new UUPSProxy();
        proxy.initializeProxy(address(_constantInflowNFTLogic));

        _constantInflowNFTProxy = ConstantInflowNFTMock(address(proxy));
        string memory symbol = superToken.symbol();
        _constantInflowNFTProxy.initialize(
            superToken,
            string.concat(symbol, INFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function helper_Deploy_NFT_Contracts_And_Set_Address_In_Super_Token()
        public
        returns (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy,
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        )
    {
        (
            _constantOutflowNFTLogic,
            _constantOutflowNFTProxy
        ) = helper_Deploy_Constant_Outflow_NFT();
        (
            _constantInflowNFTLogic,
            _constantInflowNFTProxy
        ) = helper_Deploy_Constant_Inflow_NFT();

        vm.prank(sf.governance.owner());
        superToken.setNFTProxyContracts(
            address(_constantOutflowNFTProxy),
            address(_constantInflowNFTProxy),
            address(0),
            address(0)
        );
    }

    function helper_Create_Flow_And_Assert_Global_Invariants(
        address flowSender,
        address flowReceiver,
        uint32 _flowRate
    ) internal returns (int96 absoluteFlowRate) {
        int96 flowRate = assume_Valid_Flow_Rate(_flowRate);

        absoluteFlowRate = flowRate;

        int96 senderNetFlowRateBefore = superToken.getNetFlowRate(flowSender);
        int96 receiverNetFlowRateBefore = superToken.getNetFlowRate(
            flowReceiver
        );

        vm.startPrank(flowSender);
        superToken.createFlow(flowReceiver, flowRate);
        vm.stopPrank();

        assert_Modify_Flow_And_Net_Flow_Is_Expected(
            flowSender,
            flowReceiver,
            flowRate,
            senderNetFlowRateBefore,
            receiverNetFlowRateBefore
        );

        assert_Modify_Flow_And_Flow_Info_Is_Expected(
            flowSender,
            flowReceiver,
            flowRate,
            block.timestamp,
            0
        );

        assert_Global_Invariants();
    }
}
