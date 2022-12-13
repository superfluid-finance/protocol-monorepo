// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { SuperfluidFrameworkDeployer, SuperfluidTester, Superfluid, ConstantFlowAgreementV1, CFAv1Library } from "../test/SuperfluidTester.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ERC1820RegistryCompiled } from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";
import { Manager } from "./../contracts/Manager.sol";
import { WrapStrategy } from "./../contracts/strategies/WrapStrategy.sol";
import { IManager } from "./../contracts/interfaces/IManager.sol";


/// @title ManagerTests
contract ManagerTests is SuperfluidTester {

    event WrapScheduleCreated(
        bytes32 indexed id,
        address indexed user,
        address indexed superToken,
        address strategy,
        address liquidityToken,
        uint256 expiry,
        uint256 lowerLimit,
        uint256 upperLimit
    );
    event WrapScheduleDeleted(
        bytes32 indexed id,
        address indexed user,
        address indexed superToken,
        address strategy,
        address liquidityToken
    );
    event WrapExecuted(bytes32 indexed id, uint256 wrapAmount);
    event AddedApprovedStrategy(address indexed strategy);
    event RemovedApprovedStrategy(address indexed strategy);
    event LimitsChanged(uint64 lowerLimit, uint64 upperLimit);

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;
    Superfluid host;
    ConstantFlowAgreementV1 cfa;
    uint256 private _expectedTotalSupply = 0;
    Manager public manager;
    WrapStrategy public wrapStrategy;
    ISuperToken nativeSuperToken;

    /// @dev This is required by solidity for using the CFAv1Library in the tester
    using CFAv1Library for CFAv1Library.InitData;

    /// @dev Constants for Testing

    uint64 constant MIN_LOWER = 2;
    uint64 constant MIN_UPPER = 7;
    uint64 constant EXPIRY = type(uint64).max;

    constructor() SuperfluidTester(3) {
        vm.startPrank(admin);
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);
        sfDeployer = new SuperfluidFrameworkDeployer();
        sf = sfDeployer.getFramework();
        host = sf.host;
        cfa = sf.cfa;
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                    )
                )
            )
        );
        manager = new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        wrapStrategy = new WrapStrategy(address(manager));
        vm.stopPrank();
    }

    /// SETUP AND HELPERS

    function setUp() public virtual {
        (token, superToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        for (uint32 i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }

            nativeSuperToken = sfDeployer.deployNativeAssetSuperToken("xFTT", "xFTT");
    }

    function getWrapIndex(
        address user,
        address superToken,
        address liquidityToken
    ) public pure returns (bytes32) {
        return keccak256(abi.encode(user, superToken, liquidityToken));
    }

    function startStream(address sender, address receiver, int96 flowRate) public {
        vm.startPrank(sender);
        cfaV1.createFlow(receiver, superToken, flowRate);
        vm.stopPrank();
    }

    function stopStream(address sender, address receiver) public {
        vm.startPrank(sender);
        cfaV1.deleteFlow(sender, receiver, superToken);
        vm.stopPrank();
    }


    /// TESTS

    function testFailDeploymentWithoutCFA() public {
        new Manager(address(0), 1, 2);
    }

    function testFailDeploymentWrongLimits() public {
        new Manager(address(cfa), 2, 1);
    }

    function testDeploymentCheckData() public {
        assertEq(address(manager.cfaV1()), address(cfa), "manager.cfaV1 not equal");
        assertEq(manager.owner(), admin, "manager.owner not equal");
        assertEq(manager.minLower(), MIN_LOWER, "manager.minLower not equal");
        assertEq(manager.minUpper(), MIN_UPPER, "manager.minUpper not equal");
    }

    // Change Limits
    function testChangeLimits() public {
        uint64 newMinLower = 10;
        uint64 newMinUpper = 20;

        // owner can set new limits
        vm.expectEmit(true, true, true, true);
        emit LimitsChanged(newMinLower, newMinUpper);
        vm.prank(admin);
        manager.setLimits(newMinLower, newMinUpper);
        // non owner can't set new limits
        vm.expectRevert(bytes("Ownable: caller is not the owner"));
        manager.setLimits(newMinLower, newMinUpper);
    }

    // Strategies
    function testAddingNewStrategy() public {
        vm.startPrank(admin);
        // strategy can't be zero address
        vm.expectRevert(abi.encodeWithSelector(IManager.InvalidStrategy.selector, address(0)));
        manager.addApprovedStrategy(address(0));
        // owner can add new strategy
        vm.expectEmit(true, true, true, true);
        emit AddedApprovedStrategy(address(wrapStrategy));
        manager.addApprovedStrategy(address(wrapStrategy));
        vm.stopPrank();
        // non owner can't add new strategy
        vm.expectRevert(bytes("Ownable: caller is not the owner"));
        manager.addApprovedStrategy(address(wrapStrategy));
        bool isStrategyApproved = manager.approvedStrategies(address(wrapStrategy));
        assertTrue(isStrategyApproved, "strategy should be register");
    }

    function testRemovingNewStrategy() public {
        vm.prank(admin);
        //add strategy to be removed
        manager.addApprovedStrategy(address(wrapStrategy));
        // non owner can't add new strategy
        vm.expectRevert(bytes("Ownable: caller is not the owner"));
        manager.removeApprovedStrategy(address(wrapStrategy));
        vm.startPrank(admin);
        vm.expectEmit(true, true, true, true);
        emit RemovedApprovedStrategy(address(wrapStrategy));
        manager.removeApprovedStrategy(address(wrapStrategy));
        vm.stopPrank();
        bool isStrategyApproved = manager.approvedStrategies(address(wrapStrategy));
        assertTrue(!isStrategyApproved, "strategy should be removed");
    }

    // Register Top Ups
    function testCannotCreateWrapWrongData() public {
        // revert with superToken = 0
        vm.expectRevert(IManager.ZeroAddress.selector);
        manager.createWrapSchedule(
            address(0),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );

        // revert with _strategy = 0
        vm.expectRevert(IManager.ZeroAddress.selector);
        manager.createWrapSchedule(
            address(superToken),
            address(0),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );

        // revert with liquidityToken = 0
        vm.expectRevert(IManager.ZeroAddress.selector);
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(0),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );

        // revert with expiry < now
        vm.warp(2);
        vm.expectRevert(abi.encodeWithSelector(IManager.InvalidExpirationTime.selector, 1, block.timestamp));
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(token),
            1,
            MIN_LOWER,
            MIN_UPPER
        );

        // revert with MIN_LOWER < minLower
        vm.expectRevert(abi.encodeWithSelector(IManager.InsufficientLimits.selector, 1, MIN_LOWER));
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            1,
            MIN_UPPER
        );

        // revert with MIN_UPPER < minUpper
        vm.expectRevert(abi.encodeWithSelector(IManager.InsufficientLimits.selector, 1, MIN_UPPER));
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            1
        );

        // revert if strategy invalid
        vm.expectRevert(abi.encodeWithSelector(IManager.InvalidStrategy.selector, address(wrapStrategy)));
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );
        vm.startPrank(admin);
        manager.addApprovedStrategy(address(wrapStrategy));
        vm.stopPrank();

        // revert if superToken unsupported. eg - Native Super Token
        vm.expectRevert(abi.encodeWithSelector(IManager.UnsupportedSuperToken.selector, address(nativeSuperToken)));
        manager.createWrapSchedule(
            address(nativeSuperToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );
    }

    function testCreateWrap() public {
        bytes32 index = getWrapIndex(address(alice), address(superToken), address(token));
        vm.expectEmit(true, true, true, true);
        emit WrapScheduleCreated(
            index,
            address(alice),
            address(superToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );
        vm.prank(admin);
        manager.addApprovedStrategy(address(wrapStrategy));
        vm.prank(alice);
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );

        IManager.WrapSchedule memory wrap = manager.getWrapSchedule(address(alice), address(superToken), address(token));
        assertEq(wrap.user, address(alice), "wrap.user not equal");
        assertEq(address(wrap.superToken), address(superToken), "wrap.superToken not equal");
        assertEq(address(wrap.strategy), address(wrapStrategy), "wrap.strategy not equal");
        assertEq(wrap.liquidityToken, address(token), "wrap.liquidityToken not equal");
        assertEq(wrap.expiry, EXPIRY, "wrap.expiry not equal");
        assertEq(wrap.lowerLimit, MIN_LOWER, "wrap.lowerLimit not equal");
        assertEq(wrap.upperLimit, MIN_UPPER, "wrap.upperLimit not equal");
    }

}
