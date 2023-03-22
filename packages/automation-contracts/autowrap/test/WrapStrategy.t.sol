// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { SuperfluidTester, CFAv1Library } from "../test/SuperfluidTester.sol";
import { Manager } from "./../contracts/Manager.sol";
import { WrapStrategy } from "./../contracts/strategies/WrapStrategy.sol";
import { IStrategy } from "./../contracts/interfaces/IStrategy.sol";

/// @title ManagerTests
contract WrapStrategyTests is SuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

    event Wrap(
        address indexed user,
        address indexed superToken,
        uint256 superTokenAmount
    );
    event ManagerChanged(
        address indexed oldManager,
        address indexed Manager
    );
    event EmergencyWithdrawInitiated(
        address indexed receiver,
        address indexed token,
        uint256 amount
    );

    /// SETUP AND HELPERS
    constructor() SuperfluidTester(3) {}

    function setUp() public virtual {
        (token, superToken) = superTokenDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        for (uint32 i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }

        nativeSuperToken = superTokenDeployer.deployNativeAssetSuperToken("xFTT", "xFTT");
        manager = new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        wrapStrategy = new WrapStrategy(address(manager));
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

    function testFailDeploymentWithoutManager() public {
        new WrapStrategy(address(0));
    }

    function testDeploymentCheckData() public {
        WrapStrategy wrap = new WrapStrategy(address(manager));
        assertEq(address(manager), wrap.manager(), "manager address not equal");
    }

    // Manager operations

    function testCannotChangeManagerContractIfNotOwner() public {
        Manager newManager =  new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        vm.prank(admin);
        vm.expectRevert(bytes("Ownable: caller is not the owner"));
        wrapStrategy.changeManager(address(newManager));
    }

    function testCannotChangeManagerZeroAddress() public {
        vm.expectRevert(IStrategy.ZeroAddress.selector);
        wrapStrategy.changeManager(address(0));
    }

    function testChangeManagerContract() public {
        Manager newManager =  new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        vm.expectEmit(true, true, true, true);
        emit ManagerChanged(address(manager), address(newManager));
        wrapStrategy.changeManager(address(newManager));
    }

    // SuperToken

    function testSupportedSuperToken() public {
        assertTrue(
            wrapStrategy.isSupportedSuperToken(superToken),
            "SuperToken should be supported"
        );
    }

    function testCannotNonSupportedSuperToken() public {
        assertTrue(
            !wrapStrategy.isSupportedSuperToken(nativeSuperToken),
            "Native SuperToken shouldn't be supported"
        );
    }

    // Wrap Strategy Operations

    function testCannotPerformWrapIfNotManager() public {
        vm.prank(alice);
        vm.expectRevert(abi.encodeWithSelector(IStrategy.UnauthorizedCaller.selector, alice, address(manager)));
        wrapStrategy.wrap(bob, superToken, 1);
    }

}
