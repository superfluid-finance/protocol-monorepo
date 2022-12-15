// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { SuperfluidFrameworkDeployer, SuperfluidTester, Superfluid, ConstantFlowAgreementV1, CFAv1Library } from "../test/SuperfluidTester.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ERC1820RegistryCompiled } from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";
import { Manager } from "./../contracts/Manager.sol";
import { WrapStrategy } from "./../contracts/strategies/WrapStrategy.sol";
import { IStrategy } from "./../contracts/interfaces/IStrategy.sol";
import { IManager } from "./../contracts/interfaces/IManager.sol";

import {console} from "forge-std/console.sol";

/// @title ManagerTests
contract WrapTests is SuperfluidTester {

    event WrapExecuted(bytes32 indexed id, uint256 WrapAmount);

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

    uint64 constant MIN_LOWER = 2 days;
    uint64 constant MIN_UPPER = 7 days;
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

        manager = new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        wrapStrategy = new WrapStrategy(address(manager));
        manager.addApprovedStrategy(address(wrapStrategy));
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
        (,int96 flow,,) = cfaV1.cfa.getFlow(superToken, alice, bob);
        assertEq(flowRate, flow, "startStream Flow rate are not the same");
        vm.stopPrank();
    }

    function stopStream(address sender, address receiver) public {
        vm.startPrank(sender);
        cfaV1.deleteFlow(sender, receiver, superToken);
        vm.stopPrank();
    }

    function setDefaultWrap(address sender) public {
        vm.startPrank(sender);
        token.approve(address(wrapStrategy), INIT_SUPER_TOKEN_BALANCE);
        manager.createWrapSchedule(
            address(superToken),
            address(wrapStrategy),
            address(token),
            EXPIRY,
            MIN_LOWER,
            MIN_UPPER
        );
        vm.stopPrank();
    }


    /// TESTS

    function testExecuteWrapCheckWrappedAmount() public {
        int96 flowRate = 10000;
        uint256 wrappedAmount = uint256(uint96(flowRate)) * 7 days;
        setDefaultWrap(alice);
        startStream(alice, bob, flowRate);
        vm.warp(1 days);
        vm.startPrank(alice);
        // take alice balance to zero
        (int256 availableBalance,,,) = superToken.realtimeBalanceOfNow(alice);
        superToken.downgrade(uint256(availableBalance));
        vm.stopPrank();
        vm.expectEmit(true, true, true, true);
        emit WrapExecuted(getWrapIndex(alice, address(superToken), address(token)), wrappedAmount);
        manager.executeWrap(alice, address(superToken), address(token));
        assertEq(wrappedAmount, superToken.balanceOf(alice), "Alice balance should match upper * flowrate");
    }

}
