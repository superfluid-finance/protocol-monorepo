// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Test } from "forge-std/Test.sol";
import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import { IERC20, ISuperToken, SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import { TokenDeployerLibrary } from "../../../contracts/utils/SuperfluidFrameworkDeploymentSteps.sol";

contract SuperTokenIntegrationTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) { }

    function setUp() public override {
        super.setUp();
    }

    function testUnderlyingTokenDecimals() public {
        assertEq(token.decimals(), superToken.getUnderlyingDecimals());
    }

    function testToUnderlyingAmountWithUpgrade(uint8 decimals, uint256 amount) public {
        vm.assume(amount < type(uint64).max);
        // We assume that most underlying tokens will not have more than 32 decimals
        vm.assume(decimals <= 32);
        (TestToken localToken, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", decimals, type(uint256).max);
        (uint256 underlyingAmount, uint256 adjustedAmount) = localSuperToken.toUnderlyingAmount(amount);
        localToken.mint(alice, INIT_TOKEN_BALANCE);
        vm.startPrank(alice);
        localToken.approve(address(localSuperToken), underlyingAmount);
        localSuperToken.upgrade(adjustedAmount);
        vm.stopPrank();
        uint256 upgradedBalance = localSuperToken.balanceOf(alice);
        assertEq(upgradedBalance, adjustedAmount, "testToUnderlyingAmount: upgraded amount wrong");
    }

    function testToUnderlyingAmountWithDowngrade(uint8 decimals, uint256 upgradeAmount, uint256 downgradeAmount)
        public
    {
        vm.assume(upgradeAmount < type(uint64).max);
        // We assume that most underlying tokens will not have more than 32 decimals
        vm.assume(decimals <= 32);
        vm.assume(downgradeAmount < upgradeAmount);
        (TestToken localToken, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", decimals, type(uint256).max);
        (uint256 underlyingAmount, uint256 adjustedAmount) = localSuperToken.toUnderlyingAmount(upgradeAmount);
        localToken.mint(alice, INIT_TOKEN_BALANCE);

        vm.startPrank(alice);
        localToken.approve(address(localSuperToken), underlyingAmount);
        localSuperToken.upgrade(adjustedAmount);

        uint256 underlyingBalanceBefore = localToken.balanceOf(alice);
        (underlyingAmount, adjustedAmount) = localSuperToken.toUnderlyingAmount(downgradeAmount);
        localSuperToken.downgrade(adjustedAmount);
        uint256 underlyingBalance = localToken.balanceOf(alice);
        vm.stopPrank();

        assertEq(
            underlyingBalance,
            underlyingBalanceBefore + underlyingAmount,
            "testToUnderlyingAmount: underlying amount wrong"
        );
    }

    function testRevertSuperTokenUpdateCodeWrongNFTProxies() public {
        UUPSProxy cifProxy = new UUPSProxy();
        UUPSProxy cofProxy = new UUPSProxy();

        ConstantInflowNFT cifNFTLogic = new ConstantInflowNFT(
            sf.host,
            IConstantOutflowNFT(address(cofProxy))
        );
        ConstantOutflowNFT cofNFTLogic = new ConstantOutflowNFT(
            sf.host,
            IConstantInflowNFT(address(cifProxy))
        );

        cifNFTLogic.castrate();
        cofNFTLogic.castrate();

        cifProxy.initializeProxy(address(cifNFTLogic));
        cofProxy.initializeProxy(address(cofNFTLogic));

        ConstantInflowNFT(address(cofProxy)).initialize("Constant Outflow NFT", "COF");
        ConstantOutflowNFT(address(cifProxy)).initialize("Constant Inflow NFT", "CIF");

        // both nft proxies incorrect
        SuperToken superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            ConstantInflowNFT(address(cifProxy))
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        UUPSProxiable(address(superToken)).updateCode(address(superTokenLogic));

        // inflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            superToken.CONSTANT_OUTFLOW_NFT(),
            ConstantInflowNFT(address(cifProxy))
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        UUPSProxiable(address(superToken)).updateCode(address(superTokenLogic));

        // outflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            superToken.CONSTANT_INFLOW_NFT()
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        UUPSProxiable(address(superToken)).updateCode(address(superTokenLogic));
    }

    function testInitializeSuperTokenWithAndWithoutAdminOverride(address adminOverride) public {
        (, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max, adminOverride);

        assertEq(
            localSuperToken.getAdminOverride().admin,
            adminOverride,
            "testInitializeSuperTokenWithAndWithoutAdminOverride: admin override not set correctly"
        );
    }

    function testOnlyHostCanChangeAdminWhenNoAdminOverride(address adminOverride) public {
        (, ISuperToken localSuperToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        vm.startPrank(address(sf.host));
        localSuperToken.changeAdmin(adminOverride);
        vm.stopPrank();

        assertEq(
            localSuperToken.getAdminOverride().admin,
            adminOverride,
            "testOnlyHostCanChangeAdminWhenNoAdminOverride: admin override not set correctly"
        );
    }

    function testOnlyAdminOverrideCanChangeAdmin(address adminOverride, address newAdminOverride) public {
        if (adminOverride == address(0)) {
            adminOverride = address(sf.host);
        }

        (, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max, adminOverride);

        vm.startPrank(adminOverride);
        localSuperToken.changeAdmin(newAdminOverride);
        vm.stopPrank();

        assertEq(
            localSuperToken.getAdminOverride().admin,
            newAdminOverride,
            "testOnlyAdminOverrideCanChangeAdmin: admin override not set correctly"
        );
    }

    function testRevertWhenNonAdminTriesToChangeAdmin(address adminOverride, address nonAdminOverride) public {
        vm.assume(adminOverride != nonAdminOverride);
        vm.assume(nonAdminOverride != address(0));
        if (adminOverride == address(0)) {
            adminOverride = address(sf.host);
        }

        (, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max, adminOverride);

        vm.startPrank(nonAdminOverride);
        vm.expectRevert(ISuperToken.SUPER_TOKEN_ONLY_ADMIN.selector);
        localSuperToken.changeAdmin(nonAdminOverride);
        vm.stopPrank();
    }

    function testOnlyHostCanUpdateCodeWhenNoAdminOverride() public {
        (TestToken localTestToken, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        SuperToken newSuperTokenLogic =
            _helperDeploySuperTokenAndInitialize(localSuperToken, localTestToken, 18, "FTT", "FTT", address(0));

        vm.startPrank(address(sf.host));
        UUPSProxiable(address(localSuperToken)).updateCode(address(newSuperTokenLogic));
        vm.stopPrank();

        assertEq(
            UUPSProxiable(address(localSuperToken)).getCodeAddress(),
            address(newSuperTokenLogic),
            "testOnlyHostCanUpdateCodeWhenNoAdminOverride: super token logic not updated correctly"
        );
    }

    function testOnlyAdminOverrideCanUpdateCode(address adminOverride) public {
        if (adminOverride == address(0)) {
            adminOverride = address(sf.host);
        }

        (TestToken localTestToken, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max, adminOverride);

        SuperToken newSuperTokenLogic =
            _helperDeploySuperTokenAndInitialize(localSuperToken, localTestToken, 18, "FTT", "FTT", adminOverride);

        vm.startPrank(adminOverride);
        UUPSProxiable(address(localSuperToken)).updateCode(address(newSuperTokenLogic));
        vm.stopPrank();

        assertEq(
            UUPSProxiable(address(localSuperToken)).getCodeAddress(),
            address(newSuperTokenLogic),
            "testOnlyHostCanUpdateCodeWhenNoAdminOverride: super token logic not updated correctly"
        );
    }

    function testRevertWhenNonAdminTriesToUpdateCode(address adminOverride, address nonAdminOverride) public {
        vm.assume(adminOverride != address(sf.host));

        (TestToken localTestToken, ISuperToken localSuperToken) =
            sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        SuperToken newSuperTokenLogic =
            _helperDeploySuperTokenAndInitialize(localSuperToken, localTestToken, 18, "FTT", "FTT", adminOverride);

        vm.startPrank(nonAdminOverride);
        vm.expectRevert(ISuperToken.SUPER_TOKEN_ONLY_ADMIN.selector);
        UUPSProxiable(address(localSuperToken)).updateCode(address(newSuperTokenLogic));
        vm.stopPrank();
    }
}
