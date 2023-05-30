// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Test } from "forge-std/Test.sol";
import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import { ISuperToken, SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../../../contracts/superfluid/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../../../contracts/superfluid/PoolMemberNFT.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";

contract SuperTokenTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) { }

    function setUp() public override {
        super.setUp();
    }

    function testRevertSuperTokenUpdateCodeWrongNFTProxies() public {
        UUPSProxy cifProxy = new UUPSProxy();
        UUPSProxy cofProxy = new UUPSProxy();
        UUPSProxy paProxy = new UUPSProxy();
        UUPSProxy pmProxy = new UUPSProxy();

        ConstantInflowNFT cifNFTLogic = new ConstantInflowNFT(
            sf.host,
            IConstantOutflowNFT(address(cofProxy))
        );
        ConstantOutflowNFT cofNFTLogic = new ConstantOutflowNFT(
            sf.host,
            IConstantInflowNFT(address(cifProxy))
        );
        PoolAdminNFT paNFTLogic = new PoolAdminNFT(
            sf.host
        );
        PoolMemberNFT pmNFTLogic = new PoolMemberNFT(
            sf.host
        );

        cifNFTLogic.castrate();
        cofNFTLogic.castrate();
        paNFTLogic.castrate();
        pmNFTLogic.castrate();

        cifProxy.initializeProxy(address(cifNFTLogic));
        cofProxy.initializeProxy(address(cofNFTLogic));
        paProxy.initializeProxy(address(paNFTLogic));
        pmProxy.initializeProxy(address(pmNFTLogic));

        ConstantInflowNFT(address(cofProxy)).initialize("Constant Outflow NFT", "COF");
        ConstantOutflowNFT(address(cifProxy)).initialize("Constant Inflow NFT", "CIF");
        PoolAdminNFT(address(paProxy)).initialize("Pool Admin NFT", "PA");
        PoolMemberNFT(address(pmProxy)).initialize("Pool Member NFT", "PM");

        // all nft proxies incorrect
        SuperToken superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            ConstantInflowNFT(address(cifProxy)),
            PoolAdminNFT(address(paProxy)),
            PoolMemberNFT(address(pmProxy))
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        superToken.updateCode(address(superTokenLogic));

        // inflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            superToken.CONSTANT_OUTFLOW_NFT(),
            ConstantInflowNFT(address(cifProxy)),
            superToken.POOL_ADMIN_NFT(),
            superToken.POOL_MEMBER_NFT()
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        superToken.updateCode(address(superTokenLogic));

        // outflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            superToken.CONSTANT_INFLOW_NFT(),
            superToken.POOL_ADMIN_NFT(),
            superToken.POOL_MEMBER_NFT()
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        superToken.updateCode(address(superTokenLogic));
    }
}
