// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Test } from "forge-std/Test.sol";
import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import { ISuperToken, SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";

contract SuperTokenTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) { }

    function setUp() public override {
        super.setUp();
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
        superToken.updateCode(address(superTokenLogic));

        // inflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            superToken.CONSTANT_OUTFLOW_NFT(),
            ConstantInflowNFT(address(cifProxy))
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        superToken.updateCode(address(superTokenLogic));

        // outflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            superToken.CONSTANT_INFLOW_NFT()
        );
        vm.prank(address(sf.host));
        vm.expectRevert(ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector);
        superToken.updateCode(address(superTokenLogic));
    }
}
