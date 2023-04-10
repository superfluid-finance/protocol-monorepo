// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Test } from "forge-std/Test.sol";
import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    ISuperToken,
    SuperToken
} from "../../../contracts/superfluid/SuperToken.sol";
import {
    ConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";

contract SuperTokenTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) {}

    function setUp() public override {
        super.setUp();
    }

    function testRevert_superToken_UpdateCode_Wrong_NFT_Proxies() public {
        ConstantInflowNFT cifNFTLogic = new ConstantInflowNFT(sf.cfa);
        ConstantOutflowNFT cofNFTLogic = new ConstantOutflowNFT(sf.cfa);
        UUPSProxy cifProxy = new UUPSProxy();
        UUPSProxy cofProxy = new UUPSProxy();

        cifProxy.initializeProxy(address(cifNFTLogic));
        cofProxy.initializeProxy(address(cofNFTLogic));

        // both nft proxies incorrect
        SuperToken superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            ConstantInflowNFT(address(cifProxy))
        );
        vm.prank(address(sf.host));
        vm.expectRevert(
            ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector
        );
        superToken.updateCode(address(superTokenLogic));

        // inflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            superToken.CONSTANT_OUTFLOW_NFT(),
            ConstantInflowNFT(address(cifProxy))
        );
        vm.prank(address(sf.host));
        vm.expectRevert(
            ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector
        );
        superToken.updateCode(address(superTokenLogic));

        // outflow nft proxy incorrect
        superTokenLogic = new SuperToken(
            sf.host,
            ConstantOutflowNFT(address(cofProxy)),
            superToken.CONSTANT_INFLOW_NFT()
        );
        vm.prank(address(sf.host));
        vm.expectRevert(
            ISuperToken.SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED.selector
        );
        superToken.updateCode(address(superTokenLogic));

    }
}
