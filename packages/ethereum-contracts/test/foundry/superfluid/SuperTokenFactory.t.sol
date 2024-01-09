// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import { SuperTokenFactory } from "../../../contracts/superfluid/SuperTokenFactory.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../../../contracts/agreements/gdav1/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../../../contracts/agreements/gdav1/PoolMemberNFT.sol";
import { ISuperToken, SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";

contract SuperTokenFactoryTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) { }

    function setUp() public override {
        super.setUp();
    }

    function testUpdateCodeSetsNewContracts() public {
        SuperToken newSuperTokenLogic = new SuperToken(
            sf.host,
            superToken.CONSTANT_OUTFLOW_NFT(),
            superToken.CONSTANT_INFLOW_NFT(),
            superToken.POOL_ADMIN_NFT(),
            superToken.POOL_MEMBER_NFT()
        );
        ConstantOutflowNFT newConstantOutflowNFTLogic = new ConstantOutflowNFT(
            sf.host,
            IConstantInflowNFT(address(superToken.CONSTANT_INFLOW_NFT()))
        );
        ConstantInflowNFT newConstantInflowNFTLogic = new ConstantInflowNFT(
            sf.host,
            IConstantOutflowNFT(address(superToken.CONSTANT_OUTFLOW_NFT()))
        );
        PoolAdminNFT newPoolAdminNFTLogic = new PoolAdminNFT(sf.host);
        PoolMemberNFT newPoolMemberNFTLogic = new PoolMemberNFT(sf.host);
        assertEq(
            UUPSProxiable(address(superToken.CONSTANT_OUTFLOW_NFT())).getCodeAddress(),
            address(sf.superTokenFactory.CONSTANT_OUTFLOW_NFT_LOGIC())
        );
        assertEq(
            UUPSProxiable(address(superToken.CONSTANT_INFLOW_NFT())).getCodeAddress(),
            address(sf.superTokenFactory.CONSTANT_INFLOW_NFT_LOGIC())
        );
        SuperTokenFactory newSuperTokenFactoryLogic = new SuperTokenFactory(
            sf.host,
            newSuperTokenLogic,
            newConstantOutflowNFTLogic,
            newConstantInflowNFTLogic,
            newPoolAdminNFTLogic,
            newPoolMemberNFTLogic
        );
        vm.startPrank(address(sf.host));
        // We expect this to revert if the protocol is not upgradeable
        if (sf.host.NON_UPGRADABLE_DEPLOYMENT()) {
            vm.expectRevert("UUPSProxiable: not upgradable");
        }
        sf.superTokenFactory.updateCode(address(newSuperTokenFactoryLogic));
        vm.stopPrank();

        // We only assert this if the protocol is upgradable
        if (!sf.host.NON_UPGRADABLE_DEPLOYMENT()) {
            assertEq(address(newConstantOutflowNFTLogic), address(sf.superTokenFactory.CONSTANT_OUTFLOW_NFT_LOGIC()));
            assertEq(address(newConstantInflowNFTLogic), address(sf.superTokenFactory.CONSTANT_INFLOW_NFT_LOGIC()));
        }
    }
}
