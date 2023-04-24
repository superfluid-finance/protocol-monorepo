// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import {
    SuperTokenFactory
} from "../../../contracts/superfluid/SuperTokenFactory.sol";
import {
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT,
    IConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    ISuperToken,
    SuperToken
} from "../../../contracts/superfluid/SuperToken.sol";
import {
    UUPSProxiable
} from "../../../contracts/upgradability/UUPSProxiable.sol";

contract SuperTokenFactoryTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) {}

    function setUp() public override {
        super.setUp();
    }

    function test_Passing_Update_Code_Sets_New_Contracts() public {
        SuperToken newSuperTokenLogic = new SuperToken(
            sf.host,
            superToken.CONSTANT_OUTFLOW_NFT(),
            superToken.CONSTANT_INFLOW_NFT()
        );
        ConstantOutflowNFT newConstantOutflowNFTLogic = new ConstantOutflowNFT(
            sf.host,
            IConstantInflowNFT(address(superToken.CONSTANT_INFLOW_NFT())),
            ""
        );
        ConstantInflowNFT newConstantInflowNFTLogic = new ConstantInflowNFT(
            sf.host,
            IConstantOutflowNFT(address(superToken.CONSTANT_OUTFLOW_NFT())),
            ""
        );
        assertEq(
            UUPSProxiable(address(superToken.CONSTANT_OUTFLOW_NFT()))
                .getCodeAddress(),
            address(sf.superTokenFactory.CONSTANT_OUTFLOW_NFT_LOGIC())
        );
        assertEq(
            UUPSProxiable(address(superToken.CONSTANT_INFLOW_NFT()))
                .getCodeAddress(),
            address(sf.superTokenFactory.CONSTANT_INFLOW_NFT_LOGIC())
        );
        SuperTokenFactory newSuperTokenFactoryLogic = new SuperTokenFactory(
            sf.host,
            newSuperTokenLogic,
            newConstantOutflowNFTLogic,
            newConstantInflowNFTLogic
        );
        vm.prank(address(sf.host));
        sf.superTokenFactory.updateCode(address(newSuperTokenFactoryLogic));
        assertEq(
            address(newConstantOutflowNFTLogic),
            address(sf.superTokenFactory.CONSTANT_OUTFLOW_NFT_LOGIC())
        );
        assertEq(
            address(newConstantInflowNFTLogic),
            address(sf.superTokenFactory.CONSTANT_INFLOW_NFT_LOGIC())
        );
    }
}
