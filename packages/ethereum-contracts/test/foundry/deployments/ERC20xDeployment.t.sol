// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { console, Test } from "forge-std/Test.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import {
    IConstantFlowAgreementV1,
    ConstantFlowAgreementV1,
    IConstantFlowAgreementHook
} from "../../../contracts/agreements/ConstantFlowAgreementV1.sol";
import {
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT,
    IConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    IInstantDistributionAgreementV1
} from "../../../contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import { IResolver } from "../../../contracts/interfaces/utils/IResolver.sol";
import {
    ISuperfluidGovernance
} from "../../../contracts/interfaces/superfluid/ISuperfluidGovernance.sol";
import {
    SuperfluidLoader
} from "../../../contracts/utils/SuperfluidLoader.sol";
import {
    ISuperfluid
} from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import {
    IERC20,
    ISuperToken
} from "../../../contracts/interfaces/superfluid/ISuperToken.sol";
import {
    ISuperTokenFactory,
    SuperToken,
    SuperTokenFactory
} from "../../../contracts/superfluid/SuperTokenFactory.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";
import { CFAv1Library } from "../../../contracts/apps/CFAv1Library.sol";

contract ERC20xDeploymentTest is Test {
    using CFAv1Library for CFAv1Library.InitData;
    uint256 polygonFork;

    string POLYGON_MAINNET_PROVIDER_URL =
        vm.envString("POLYGON_MAINNET_PROVIDER_URL");

    IResolver public constant resolver =
        IResolver(0xE0cc76334405EE8b39213E620587d815967af39C);
    ISuperfluid public host;
    ConstantFlowAgreementV1 public cfaV1;
    SuperfluidLoader public superfluidLoader;
    ISuperTokenFactory public superTokenFactory;
    ISuperfluidGovernance public governance;
    IERC20 public constant weth =
        IERC20(0x7ceB23fD6bC0adD59E62ac25578270cFf1b9f619);
    ISuperToken public constant ethX =
        ISuperToken(0x27e1e4E6BC79D93032abef01025811B7E4727e85);
    CFAv1Library.InitData public cfaV1Lib;

    address public constant TEST_ACCOUNT =
        0x0154d25120Ed20A516fE43991702e7463c5A6F6e;
    address public constant ALICE = address(1);
    address public constant BOB = address(2);
    address public constant DEFAULT_FLOW_OPERATOR = address(69);

    function setUp() public {
        polygonFork = vm.createSelectFork(POLYGON_MAINNET_PROVIDER_URL);
        superfluidLoader = SuperfluidLoader(
            resolver.get("SuperfluidLoader-v1")
        );
        SuperfluidLoader.Framework memory framework = superfluidLoader
            .loadFramework("v1");
        cfaV1 = ConstantFlowAgreementV1(address(framework.agreementCFAv1));
        host = ISuperfluid(framework.superfluid);
        cfaV1Lib = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(address(framework.agreementCFAv1))
        );
        governance = host.getGovernance();
        superTokenFactory = framework.superTokenFactory;
    }

    function assert_Flow_Rate_Is_Expected(
        address sender,
        address receiver,
        int96 expectedFlowRate
    ) public {
        (, int96 flowRate, , ) = cfaV1.getFlow(ethX, sender, receiver);
        assertEq(flowRate, expectedFlowRate);
    }

    function helper_Create_Update_Delete_Flow() public {
        // test that flows can still be created with SuperTokenFactory updated
        vm.prank(TEST_ACCOUNT);
        cfaV1Lib.createFlow(address(1), ethX, 42069);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, address(1), 42069);
        vm.prank(TEST_ACCOUNT);
        cfaV1Lib.updateFlow(address(1), ethX, 4206933);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, address(1), 4206933);
        vm.prank(TEST_ACCOUNT);
        cfaV1Lib.deleteFlow(TEST_ACCOUNT, address(1), ethX);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, address(1), 0);
    }

    function test_Full_Migration() public {
        address superTokenFactoryLogicPre = host.getSuperTokenFactoryLogic();
        address superTokenLogicPre = address(
            superTokenFactory.getSuperTokenLogic()
        );

        address constantFlowAgreementLogicPre = address(cfaV1.getCodeAddress());

        address governanceOwner = Ownable(address(governance)).owner();

        // Prank as governance owner
        vm.startPrank(governanceOwner);

        // Deploy new constant outflow nft logic
        ConstantOutflowNFT newConstantOutflowNFTLogic = new ConstantOutflowNFT();

        // Deploy new constant inflow nft logic
        ConstantInflowNFT newConstantInflowNFTLogic = new ConstantInflowNFT();

        // Deploy new super token logic
        SuperToken newSuperTokenLogic = new SuperToken(
            host,
            IConstantOutflowNFT(address(newConstantOutflowNFTLogic)),
            IConstantInflowNFT(address(newConstantInflowNFTLogic))
        );

        // the first upgrade of SuperTokenFactory is to add in updateLogicContracts
        // there is a separate PR open for this currently
        SuperTokenFactory newLogic = new SuperTokenFactory(
            host,
            newSuperTokenLogic
        );
        governance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(newLogic)
        );

        newLogic = new SuperTokenFactory(host, newSuperTokenLogic);

        // SuperTokenFactory.updateCode
        // _updateCodeAddress(newAddress): this upgrades the SuperTokenFactory logic
        // this.updateLogicContracts()
        // _updateSuperTokenLogic(): this deploys and sets the new SuperToken logic in SuperTokenFactory
        // _updateConstantOutflowNFTLogic(): deploy and set constant outflow nft logic contract
        // _updateConstantInflowNFTLogic(): deploy and set constant inflow nft logic contract
        governance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(newLogic)
        );

        address superTokenFactoryLogicPost = host.getSuperTokenFactoryLogic();
        address superTokenLogicPost = address(
            superTokenFactory.getSuperTokenLogic()
        );

        // validate that the logic contracts have been updated
        assertFalse(superTokenFactoryLogicPre == superTokenFactoryLogicPost);
        assertFalse(superTokenLogicPre == superTokenLogicPost);

        // assert that NFT logic contracts are set in SuperTokenFactory
        // IConstantOutflowNFT constantOutflowNFTLogic = superTokenFactory
        //     .getConstantOutflowNFTLogic();
        // IConstantInflowNFT constantInflowNFTLogic = superTokenFactory
        //     .getConstantInflowNFTLogic();
        // assertFalse(address(constantOutflowNFTLogic) == address(0));
        // assertFalse(address(constantInflowNFTLogic) == address(0));

        // expect revert when trying to initialize the logic contracts
        // vm.expectRevert("Initializable: contract is already initialized");
        // constantOutflowNFTLogic.initialize(ethX, "gm", "henlo");
        // vm.expectRevert("Initializable: contract is already initialized");
        // constantInflowNFTLogic.initialize(ethX, "gm", "henlo");

        vm.stopPrank();

        // create update and delete flows after updating SuperTokenFactory logic
        // after deploying and setting new SuperToken logic in SuperTokenFactory
        // after deploying and setting new NFT contracts in SuperTokenFactory
        helper_Create_Update_Delete_Flow();
        {
            vm.startPrank(governanceOwner);
            // deploy the outflow and inflow nft PROXY contracts
            // and initialize the proxies in the same txn
            // we would do this for all supertokens on each network
            // @note TODO we probably want to have a batch for this?
            // (
            //     IConstantOutflowNFT constantOutflowNFTProxy,
            //     IConstantInflowNFT constantInflowNFTProxy,
            //     ,

            // ) = superTokenFactory.deployNFTProxyContractsAndInititialize(
            //         ethX,
            //         address(constantOutflowNFTLogic),
            //         address(constantInflowNFTLogic),
            //         address(0),
            //         address(0)
            //     );

            ISuperToken[] memory superTokens = new ISuperToken[](1);
            superTokens[0] = ethX;

            // batch update SuperToken logic
            // we would put all supertokens not just ethX in reality
            governance.batchUpdateSuperTokenLogic(host, superTokens);

            assertEq(address(ethX.constantOutflowNFT()), address(0));
            assertEq(address(ethX.constantInflowNFT()), address(0));

            // link the NFT contracts to the SuperToken
            // ethX.setNFTProxyContracts(
            //     address(constantOutflowNFTProxy),
            //     address(constantInflowNFTProxy),
            //     address(0),
            //     address(0)
            // );

            // validate that the NFT contracts are set in the SuperToken
            assertFalse(address(ethX.constantOutflowNFT()) == address(0));
            assertFalse(address(ethX.constantInflowNFT()) == address(0));

            vm.stopPrank();
        }
        // create update and delete flows after updating super token logic
        helper_Create_Update_Delete_Flow();

        // upgrade cfa logic contract
        vm.startPrank(governanceOwner);

        ConstantFlowAgreementV1 newCFAv1Logic = new ConstantFlowAgreementV1(
            host,
            IConstantFlowAgreementHook(address(0))
        );
        address[] memory agreementAddresses = new address[](1);
        agreementAddresses[0] = address(newCFAv1Logic);

        governance.updateContracts(
            host,
            address(0),
            agreementAddresses,
            address(0)
        );
        address constantFlowAgreementLogicPost = address(
            cfaV1.getCodeAddress()
        );

        assertFalse(
            constantFlowAgreementLogicPre == constantFlowAgreementLogicPost
        );
        vm.stopPrank();

        // create update and delete flows after updating CFAv1 Logic
        helper_Create_Update_Delete_Flow();

        // LOGGING
        console.log("Chain ID                                   ", block.chainid);
        console.log("Governance Owner Address:                  ", governanceOwner);
        console.log("SuperfluidLoader Address:                  ", address(superfluidLoader));
        console.log("Superfluid Host Address:                   ", address(host));
        console.log("Superfluid Governance Address:             ", address(governance));
        console.log("SuperTokenFactory Address:                 ", address(superTokenFactory));
        console.log("SuperTokenFactoryLogic Pre Migration:      ", superTokenFactoryLogicPre);
        console.log("SuperTokenFactoryLogic Post Migration:     ", superTokenFactoryLogicPost);
        console.log("SuperTokenLogic Pre Migration:             ", superTokenLogicPre);
        console.log("SuperTokenLogic Post Migration:            ", superTokenLogicPost);
        console.log("ConstantFlowAgreementLogic Pre Migration:  ", constantFlowAgreementLogicPre);
        console.log("ConstantFlowAgreementLogic Post Migration: ", constantFlowAgreementLogicPost);
    }
}
