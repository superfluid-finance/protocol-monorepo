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
    SuperTokenFactory,
    SuperTokenFactoryHelper
} from "../../../contracts/superfluid/SuperTokenFactory.sol";
import {
    SuperTokenFactoryUpdateLogicContractsTester
} from "../../../contracts/mocks/SuperTokenFactoryMock.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";

contract SuperTokenFactoryUpgradeTest is Test {
    using SuperTokenV1Library for ISuperToken;
    uint256 forkId;

    string public POLYGON_MAINNET_PROVIDER_URL;

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

    // arbitrary test account with a good amount of ETHx
    address public constant TEST_ACCOUNT =
        0x0154d25120Ed20A516fE43991702e7463c5A6F6e;
    address public constant ALICE = address(1);
    address public constant BOB = address(2);
    address public constant DEFAULT_FLOW_OPERATOR = address(69);

    function setUp() public {
        POLYGON_MAINNET_PROVIDER_URL = vm.envString(
            "POLYGON_MAINNET_PROVIDER_URL"
        );
        forkId = vm.createSelectFork(POLYGON_MAINNET_PROVIDER_URL);
        superfluidLoader = SuperfluidLoader(
            resolver.get("SuperfluidLoader-v1")
        );
        SuperfluidLoader.Framework memory framework = superfluidLoader
            .loadFramework("v1");
        cfaV1 = ConstantFlowAgreementV1(address(framework.agreementCFAv1));
        host = ISuperfluid(framework.superfluid);
        governance = host.getGovernance();
        superTokenFactory = framework.superTokenFactory;
    }

    function assert_Flow_Rate_Is_Expected(
        address sender,
        address receiver,
        int96 expectedFlowRate
    ) public {
        (, int96 flowRate, , ) = ethX.getFlowInfo(sender, receiver);
        assertEq(flowRate, expectedFlowRate);
    }

    function helper_Create_Update_Delete_Flow() public {
        // test that flows can still be created with SuperTokenFactory updated
        vm.startPrank(TEST_ACCOUNT);

        ethX.createFlow(address(1), 42069);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, address(1), 42069);
        ethX.updateFlow(address(1), 4206933);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, address(1), 4206933);
        ethX.deleteFlow(TEST_ACCOUNT, address(1));
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, address(1), 0);

        vm.stopPrank();
    }

    function test_Passing_Super_Token_Factory_Upgrade() public {
        address superTokenFactoryLogicPre = host.getSuperTokenFactoryLogic();
        address superTokenLogicPre = address(
            superTokenFactory.getSuperTokenLogic()
        );

        address governanceOwner = Ownable(address(governance)).owner();

        // Prank as governance owner
        vm.startPrank(governanceOwner);

        vm.expectRevert();
        superTokenFactory.updateLogicContracts();

        SuperTokenFactoryUpdateLogicContractsTester newLogic = new SuperTokenFactoryUpdateLogicContractsTester(
                host
            );
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

        // expect revert when trying to initialize the logic contracts
        vm.expectRevert("Initializable: contract is already initialized");
        SuperTokenFactory(superTokenFactoryLogicPost).initialize();

        vm.stopPrank();

        // this time calling updateLogicContracts doesn't work because of the only self modifier
        // this isn't reverting
        // vm.expectRevert(
        //     ISuperTokenFactory.SUPER_TOKEN_FACTORY_ONLY_SELF.selector
        // );
        // vm.prank(address(0));
        // superTokenFactory.updateLogicContracts();

        // the mock contract adds a new storage variable and sets it to 69
        assertEq(
            SuperTokenFactoryUpdateLogicContractsTester(
                address(superTokenFactory)
            ).newVariable(),
            69
        );

        vm.stopPrank();

        // create update and delete flows after updating SuperTokenFactory logic
        // after deploying and setting new SuperToken logic in SuperTokenFactory
        helper_Create_Update_Delete_Flow();

        // LOGGING
        console.log("Chain ID:                                  ", block.chainid);
        console.log("Governance Owner Address:                  ", governanceOwner);
        console.log("SuperfluidLoader Address:                  ", address(superfluidLoader));
        console.log("Superfluid Host Address:                   ", address(host));
        console.log("Superfluid Governance Address:             ", address(governance));
        console.log("SuperTokenFactory Address:                 ", address(superTokenFactory));
        console.log("SuperTokenFactoryLogic Pre Migration:      ", superTokenFactoryLogicPre);
        console.log("SuperTokenFactoryLogic Post Migration:     ", superTokenFactoryLogicPost);
        console.log("SuperTokenLogic Pre Migration:             ", superTokenLogicPre);
        console.log("SuperTokenLogic Post Migration:            ", superTokenLogicPost);
    }
}
