// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/console.sol";
import "../FoundrySuperfluidTester.sol";
import { SuperAppBaseCFA } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBaseCFA.sol";
import { SuperAppBaseCFATester } from "@superfluid-finance/ethereum-contracts/contracts/mocks/SuperAppBaseCFATester.sol";
import { ISuperToken, ISuperApp, SuperAppDefinitions } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { SuperTokenV1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperTokenV1Library.sol";

contract SuperAppBaseCFATest is FoundrySuperfluidTester {

    using SuperTokenV1Library for SuperToken;
    using SuperTokenV1Library for ISuperToken;

    SuperAppBaseCFATester superApp;
    address superAppAddress;
    ISuperToken otherSuperToken;

    constructor () FoundrySuperfluidTester(3) { }

    function setUp() public override virtual {
        super.setUp();
        vm.startPrank(admin);
        superApp = new SuperAppBaseCFATester(sf.host, true, true, true);
        superAppAddress = address(superApp);
        otherSuperToken = superTokenDeployer.deployPureSuperToken("FTT", "FTT", 1e27);
        otherSuperToken.transfer(alice, 1e21);
        vm.stopPrank();
    }

    function _genManifest(bool activateOnCreated, bool activateOnUpdated, bool activateOnDeleted) internal pure returns (uint256) {

        uint256 callBackDefinitions = SuperAppDefinitions.APP_LEVEL_FINAL
        | SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP;

        if (!activateOnCreated) {
            callBackDefinitions |= SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP;
        }

        if (!activateOnUpdated) {
            callBackDefinitions |= SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP
            | SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP;
        }

        if (!activateOnDeleted) {
            callBackDefinitions |= SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP
            | SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;
        }

        return callBackDefinitions;
    }

    function _deploySuperAppAndGetConfig(bool activateOnCreated, bool activateOnUpdated, bool activateOnDeleted) internal returns (SuperAppBaseCFATester, uint256 configWord) {
        SuperAppBaseCFATester mySuperApp = new SuperAppBaseCFATester(sf.host, activateOnCreated, activateOnUpdated, activateOnDeleted);
        uint256 appConfig = _genManifest(activateOnCreated, activateOnUpdated, activateOnDeleted);
        return (mySuperApp, appConfig);
    }

    function testOnFlagsSetAppManifest() public {
        //all onOperations
        (SuperAppBaseCFATester mySuperApp, uint256 configWord) = _deploySuperAppAndGetConfig(true, true, true);
        (bool isSuperApp,,uint256 noopMask) = sf.host.getAppManifest(ISuperApp(mySuperApp));
        configWord = configWord & SuperAppDefinitions.AGREEMENT_CALLBACK_NOOP_BITMASKS;
        assertTrue(isSuperApp, "isSuperApp");
        assertEq(noopMask, configWord, "noopMask");

        // activateOnUpdated and activateOnDeleted
        (mySuperApp, configWord) = _deploySuperAppAndGetConfig(false, true, true);
        (isSuperApp,,noopMask) = sf.host.getAppManifest(ISuperApp(mySuperApp));
        configWord = configWord & SuperAppDefinitions.AGREEMENT_CALLBACK_NOOP_BITMASKS;
        assertTrue(isSuperApp, "isSuperApp");
        assertEq(noopMask, configWord, "noopMask");

        // activateOnDeleted
        (mySuperApp, configWord) = _deploySuperAppAndGetConfig(false, false, true);
        (isSuperApp,,noopMask) = sf.host.getAppManifest(ISuperApp(mySuperApp));
        configWord = configWord & SuperAppDefinitions.AGREEMENT_CALLBACK_NOOP_BITMASKS;
        assertTrue(isSuperApp, "isSuperApp");
        assertEq(noopMask, configWord, "noopMask");

        // no Operations
        (mySuperApp, configWord) = _deploySuperAppAndGetConfig(false, false, false);
        (isSuperApp,,noopMask) = sf.host.getAppManifest(ISuperApp(mySuperApp));
        configWord = configWord & SuperAppDefinitions.AGREEMENT_CALLBACK_NOOP_BITMASKS;
        assertTrue(isSuperApp, "isSuperApp");
        assertEq(noopMask, configWord, "noopMask");
    }

    function testAllowAllSuperTokensByDefault() public {
        assertTrue(superApp.isAcceptedSuperToken(superToken), "unrestricted: primary SuperToken accepted");
        assertTrue(superApp.isAcceptedSuperToken(otherSuperToken), "unrestricted: other SuperToken accepted");
    }

    function testRestrictAllowedSuperTokens() public {
        // Using the setter activates the filter, ONLY this token shall be accepted now
        superApp.setAcceptedSuperToken(superToken, true);
        assertTrue(superApp.isAcceptedSuperToken(superToken), "restricted: primary SuperToken accepted");
        assertFalse(superApp.isAcceptedSuperToken(otherSuperToken), "restricted: other SuperToken not accepted");
        superApp.setAcceptedSuperToken(otherSuperToken, true); // both shall now be accepted
        assertTrue(superApp.isAcceptedSuperToken(otherSuperToken), "restricted: other SuperToken now accepted");
    }

    function testUnauthorizedHost() public {
        vm.startPrank(eve);

        vm.expectRevert(SuperAppBaseCFA.UnauthorizedHost.selector);
        superApp.afterAgreementCreated(superToken, address(sf.cfa), "0x", "0x", "0x", "0x");

        vm.expectRevert(SuperAppBaseCFA.UnauthorizedHost.selector);
        superApp.afterAgreementUpdated(superToken, address(sf.cfa), "0x", "0x", "0x", "0x");

        // termination callback doesn't revert, but should have no side effects
        superApp.afterAgreementTerminated(superToken, address(sf.cfa), "0x", "0x", "0x", "0x");
        assertEq(superApp.afterSenderHolder(), address(0));

        vm.stopPrank();
    }

    function testNotAcceptedAgreement() public {
        vm.startPrank(address(sf.host));

        // correct host, but wrong agreement (ida instead of cfa)
        superApp.afterAgreementCreated(superToken, address(sf.ida), "0x", "0x", "0x", "0x");
        // should have no side effects
        assertEq(superApp.afterSenderHolder(), address(0));

        superApp.afterAgreementUpdated(superToken, address(sf.ida), "0x", "0x", "0x", "0x");
        assertEq(superApp.afterSenderHolder(), address(0));

        superApp.afterAgreementTerminated(superToken, address(sf.ida), "0x", "0x", "0x", "0x");
        assertEq(superApp.afterSenderHolder(), address(0));

        vm.stopPrank();
    }

    // test create flow
    function testCreateFlowToSuperApp() public {
        vm.startPrank(alice);
        superToken.createFlow(superAppAddress, 100);
        assertEq(superToken.getFlowRate(alice, superAppAddress), 100);
        assertEq(superApp.afterSenderHolder(), alice);
        vm.stopPrank();
    }

    // test update flow
    function testUpdateFlowToSuperApp() public {
        vm.startPrank(alice);
        superToken.createFlow(superAppAddress, 100);
        assertEq(superToken.getFlowRate(alice, superAppAddress), 100);
        assertEq(superApp.afterSenderHolder(), alice);
        assertEq(superApp.oldFlowRateHolder(), 0);
        superToken.updateFlow(superAppAddress, 200);
        assertEq(superToken.getFlowRate(alice, superAppAddress), 200);
        assertEq(superApp.afterSenderHolder(), alice);
        assertEq(superApp.oldFlowRateHolder(), 100);
        vm.stopPrank();
    }

    // test delete flow
    function testDeleteFlowToSuperApp() public {
        vm.startPrank(alice);
        superToken.createFlow(superAppAddress, 100);
        assertEq(superToken.getFlowRate(alice, superAppAddress), 100);
        assertEq(superApp.afterSenderHolder(), alice);
        assertEq(superApp.oldFlowRateHolder(), 0);
        superToken.deleteFlow(alice, superAppAddress);
        assertEq(superToken.getFlowRate(alice, superAppAddress), 0);
        assertEq(superApp.afterSenderHolder(), alice);
        assertEq(superApp.afterReceiverHolder(), superAppAddress);
        assertEq(superApp.oldFlowRateHolder(), 100);
        vm.stopPrank();
    }

    function testFlowOfNotAcceptedSuperTokenToSuperApp() public {
        vm.startPrank(alice);
        // enable the filter
        superApp.setAcceptedSuperToken(superToken, true);
        vm.expectRevert(SuperAppBaseCFA.NotAcceptedSuperToken.selector);
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.createFlow,
                (otherSuperToken, address(superApp), int96(69), new bytes(0))
            ),
            new bytes(0) // userData
        );
        vm.stopPrank();
    }
}
