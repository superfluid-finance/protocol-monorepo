// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import { ISuperToken, SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { ISuperAgreement } from "../../../contracts/interfaces/superfluid/ISuperAgreement.sol";
import { ISuperfluid } from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import { AgreementMock } from "../../../contracts/mocks/AgreementMock.sol";

contract SuperfluidGovernanceIntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    constructor() FoundrySuperfluidTester(3) { }

    function testChangeSuperTokenAdmin(address newAdmin) public {
        vm.assume(newAdmin != address(0));

        vm.startPrank(sf.governance.owner());
        sf.governance.changeSuperTokenAdmin(sf.host, superToken, newAdmin);
        vm.stopPrank();

        assertEq(superToken.getAdmin(), newAdmin, "Superfluid.t: super token admin not changed");
    }

    function testRevertChangeSuperTokenAdminWhenCallerIsNotNotGovOwner(address newAdmin) public {
        vm.assume(newAdmin != address(0));

        vm.startPrank(newAdmin);
        vm.expectRevert();
        sf.governance.changeSuperTokenAdmin(sf.host, superToken, newAdmin);
        vm.stopPrank();
    }

    function testRevertWhenHostIsNotAdmin(address initialAdmin) public {
        vm.assume(initialAdmin != address(0));
        vm.assume(initialAdmin != address(sf.host));

        vm.startPrank(address(sf.host));
        superToken.changeAdmin(initialAdmin);
        vm.stopPrank();

        vm.startPrank(sf.governance.owner());
        vm.expectRevert(ISuperToken.SUPER_TOKEN_ONLY_ADMIN.selector);
        sf.governance.changeSuperTokenAdmin(sf.host, superToken, initialAdmin);
        vm.stopPrank();
    }

    function testBatchChangeSuperTokenAdmin(address newAdmin) public {
        vm.assume(newAdmin != address(0));

        (, ISuperToken localSuperToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        ISuperToken[] memory superTokens = new ISuperToken[](2);
        superTokens[0] = superToken;        // host admin
        superTokens[1] = localSuperToken;   // host admin

        address[] memory newAdmins = new address[](2);
        newAdmins[0] = newAdmin;
        newAdmins[1] = newAdmin;

        vm.startPrank(sf.governance.owner());
        sf.governance.batchChangeSuperTokenAdmin(sf.host, superTokens, newAdmins);
        vm.stopPrank();

        assertEq(superToken.getAdmin(), newAdmin, "Superfluid.t: super token admin not changed");
    }

    function testRevertBatchChangeSuperTokenAdminWhenHostNotAdmin(address newAdmin) public {
        vm.assume(newAdmin != address(0));

        (, ISuperToken localSuperToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max, alice);

        ISuperToken[] memory superTokens = new ISuperToken[](2);
        superTokens[0] = superToken;        // host admin
        superTokens[1] = localSuperToken;   // non-host admin

        address[] memory newAdmins = new address[](2);
        newAdmins[0] = newAdmin;
        newAdmins[1] = newAdmin;

        vm.startPrank(sf.governance.owner());
        vm.expectRevert();
        sf.governance.batchChangeSuperTokenAdmin(sf.host, superTokens, newAdmins);
        vm.stopPrank();
    }

    function testRevertBatchChangeWhenCallerIsNotGovOwner(address newAdmin) public {
        vm.assume(newAdmin != address(0));
        vm.assume(newAdmin != address(sf.governance.owner()));

        (, ISuperToken localSuperToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        ISuperToken[] memory superTokens = new ISuperToken[](2);
        superTokens[0] = superToken;        // host admin
        superTokens[1] = localSuperToken;   // host admin

        address[] memory newAdmins = new address[](2);
        newAdmins[0] = newAdmin;
        newAdmins[1] = newAdmin;

        vm.startPrank(newAdmin);
        vm.expectRevert();
        sf.governance.batchChangeSuperTokenAdmin(sf.host, superTokens, newAdmins);
        vm.stopPrank();
    }
}
