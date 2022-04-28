// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { Test } from "forge-std/Test.sol";
import { Vm } from "forge-std/Vm.sol";
import { ERC20PresetMinterPauser } from "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";
import { SuperToken } from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { ISuperTokenFactory } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperTokenFactory.sol";
import { IInstantDistributionAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { SuperfluidFramework, Superfluid, ConstantFlowAgreementV1, SuperTokenFactory } from "./SuperfluidFramework.t.sol";
import { SimpleACLCloseResolver } from "../SimpleACLCloseResolver.sol";
import { OpsMock } from "../mocks/OpsMock.sol";

contract SimpleACLCloseResolverTest is Test {
    Vm private _vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

    uint256 private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address private constant alpha = address(1);
    address private constant npc = address(2);
    OpsMock private ops;
    address private constant admin = address(420);
    address[] private TEST_ACCOUNTS = [alpha, npc, admin];

    using CFAv1Library for CFAv1Library.InitData;

    Superfluid private _host;
    ConstantFlowAgreementV1 private _cfa;
    SuperTokenFactory private _superTokenFactory;
    ERC20PresetMinterPauser private _token;
    ISuperToken private _superToken;
    SimpleACLCloseResolver private _simpleACLCloseResolver;
    CFAv1Library.InitData private _cfaLib;

    /**************************************************************************
     * Setup Function
     *************************************************************************/

    function setUp() public {
        // Deploy and retrieve contracts using `_vm` and `admin`. The admin deploys everything.
        (_host, _cfa, , _superTokenFactory) = new SuperfluidFramework(
            _vm,
            admin
        ).framework();
        _cfaLib.host = _host;
        _cfaLib.cfa = _cfa;

        // NOTE: If you're copy-pasting this for your own test, you can safely delete the rest of
        // this function :)

        // Become admin
        _vm.startPrank(admin);

        // initialize underlying token
        _token = new ERC20PresetMinterPauser("Butler Token", "BUT");
        _superToken = _superTokenFactory.createERC20Wrapper(
            _token,
            18,
            ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
            "Super Butler Token",
            "BUTx"
        );
        _vm.stopPrank();

        // mint tokens for accounts
        for (uint8 i = 0; i < TEST_ACCOUNTS.length; i++) {
            _vm.prank(admin);
            _token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            _vm.startPrank(TEST_ACCOUNTS[i]);
            _token.approve(address(_superToken), INIT_TOKEN_BALANCE);
            _superToken.upgrade(INIT_TOKEN_BALANCE);
            _vm.stopPrank();
        }

        _vm.startPrank(admin);
        // initialize Simple ACL Close Resolver
        _simpleACLCloseResolver = new SimpleACLCloseResolver(
            block.timestamp + 14400,
            _cfa,
            _superToken,
            admin,
            npc
        );

        // create ops mock contract
        ops = new OpsMock(address(_simpleACLCloseResolver), address(_host));

        _simpleACLCloseResolver.updateOps(address(ops));

        _vm.stopPrank();
    }

    /**************************************************************************
     * Tests
     *************************************************************************/

    /**
     * Revert Tests
     */

    // Ops Tests
    function testCannotExecuteRightNow() public {
        // create a stream from admin to npc
        _vm.startPrank(admin);
        _cfaLib.flow(npc, _superToken, 100);

        // fails because cannot execute yet
        _vm.expectRevert(OpsMock.CannotExecute.selector);

        ops.exec();
        _vm.stopPrank();
    }

    function testCannotCloseWithoutApproval() public {
        // create a stream from admin to npc
        _vm.startPrank(admin);
        _cfaLib.flow(npc, _superToken, 100);

        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14401);
        _vm.expectRevert(OpsMock.FailedExecution.selector);

        // still fail because of no flowOperator approval
        ops.exec();
    }

    function testCannotCloseBeforeEndTime() public {
        // create a stream from admin to npc
        _vm.startPrank(admin);
        _cfaLib.flow(npc, _superToken, 100);

        // grant permissions so ops has full flow operator permissions
        _grantFlowOperatorPermissions(
            address(_superToken),
            admin,
            address(ops)
        );

        // fails because cannot execute yet
        _vm.expectRevert(OpsMock.CannotExecute.selector);

        ops.exec();
        _vm.stopPrank();
    }

    function testCannotCloseNonExistentFlow() public {
        _vm.startPrank(admin);

        // grant permissions so ops has full flow operator permissions
        _grantFlowOperatorPermissions(
            address(_superToken),
            admin,
            address(ops)
        );

        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14401);
        _vm.expectRevert(OpsMock.FailedExecution.selector);
        // try to close a stream that doesn't exist
        ops.exec();
        _vm.stopPrank();
    }

    // Resolver Tests

    function testCannotSetInvalidReceiver() public {
        _vm.startPrank(admin);
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowReceiver.selector);
        _simpleACLCloseResolver.updateFlowReceiver(address(0));
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowReceiver.selector);
        _simpleACLCloseResolver.updateFlowReceiver(admin);
    }

    function testCannotSetInvalidSender() public {
        _vm.startPrank(admin);
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowSender.selector);
        _simpleACLCloseResolver.updateFlowSender(address(0));
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowSender.selector);
        _simpleACLCloseResolver.updateFlowSender(npc);
    }

    /**
     * Passing Tests
     */

    // Ops Tests
    function testCanCloseAfterEndTime() public {
        // create a stream from admin to npc
        _vm.startPrank(admin);
        _cfaLib.flow(npc, _superToken, 100);

        // grant permissions so ops has full flow operator permissions
        _grantFlowOperatorPermissions(
            address(_superToken),
            admin,
            address(ops)
        );

        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14400);

        ops.exec();
        _vm.stopPrank();
    }

    /**
     * Fuzz Tests
     */

    // Resolver Tests
    function testNonOpsCannotRunChecker(address _caller) public {
        _vm.assume(_caller != address(ops));
        _vm.expectRevert(SimpleACLCloseResolver.OpsOnly.selector);
        _vm.prank(_caller);
        _simpleACLCloseResolver.checker();
    }

    function testCanUpdateEndTime(uint256 _endTime, address _newOwner) public {
        _vm.warp(1);
        _vm.assume(_endTime > block.timestamp && _newOwner != address(0));

        _vm.prank(admin);
        _simpleACLCloseResolver.transferOwnership(_newOwner);

        _vm.prank(_newOwner);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    function testCanUpdateFlowReceiver(address _flowReceiver, address _newOwner)
        public
    {
        _vm.assume(
            _flowReceiver != _newOwner &&
                _flowReceiver != address(0) &&
                _newOwner != address(0)
        );

        _vm.prank(admin);
        _simpleACLCloseResolver.transferOwnership(_newOwner);

        _vm.prank(_newOwner);
        _simpleACLCloseResolver.updateFlowReceiver(_flowReceiver);
    }

    function testCanUpdateFlowSender(address _flowSender, address _newOwner)
        public
    {
        _vm.assume(
            _flowSender != _newOwner &&
                _flowSender != address(0) &&
                _newOwner != address(0)
        );

        _vm.prank(admin);
        _simpleACLCloseResolver.transferOwnership(_newOwner);

        _vm.prank(_newOwner);
        _simpleACLCloseResolver.updateFlowSender(_flowSender);
    }

    function testCanUpdateOps(address _newOps, address _newOwner) public {
        _vm.assume(
            _newOps != _newOwner &&
                _newOps != address(0) &&
                _newOwner != address(0)
        );

        _vm.prank(admin);
        _simpleACLCloseResolver.transferOwnership(_newOwner);

        _vm.prank(_newOwner);
        _simpleACLCloseResolver.updateOps(_newOps);

        // call checker as _newOps
        _vm.prank(_newOps);
        _simpleACLCloseResolver.checker();
    }

    function testCannotUpdateEndTimeToEarlier(uint256 _endTime) public {
        _vm.expectRevert(SimpleACLCloseResolver.InvalidEndTime.selector);
        _vm.warp(100000000);
        _vm.assume(_endTime < block.timestamp);
        _vm.startPrank(admin);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    function testNonOwnerCannotUpdateFlowSender(address _nonOwner) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != admin);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateFlowSender(_nonOwner);
    }

    function testNonOwnerCannotUpdateFlowReceiver(address _nonOwner) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != admin);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateFlowReceiver(_nonOwner);
    }

    function testNonOwnerCannotUpdateEndTime(
        address _nonOwner,
        uint256 _endTime
    ) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != admin && _endTime > block.timestamp);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    function testNonOwnerCannotUpdateOps(address _nonOwner) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != admin);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateOps(_nonOwner);
    }

    // TODO: should be able to update flow receiver and handle closing of new flow

    /**************************************************************************
     * Helper functions
     *************************************************************************/

    function _grantFlowOperatorPermissions(
        address _flowSuperToken,
        address _sender,
        address _flowOperator
    ) internal {
        _host.callAgreement(
            _cfa,
            abi.encodeWithSelector(
                _cfa.authorizeFlowOperatorWithFullControl.selector,
                _flowSuperToken,
                _sender,
                _flowOperator,
                new bytes(0)
            ),
            "0x"
        );
    }
}
