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

        _simpleACLCloseResolver.setOps(address(ops));

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
    function testNonOpsCannotRunChecker() public {
        _vm.expectRevert(SimpleACLCloseResolver.OpsOnly.selector);
        _simpleACLCloseResolver.checker();
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
        _vm.warp(block.timestamp + 14401);

        ops.exec();
        _vm.stopPrank();
    }

    // Resolver Tests
    function testOpsCanRunChecker() public {
        _vm.prank(address(ops));
        (bool canExec, ) = _simpleACLCloseResolver.checker();
        assertEq(canExec, false);
    }

    /**
     * Fuzz Tests
     */

    // Resolver Tests
    function testUpdateEndTime(uint256 _endTime) public {
        _vm.warp(1);
        _vm.assume(_endTime > block.timestamp);
        _vm.startPrank(admin);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    function testCannotUpdateEndTimeToEarlier(uint256 _endTime) public {
        _vm.expectRevert(SimpleACLCloseResolver.InvalidEndTime.selector);
        _vm.warp(100000000);
        _vm.assume(_endTime < block.timestamp);
        _vm.startPrank(admin);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    // TODO: should be able to update flow receiver

    // TODO: should be able to change ops and run checker with new ops

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
