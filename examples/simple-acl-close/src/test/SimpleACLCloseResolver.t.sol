// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { Test } from "forge-std/Test.sol";
import { Vm } from "forge-std/Vm.sol";
import { ERC1820RegistryCompiled } from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";
import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { ISuperTokenFactory } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperTokenFactory.sol";
import { IInstantDistributionAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import {
    CFAv1Library,
    ConstantFlowAgreementV1,
    ERC20PresetMinterPauser,
    Superfluid,
    SuperToken,
    SuperTokenFactory,
    SuperfluidFrameworkDeployer
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";
import { SimpleACLCloseResolver } from "../SimpleACLCloseResolver.sol";
import { OpsMock } from "../mocks/OpsMock.sol";

contract SimpleACLCloseResolverTest is Test {
    Vm private _vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

    uint256 private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address private constant receiver = address(2);
    OpsMock private ops;
    address private constant sender = address(420);
    address[] private TEST_ACCOUNTS = [receiver, sender];

    using CFAv1Library for CFAv1Library.InitData;

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperfluidFrameworkDeployer.Framework internal sfFramework;
    Superfluid private _host;
    ConstantFlowAgreementV1 private _cfa;
    SuperTokenFactory private _superTokenFactory;
    ERC20PresetMinterPauser private _token;
    ISuperToken private _superToken;
    SimpleACLCloseResolver private _simpleACLCloseResolver;

    /**************************************************************************
     * Setup Function
     *************************************************************************/

    constructor() {
        _vm.startPrank(sender);

        // Deploy ERC1820
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);

        sfDeployer = new SuperfluidFrameworkDeployer();
        sfFramework = sfDeployer.getFramework();
        _host = sfFramework.host;
        _cfa = sfFramework.cfa;
        _superTokenFactory = sfFramework.superTokenFactory;

        _vm.stopPrank();
    }

    function setUp() public {
        // Become sender
        _vm.startPrank(sender);

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
            _vm.prank(sender);
            _token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            _vm.startPrank(TEST_ACCOUNTS[i]);
            _token.approve(address(_superToken), INIT_TOKEN_BALANCE);
            _superToken.upgrade(INIT_TOKEN_BALANCE);
            _vm.stopPrank();
        }

        _vm.startPrank(sender);
        // initialize Simple ACL Close Resolver
        _simpleACLCloseResolver = new SimpleACLCloseResolver(
            block.timestamp + 14400,
            _cfa,
            _superToken,
            sender,
            receiver
        );

        // create ops mock contract
        ops = new OpsMock(address(_simpleACLCloseResolver), address(_host));

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
        // create a stream from sender to receiver
        _vm.startPrank(sender);
        sfFramework.cfaLib.createFlow(receiver, _superToken, 100);

        // fails because cannot execute yet
        _vm.expectRevert(OpsMock.CannotExecute.selector);

        ops.exec();
        _vm.stopPrank();
    }

    function testCannotCloseWithoutApproval() public {
        // create a stream from sender to receiver
        _vm.startPrank(sender);

        // move block.timestamp to 1 because it is currently at 0
        _vm.warp(block.timestamp + 1);

        sfFramework.cfaLib.createFlow(receiver, _superToken, 100);

        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14401);
        _vm.expectRevert(OpsMock.FailedExecution.selector);

        // still fail because of no flowOperator approval
        ops.exec();
    }

    function testCannotCloseBeforeEndTime() public {
        // create a stream from sender to receiver
        _vm.startPrank(sender);
        sfFramework.cfaLib.createFlow(receiver, _superToken, 100);

        // grant permissions so ops has full flow operator permissions
        _grantFlowOperatorPermissions(address(_superToken), address(ops));

        // fails because cannot execute yet
        _vm.expectRevert(OpsMock.CannotExecute.selector);

        ops.exec();
        _vm.stopPrank();
    }

    function testCannotCloseNonExistentFlow() public {
        _vm.startPrank(sender);

        // grant permissions so ops has full flow operator permissions
        _grantFlowOperatorPermissions(address(_superToken), address(ops));

        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14401);
        _vm.expectRevert(OpsMock.CannotExecute.selector);
        // try to close a stream that doesn't exist
        ops.exec();
        _vm.stopPrank();
    }

    // Resolver Tests

    function testCannotSetInvalidReceiver() public {
        _vm.startPrank(sender);
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowReceiver.selector);
        _simpleACLCloseResolver.updateFlowReceiver(address(0));
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowReceiver.selector);
        _simpleACLCloseResolver.updateFlowReceiver(sender);
    }

    function testCannotSetInvalidSender() public {
        _vm.startPrank(sender);
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowSender.selector);
        _simpleACLCloseResolver.updateFlowSender(address(0));
        _vm.expectRevert(SimpleACLCloseResolver.InvalidFlowSender.selector);
        _simpleACLCloseResolver.updateFlowSender(receiver);
    }

    /**
     * Passing Tests
     */

    // Ops Tests
    function testCanCloseAfterEndTime() public {
        // create a stream from sender to receiver
        _vm.startPrank(sender);

        // move block.timestamp to 1 because it is currently at 0
        _vm.warp(block.timestamp + 1);

        sfFramework.cfaLib.createFlow(receiver, _superToken, 100);

        // grant permissions so ops has delete flow operator permissions
        _grantFlowOperatorPermissions(address(_superToken), address(ops));

        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14400);

        ops.exec();
        _vm.stopPrank();
    }

    /**
     * Fuzz Tests
     */

    // Resolver Tests

    function testCanUpdateEndTime(uint256 _endTime, address _newOwner) public {
        _vm.warp(1);
        _vm.assume(_endTime > block.timestamp && _newOwner != address(0));

        _vm.prank(sender);
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
                _newOwner != address(0) &&
                _flowReceiver != sender
        );

        _vm.prank(sender);
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

        _vm.prank(sender);
        _simpleACLCloseResolver.transferOwnership(_newOwner);

        _vm.prank(_newOwner);
        _simpleACLCloseResolver.updateFlowSender(_flowSender);
    }

    function testCannotUpdateEndTimeToEarlier(uint256 _endTime) public {
        _vm.expectRevert(SimpleACLCloseResolver.InvalidEndTime.selector);
        _vm.warp(100000000);
        _vm.assume(_endTime < block.timestamp);
        _vm.startPrank(sender);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    function testNonOwnerCannotUpdateFlowSender(address _nonOwner) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != sender);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateFlowSender(_nonOwner);
    }

    function testNonOwnerCannotUpdateFlowReceiver(address _nonOwner) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != sender);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateFlowReceiver(_nonOwner);
    }

    function testNonOwnerCannotUpdateEndTime(
        address _nonOwner,
        uint256 _endTime
    ) public {
        _vm.expectRevert("Ownable: caller is not the owner");
        _vm.assume(_nonOwner != sender && _endTime > block.timestamp);
        _vm.prank(_nonOwner);
        _simpleACLCloseResolver.updateEndTime(_endTime);
    }

    /**************************************************************************
     * Helper functions
     *************************************************************************/

    function _grantFlowOperatorPermissions(
        address _flowSuperToken,
        address _flowOperator
    ) internal {
        _host.callAgreement(
            _cfa,
            abi.encodeWithSelector(
                _cfa.updateFlowOperatorPermissions.selector,
                _flowSuperToken,
                _flowOperator,
                4,
                0,
                new bytes(0)
            ),
            "0x"
        );
    }
}
