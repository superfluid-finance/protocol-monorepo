// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { Test } from "forge-std/Test.sol";
import { Vm } from "forge-std/Vm.sol";
import { ERC20PresetMinterPauser } from "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";
import { SuperToken } from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { ISuperTokenFactory } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperTokenFactory.sol";
import { IInstantDistributionAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import { SuperfluidFramework, Superfluid, ConstantFlowAgreementV1, SuperTokenFactory } from "./SuperfluidFramework.t.sol";
import { SuperfluidTester } from "./SuperfluidTester.t.sol";
import { SimpleACLCloseResolver } from "../SimpleACLCloseResolver.sol";
import { OpsMock } from "../mocks/OpsMock.sol";

// Only the owner or gelato ops can execute the functions with the isOwnerOrOps modifier
// Should not be able to close the flow unless you are the owner OR gelato ops and it is within the timeframe
// The contract should definitely have permissions to modify if permissions have been granted
// the last deleted index stuff should be working properly

contract SimpleACLCloseResolverTest is Test {
    Vm private _vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

    uint256 private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address private constant alpha = address(1);
    address private constant npc = address(2);
    OpsMock private ops;
    address private constant admin = address(420);

    Superfluid private _host;
    ConstantFlowAgreementV1 private _cfa;
    SuperTokenFactory private _superTokenFactory;
    ERC20PresetMinterPauser private _token;
    ISuperToken private _superToken;
    SimpleACLCloseResolver private _simpleACLCloseResolver;
    SuperfluidTester private _alphaTester;
    SuperfluidTester private _adminTester;

    function setUp() public {
        // Deploy and retrieve contracts using `_vm` and `admin`. The admin deploys everything.
        (_host, _cfa, , _superTokenFactory) = new SuperfluidFramework(
            _vm,
            admin
        ).framework();

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
        // initialize Super ButlerToken

        // initialize SuperfluidTester accounts
        _adminTester = new SuperfluidTester(
            _host,
            _cfa,
            IInstantDistributionAgreementV1(address(0)), // don't care about IDA
            _token,
            _superToken
        );
        _alphaTester = new SuperfluidTester(
            _host,
            _cfa,
            IInstantDistributionAgreementV1(address(0)), // don't care about IDA
            _token,
            _superToken
        );

        // mint tokens for accounts
        _token.mint(address(_adminTester), INIT_TOKEN_BALANCE);
        _token.mint(address(_alphaTester), INIT_TOKEN_BALANCE);

        // upgrade tokens for accounts
        _adminTester.upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);
        _alphaTester.upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);

        // initialize Simple ACL Close Resolver
        _simpleACLCloseResolver = new SimpleACLCloseResolver(
            block.timestamp + 14400,
            _cfa,
            _superToken,
            address(_alphaTester)
        );

        // create ops mock contract
        ops = new OpsMock(address(_simpleACLCloseResolver), address(_host));

        _simpleACLCloseResolver.setOps(address(ops));

        // transfer ownership of the Simple ACL Close Resolver from
        // this test contract to the _admin
        _simpleACLCloseResolver.transferOwnership(address(_adminTester));

        _vm.stopPrank();
    }

    // expect revert if you try to create a stream via StreamButler prior to providing allowance
    // provide allowance
    // expect revert for all the functions which are called by someone other than the owner or ops" isOwnerOrOps

    function testCannotExecuteRightNow() public {
        // create a stream from _adminTester to npc
        _vm.startPrank(address(_adminTester));
        _adminTester.flow(npc, 100);
        _vm.expectRevert(OpsMock.CannotExecute.selector);
    
        ops.exec();
        _vm.stopPrank();
    }

    function testCannotCloseWithoutApproval() public {
        // create a stream from _adminTester to npc
        _vm.startPrank(address(_adminTester));
        _adminTester.flow(npc, 100);
        
        // warp to a time when it's acceptable to execute
        _vm.warp(block.timestamp + 14401);
        _vm.expectRevert(OpsMock.FailedExecution.selector);

        // still fail because of no flowOperator approval
        ops.exec();
    }
}
