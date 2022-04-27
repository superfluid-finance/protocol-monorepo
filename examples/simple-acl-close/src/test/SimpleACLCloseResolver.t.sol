// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { Test } from "forge-std/Test.sol";
import { Vm } from "forge-std/Vm.sol";
// import { console } from "forge-std/console.sol";
import { ERC20PresetMinterPauser } from "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";
import { SuperToken } from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import {IInstantDistributionAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import { SuperfluidFramework, Superfluid, ConstantFlowAgreementV1, SuperTokenFactory } from "./SuperfluidFramework.t.sol";
import { SuperfluidTester } from "./SuperfluidTester.t.sol";
import { SimpleACLCloseResolver } from "../SimpleACLCloseResolver.sol";

// Only the owner or gelato ops can execute the functions with the isOwnerOrOps modifier
// Should not be able to close the flow unless you are the owner OR gelato ops and it is within the timeframe
// The contract should definitely have permissions to modify if permissions have been granted
// the last deleted index stuff should be working properly

contract SimpleACLCloseResolverTest is Test {
    Vm private _vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

    uint256 private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address private constant alpha = address(1);
    address private constant ops = address(69);
    address private constant admin = address(420);

    Superfluid private _host;
    ConstantFlowAgreementV1 private _cfa;
    SuperTokenFactory private _superTokenFactory;
    ERC20PresetMinterPauser private _token;
    SuperToken private _superToken;
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

        // Deploy Super ButlerToken
        _superToken = new SuperToken(_host);

        // initialize underlying token
        _token = new ERC20PresetMinterPauser("Butler Token", "BUT");

        // initialize Super ButlerToken
        _superToken.initialize(_token, 18, "Butler Token", "BUTx");

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

        // upgrade tokens for accounts\
        _adminTester.upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);
        _alphaTester.upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);

        // initialize Simple ACL Close Resolver
        _simpleACLCloseResolver = new SimpleACLCloseResolver(
            block.timestamp + 14400,
            _cfa,
            _superToken,
            address(_alphaTester)
        );

        // transfer ownership of the Simple ACL Close Resolver from
        // this test contract to the _admin
        _simpleACLCloseResolver.transferOwnership(address(_adminTester));

        _vm.stopPrank();
    }

    // expect revert if you try to create a stream via StreamButler prior to providing allowance
    // provide allowance
    // expect revert for all the functions which are called by someone other than the owner or ops" isOwnerOrOps
    // try to query 
    // expect revert for trying to set an end time which is before the current block.timestamp
    // expect revert for trying to createButlerFlowData as non-owner
    // expect revert for trying to createButlerFlowData where the flow does not exist
    // expect revert for trying to createButlerFlowData when it already exists
    // should be able to serve one flow as owner
    // should be able to update one flow as owner
    // should be able to delete one flow as owner

    function testExample() public {
        assertTrue(false);
    }
}
