// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { DSTest } from "ds-test/test.sol";
import { Vm } from "forge-std/Vm.sol";
import { ERC20PresetMinterPauser } from "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";
import { SuperToken } from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import { SuperfluidFramework, Superfluid, ConstantFlowAgreementV1, SuperTokenFactory } from "./SuperfluidFramework.t.sol";
import { SuperfluidTester } from "./SuperfluidTester.t.sol";
import { StreamButler } from "../StreamButler.sol";
import { StreamButlerResolver } from "../StreamButlerResolver.sol";

// Only the owner or gelato ops can execute the functions with the isOwnerOrOps modifier
// Should not be able to close the flow unless you are the owner OR gelato ops and it is within the timeframe
// The contract should definitely have permissions to modify if permissions have been granted
// the last deleted index stuff should be working properly

contract StreamButlerTest is DSTest {
    Vm private _vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);

    uint256 private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address private constant alpha = address(1);
    address private constant admin = address(420);

    Superfluid private _host;
    ConstantFlowAgreementV1 private _cfa;
    SuperTokenFactory private _superTokenFactory;
    ERC20PresetMinterPauser private _token;
    SuperToken private _superToken;
    StreamButler private _streamButler;
    StreamButlerResolver private _streamButlerResolver; // ops
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

        // initialize Stream Butler
        _streamButler = new StreamButler(
            address(_host),
            address(_cfa),
            30 seconds
        );

        // mint tokens for accounts
        _token.mint(address(_adminTester), INIT_TOKEN_BALANCE);
        _token.mint(address(_alphaTester), INIT_TOKEN_BALANCE);

        // upgrade tokens for accounts\
        _adminTester.upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);
        _alphaTester.upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);

        // initialize Stream Butler Resolver
        _streamButlerResolver = new StreamButlerResolver(
            address(_streamButler)
        );

        // set stream butler resolver in _streamButler
        _streamButler.setOps(address(_streamButlerResolver));

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
        assertTrue(true);
    }
}
