// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

import { console, console2 } from "forge-std/Test.sol";

import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";

contract ConstantFlowAgreementV1IntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    constructor() FoundrySuperfluidTester(3) { }

    function testAlice2Bob(int96 flowRate) public {
        _helperCreateFlow(superToken, alice, bob, flowRate);

        _warpAndAssertAll(superToken);
    }

    function testBobAliceLoop(int96 flowRate) public {
        _helperCreateFlow(superToken, alice, bob, flowRate);

        _warpAndAssertAll(superToken);

        _helperCreateFlow(superToken, bob, alice, flowRate);

        _warpAndAssertAll(superToken);
    }

    // there should be no gas limit which causes the NFT hook to fail with the tx succeeding
    function testNFTHookOutOfGas(uint256 gasLimit) public {
        gasLimit = bound(gasLimit, 400000, 500000);
        //uint256 gasLimit = 462500;
        //_helperCreateFlow(superToken, alice, bob, 1);
        /*try this.__external_helperCreateFlow{gas: 3000000}(superToken, alice, bob, 1) {
            // the NFT must exist
        } catch {
            uint256 gasConsumed = gasLeftBefore - gasleft();
            console.log("gas consumed", gasConsumed);
            assertTrue(gasConsumed > gasLimit || gasLimit - gasConsumed <= gasLimit / 64, "reverted for other reason than out of gas");
        }*/

        int96 fr = 1;
        uint256 gasLeftBefore = gasleft();
        try this.__external_createFlow{gas: gasLimit}(superToken, alice, bob, fr) {
            _assertFlowNftState(superToken, alice, bob, fr);
        } catch {
            console.log("call reverted, gas consumed", gasLeftBefore - gasleft());
        }
    }


    function __external_helperCreateFlow(ISuperToken superToken_, address sender, address receiver, int96 flowRate) external {
        _helperCreateFlow(superToken_, sender, receiver, flowRate);
    }

    function __external_createFlow(ISuperToken superToken, address sender, address receiver, int96 /*fr*/) external {
        vm.startPrank(sender);
        superToken.createFlow(receiver, 1);
        vm.stopPrank();
    }
}
