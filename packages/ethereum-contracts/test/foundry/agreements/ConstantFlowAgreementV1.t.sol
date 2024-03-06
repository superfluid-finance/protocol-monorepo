// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

import { console } from "forge-std/Test.sol";
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
    function testNFTHookOutOfGasRevertsWholeTx(uint256 gasLimit) public {
        gasLimit = bound(gasLimit, 350000, 550000);

        console.log("trying createFlow...");
        int96 fr = 1;
        try this.__external_createFlow{gas: gasLimit}(superToken, alice, bob, fr) {
            // if the tx does not revert, the NFT hook isn't allowed to revert with outofgas,
            // which we can check by verifying the FlowNFT state
            _assertFlowNftState(superToken, alice, bob, fr);
        } catch { } // revert of the tx is ok

        console.log("trying updateFlow...");
        fr = 2;
        try this.__external_updateFlow{gas: gasLimit}(superToken, alice, bob, fr) {
            _assertFlowNftState(superToken, alice, bob, fr);
        } catch { }

        console.log("trying deleteFlow...");
        try this.__external_deleteFlow{gas: gasLimit}(superToken, alice, bob) {
            _assertFlowNftState(superToken, alice, bob, 0);
        } catch { }
    }

    // helper functions wrapping internal calls into external calls (needed for try/catch)

    function __external_createFlow(ISuperToken superToken, address sender, address receiver, int96 fr) external {
        vm.startPrank(sender);
        superToken.createFlow(receiver, fr);
        vm.stopPrank();
    }

    function __external_updateFlow(ISuperToken superToken, address sender, address receiver, int96 fr) external {
        vm.startPrank(sender);
        superToken.updateFlow(receiver, fr);
        vm.stopPrank();
    }

    function __external_deleteFlow(ISuperToken superToken, address sender, address receiver) external {
        vm.startPrank(sender);
        superToken.deleteFlow(sender, receiver);
        vm.stopPrank();
    }
}
