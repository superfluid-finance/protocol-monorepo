// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {
    FoundrySuperfluidTester,
    SuperTokenV1Library
} from "../FoundrySuperfluidTester.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { TOGA } from "../../../contracts/utils/TOGA.sol";
import {
    IERC1820Registry
} from "@openzeppelin/contracts/interfaces/IERC1820Registry.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

/**
 * @title TOGATest
 * @dev A contract for testing the functionality of the TOGA contract.
 */
contract TOGATest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    TOGA internal toga;

    uint256 internal constant MIN_BOND_DURATION = 3600 * 24 * 7; // 7 days
    uint256 internal constant BOND_AMOUNT_1E18 = 1e18;
    uint256 internal constant BOND_AMOUNT_2E18 = 2e18;
    uint256 internal constant BOND_AMOUNT_10E18 = 10e18;
    int96 internal constant EXIT_RATE_1 = 1;
    int96 internal constant EXIT_RATE_1E3 = 1e3;
    int96 internal constant EXIT_RATE_1E6 = 1e6;
    IERC1820Registry internal constant _ERC1820_REG =
        IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);

    constructor() FoundrySuperfluidTester(5) {}

    /**
     * @dev Sets up the contract for testing.
     */
    function setUp() public override {
        super.setUp();
        toga = new TOGA(sf.host, MIN_BOND_DURATION);
    }

    // Helper

    /**
     * @dev Checks the net flow of an asset for an account.
     * @param token The Super Token representing the asset.
     * @param account The address of the account to check.
     * @param expectedNetFlow The expected net flow.
     */
    function _assetNetflow(
        ISuperToken token,
        address account,
        int96 expectedNetFlow
    ) internal {
        int96 flowRate = sf.cfa.getNetFlow(token, account);
        assertTrue(flowRate == expectedNetFlow);
    }

    function _deleteFlow(
        ISuperToken token,
        address sender,
        address receiver
    ) internal {
        vm.startPrank(sender);
        token.deleteFlow(sender, receiver);
        vm.stopPrank();
    }

    function _startStream(
        address sender,
        address receiver,
        int96 flowRate
    ) internal {
        vm.startPrank(sender);
        superToken.createFlow(receiver, flowRate);
        vm.stopPrank();
    }

    /**
     * @dev Sends a PIC bid.
     * @param sender The address of the sender.
     * @param token The Super Token representing the asset.
     * @param bond The bond amount.
     * @param exitRate The exit rate.
     */
    function _sendPICBid(
        address sender,
        ISuperToken token,
        uint256 bond,
        int96 exitRate
    ) internal {
        vm.startPrank(sender);
        token.send(address(toga), bond, abi.encode(exitRate));
        vm.stopPrank();
    }

    function _sendPICBid(
        address sender,
        ISuperToken token,
        uint256 bond,
        bytes memory exitRate
    ) internal {
        vm.startPrank(sender);
        token.send(address(toga), bond, exitRate);
        vm.stopPrank();
    }

    function _shouldDefaultExitRate(
        uint256 bondAmount
    ) internal pure returns (int96) {
        return int96(int256(bondAmount / (MIN_BOND_DURATION * 4)));
    }

    function _shouldMaxExitRate(
        uint256 bondAmount
    ) internal pure returns (int96) {
        return int96(int256(bondAmount / MIN_BOND_DURATION));
    }

    function _collectRewards(
        ISuperToken token,
        int96 flowRate,
        uint256 time
    ) internal {
        _startStream(admin, address(toga), flowRate);
        vm.warp(block.timestamp + time);
        vm.startPrank(admin);
        token.deleteFlow(admin, address(toga));
        vm.stopPrank();
    }

    // test

    /**
     * @dev Tests the contract setup.
     */
    function testContractSetup() public {
        assertTrue(
            toga.minBondDuration() == MIN_BOND_DURATION,
            "minBondDuration"
        );
    }

    /**
     * @dev Tests that Alice becomes the PIC.
     */
    function testAliceBecomesPIC() public {
        assertTrue(address(0) == toga.getCurrentPIC(superToken));

        vm.startPrank(alice);
        superToken.send(
            address(toga),
            BOND_AMOUNT_1E18,
            abi.encode(EXIT_RATE_1)
        );

        assertTrue(toga.getCurrentPIC(superToken) == alice);

        (address pic, uint256 bond, int96 exitRate) = toga.getCurrentPICInfo(
            superToken
        );

        assertTrue(pic == alice);
        assertTrue(bond == BOND_AMOUNT_1E18);
        assertTrue(exitRate == EXIT_RATE_1);

        vm.stopPrank();

        _assetNetflow(superToken, alice, EXIT_RATE_1);
    }

    /**
     * @dev Tests that Bob can outbid Alice with a higher bond.
     */
    function testBobOutBidsAlice() public {
        // Send PIC bid from Alice
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);

        vm.startPrank(alice);

        // Assert Alice is the current PIC
        assertTrue(toga.getCurrentPIC(superToken) == alice);

        // Get the bond amount
        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond == BOND_AMOUNT_1E18);

        vm.stopPrank();

        // Fail with the same amount (needs to be strictly greater)
        // Fails to trigger if the timestamp is 1s off (happens sometimes randomly)
        // This detracting 1s exit flowrate
        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(bob, superToken, BOND_AMOUNT_1E18 - 1, EXIT_RATE_1);

        _sendPICBid(bob, superToken, BOND_AMOUNT_1E18 + 1, EXIT_RATE_1);

        // Assert Bob becomes the current PIC
        assertTrue(toga.getCurrentPIC(superToken) == bob);

        // Assert the bond amount for Bob
        (, uint256 bond2, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond2 == BOND_AMOUNT_1E18 + 1);
    }

    function testTOGARegisteredWithERC1820() public {
        address implementer1 = _ERC1820_REG.getInterfaceImplementer(
            address(toga),
            keccak256("TOGAv1")
        );
        address implementer2 = _ERC1820_REG.getInterfaceImplementer(
            address(toga),
            keccak256("TOGAv2")
        );

        assertTrue(
            implementer1 == address(toga),
            "TOGA should be registered as TOGAv1"
        );
        assertTrue(
            implementer2 == address(toga),
            "TOGA should be registered as TOGAv2"
        );
    }

    function testEnforceMinExitRateLimit() public {
        // lower limit: 0 wei/second (no negative value allowed)
        vm.expectRevert("TOGA: negative exitRate not allowed");
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, -1);

        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);
        _assetNetflow(superToken, alice, 0);
    }

    function testEnforceMaxExitRateLimit() public {
        // upper limit: 1 wei/second
        vm.expectRevert("TOGA: exitRate too high");
        _sendPICBid(
            alice,
            superToken,
            BOND_AMOUNT_1E18,
            _shouldMaxExitRate(BOND_AMOUNT_1E18) + 1
        );

        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 1);
        _assetNetflow(superToken, alice, 1);
    }

    function testMaxAndDefaultExitRateAreCalculatedCorrectly() public {
        assertTrue(
            _shouldMaxExitRate(BOND_AMOUNT_1E18) ==
                toga.getMaxExitRateFor(superToken, BOND_AMOUNT_1E18)
        );
        assertTrue(
            _shouldMaxExitRate(BOND_AMOUNT_2E18) ==
                toga.getMaxExitRateFor(superToken, BOND_AMOUNT_2E18)
        );
        assertTrue(
            _shouldDefaultExitRate(BOND_AMOUNT_1E18) ==
                toga.getDefaultExitRateFor(superToken, BOND_AMOUNT_1E18)
        );
        assertTrue(
            _shouldDefaultExitRate(BOND_AMOUNT_2E18) ==
                toga.getDefaultExitRateFor(superToken, BOND_AMOUNT_2E18)
        );
        // the default exit rate needs to be equal or greater than the max exit rate
        assertTrue(
            toga.getMaxExitRateFor(superToken, BOND_AMOUNT_2E18) >=
                toga.getDefaultExitRateFor(superToken, BOND_AMOUNT_2E18)
        );
    }

    function testUseDefaultExitRateAsFallbackIfNoExitRateSpecified() public {
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, abi.encode());
        _assetNetflow(
            superToken,
            alice,
            toga.getDefaultExitRateFor(superToken, BOND_AMOUNT_1E18)
        );
    }

    function testCannotBecomePICWithBidSmallerThanCurrentPICBond() public {
        _sendPICBid(alice, superToken, BOND_AMOUNT_2E18, 0);

        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(bob, superToken, BOND_AMOUNT_1E18, 0);
    }

    function testFirstBidderGetsTokensPreOwnedByContract() public {
        _collectRewards(superToken, 1e6, MIN_BOND_DURATION);

        uint256 togaPrelimBal = superToken.balanceOf(address(toga));
        _sendPICBid(alice, superToken, BOND_AMOUNT_2E18, 0);
        (, uint256 aliceBond, ) = toga.getCurrentPICInfo(superToken);

        // the tokens previously collected in the contract are attributed to Alice's bond
        assertTrue(aliceBond == (togaPrelimBal + BOND_AMOUNT_2E18));

        uint256 alicePreOutbidBal = superToken.balanceOf(alice);
        _sendPICBid(bob, superToken, BOND_AMOUNT_10E18, 0);

        // the tokens previously collected are paid out to Alice if outbid
        assertTrue(
            superToken.balanceOf(alice) == (alicePreOutbidBal + aliceBond)
        );
    }

    function testCurrentPICCanIncreaseBond() public {
        _sendPICBid(alice, superToken, BOND_AMOUNT_2E18, 0);
        uint256 aliceIntermediateBal = superToken.balanceOf(alice);
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);

        assertTrue(toga.getCurrentPIC(superToken) == alice);
        _assetNetflow(superToken, alice, 0);
        assertTrue(
            superToken.balanceOf(alice) ==
                (aliceIntermediateBal - BOND_AMOUNT_1E18)
        );
        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond == BOND_AMOUNT_2E18 + BOND_AMOUNT_1E18);
    }

    function testPICCanChangeExitRate() public {
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, abi.encode());
        vm.expectRevert("TOGA: only PIC allowed");
        toga.changeExitRate(superToken, EXIT_RATE_1);
        vm.startPrank(alice);

        // don't allow negative exitRate
        vm.expectRevert("TOGA: negative exitRate not allowed");
        toga.changeExitRate(superToken, -1);

        // lower to 1 wad
        toga.changeExitRate(superToken, EXIT_RATE_1);
        _assetNetflow(superToken, alice, EXIT_RATE_1);

        // increase to 1000 wad
        toga.changeExitRate(superToken, EXIT_RATE_1E3);
        _assetNetflow(superToken, alice, EXIT_RATE_1E3);

        // to 0
        toga.changeExitRate(superToken, 0);
        _assetNetflow(superToken, alice, 0);

        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        int96 max1 = _shouldMaxExitRate(bond);
        toga.changeExitRate(superToken, max1);
        _assetNetflow(superToken, alice, max1);
        int96 max2 = _shouldMaxExitRate(bond);
        assertTrue(max1 == max2);
        vm.expectRevert("TOGA: exitRate too high");
        toga.changeExitRate(superToken, max2 + 1);
    }

    function testPICClosesSteam() public {
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        _assetNetflow(superToken, alice, EXIT_RATE_1E3);

        vm.warp(block.timestamp + 1000);
        _deleteFlow(superToken, address(toga), alice);
        _assetNetflow(superToken, alice, 0);

        vm.startPrank(alice);
        toga.changeExitRate(superToken, 0);
        _assetNetflow(superToken, alice, 0);

        toga.changeExitRate(superToken, EXIT_RATE_1);
        _assetNetflow(superToken, alice, EXIT_RATE_1);

        vm.stopPrank();

        // stop again and let bob make a bid
        vm.warp(block.timestamp + 1000);
        _deleteFlow(superToken, address(toga), alice);
        _assetNetflow(superToken, alice, 0);

        _sendPICBid(bob, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        _assetNetflow(superToken, alice, 0);
        _assetNetflow(superToken, bob, EXIT_RATE_1E3);
    }

    function testCollectedRewardsAreAddedToThePICBond() public {
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, 0);

        _collectRewards(superToken, 1e6, 1e6);
        // 1e6 token/s x 1e6 seconds = ~1e12 tokens collected in the contract

        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond == BOND_AMOUNT_1E18 + 1e12);
    }

    // TODO: Will fail cause of _liquidateExitStream(superToken)
    function testBondIsConsumedByExitFlow() public {
        int96 maxRate = _shouldMaxExitRate(BOND_AMOUNT_1E18);
        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, maxRate);
        _assetNetflow(superToken, alice, maxRate);

        // critical stream is liquidated - remaining bond goes to zero
        vm.warp(block.timestamp + 1e6);

        // TODO: this is not working
        // _liquidateExitStream(superToken); // a sentinel would do this

        _assetNetflow(superToken, alice, 0);
        _assetNetflow(superToken, address(toga), 0);

        // this assumes the flow deletion was not triggered by the PIC - otherwise rewards would be accrued
        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond == 0);

        // alice tries to re-establish stream - fail because no bond left
        vm.expectRevert("TOGA: exitRate too high");
        toga.changeExitRate(superToken, EXIT_RATE_1);

        // after some more rewards being collected, alice can re-establish the exit stream
        _collectRewards(superToken, EXIT_RATE_1E3, 1e3);
        (, uint256 bond2, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond2 >= 1e12);

        toga.changeExitRate(superToken, EXIT_RATE_1E3);
        _assetNetflow(superToken, alice, EXIT_RATE_1E3);

        uint256 alicePreBal = superToken.balanceOf(alice);
        (, uint256 aliceBondLeft, ) = toga.getCurrentPICInfo(superToken);

        // bob outbids
        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(bob, superToken, 1, abi.encode());
        _sendPICBid(bob, superToken, BOND_AMOUNT_2E18, EXIT_RATE_1E6);
        _assetNetflow(superToken, alice, 0);
        _assetNetflow(superToken, bob, EXIT_RATE_1E6);

        assertTrue(
            (alicePreBal + aliceBondLeft) == superToken.balanceOf(alice)
        );
    }

    // TODO: Will fail cause of superToken2, think of a better way
    function testMultiplePICsInParallel() public {
        (IERC20 token2, ISuperToken superToken2) = sfDeployer
            .deployWrapperSuperToken("TEST2", "TEST2", 18, type(uint256).max);

        vm.startPrank(bob);
        token.mint(bob, INIT_SUPER_TOKEN_BALANCE);
        token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
        superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
        vm.stopPrank();

        vm.startPrank(alice);
        token.mint(alice, INIT_SUPER_TOKEN_BALANCE);
        token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
        superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
        vm.stopPrank();

        _sendPICBid(alice, superToken, BOND_AMOUNT_1E18, EXIT_RATE_1E3);
        _sendPICBid(bob, superToken2, BOND_AMOUNT_2E18, EXIT_RATE_1);

        assertTrue(toga.getCurrentPIC(superToken) == alice);
        assertTrue(toga.getCurrentPIC(superToken2) == bob);

        vm.expectRevert("TOGA: only PIC allowed");
        toga.changeExitRate(superToken2, 0);

        vm.expectRevert("TOGA: only PIC allowed");
        toga.changeExitRate(superToken, 0);

        // let this run for a while...
        vm.warp(block.timestamp + 1e6);

        // alice takes over superToken2
        uint256 bobPreBal = superToken2.balanceOf(bob);
        (, uint256 bobBondLeft, ) = toga.getCurrentPICInfo(superToken2);

        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(alice, superToken2, BOND_AMOUNT_1E18, EXIT_RATE_1);

        _sendPICBid(alice, superToken2, BOND_AMOUNT_10E18, EXIT_RATE_1E3);

        assertTrue((bobPreBal + bobBondLeft) == superToken2.balanceOf(bob));
    }

    function testFuzzBondAndExitRate(uint256 bond, int96 exitRate) public {
        vm.assume(bond > 0);
        vm.assume(bond < uint256(type(int256).max)); // SuperToken doesn't support the full uint256 range
        // with small bonds, opening the stream can fail due to CFA deposit having a flow of 1<<32 due to clipping
        vm.assume(bond > 1<<32 || exitRate == 0);

        vm.assume(exitRate >= 0);
        // satisfy exitRate constraints of the TOGA
        vm.assume(exitRate <= toga.getMaxExitRateFor(superToken, bond));
        // the clipped CFA deposit needs to fit into 64 bits - since that is flowrate multiplied by
        // liquidation perdion, 14 bits are added for 14400 seconds, so we can't use the full 96 bits
        vm.assume(exitRate <= (type(int96).max) >> 14);

        deal(address(superToken), alice, uint256(type(int256).max));

        _sendPICBid(alice, superToken, bond, exitRate);
    }
}
