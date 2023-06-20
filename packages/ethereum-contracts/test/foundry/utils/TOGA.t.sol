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

    // Events

    event NewPIC(ISuperToken indexed token, address pic, uint256 bond, int96 exitRate);
    event BondIncreased(ISuperToken indexed token, uint256 additionalBond);
    event ExitRateChanged(ISuperToken indexed token, int96 exitRate);

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

    function _emptyAccount(
        address account,
        ISuperToken token
    ) internal {
        vm.startPrank(account);
        token.transfer(admin, token.balanceOf(account));
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

    function _transferToken(
        IERC20 token,
        address sender,
        address receiver,
        uint256 amount
    ) internal {
        vm.startPrank(sender);
        token.transfer(receiver, amount);
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
        return capToInt96(int256(bondAmount / (MIN_BOND_DURATION * 4)));
    }

    function capToInt96(int256 value) internal pure returns(int96) {
        return value < type(int96).max ? int96(value) : type(int96).max;
    }

    function _shouldMaxExitRate(
        uint256 bondAmount
    ) internal pure returns (int96) {
        return capToInt96(int256(bondAmount / MIN_BOND_DURATION));
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
    function testAliceBecomesPIC(uint256 bond, int96 exitRate) public {
        assertTrue(address(0) == toga.getCurrentPIC(superToken));

        vm.assume(bond > 0);
        vm.assume(bond < uint256(type(uint64).max)); // Test token only contains 64 bits

        // with small bonds, opening the stream can fail due to CFA deposit having a flow of 1<<32 due to clipping
         vm.assume(bond > 1<<32 || exitRate == 0);

         vm.assume(exitRate >= 0);
         // satisfy exitRate constraints of the TOGA
         vm.assume(exitRate <= toga.getMaxExitRateFor(superToken, bond));
         // the clipped CFA deposit needs to fit into 64 bits - since that is flowrate multiplied by
         // liquidation perdion, 14 bits are added for 14400 seconds, so we can't use the full 96 bits
         vm.assume(exitRate <= (type(int96).max) >> 14);

        vm.expectEmit(true, true, true, true, address(toga));
        emit NewPIC(superToken, alice, bond, exitRate);

        _sendPICBid(alice, superToken, bond, exitRate);

        assertTrue(toga.getCurrentPIC(superToken) == alice);

        (address pic, uint256 bond, int96 exitRatePIC) = toga.getCurrentPICInfo(
            superToken
        );

        assertTrue(pic == alice);
        assertTrue(bond == bond);
        assertTrue(exitRatePIC == exitRate);

        _assetNetflow(superToken, alice, exitRate);
    }

    /**
     * @dev Tests that Bob can outbid Alice with a higher bond.
     */
    function testBobOutBidsAlice(uint256 bobBond, uint256 aliceBond) public {
        vm.assume(aliceBond > 0);
        vm.assume(bobBond > aliceBond);
        vm.assume(bobBond < uint256(type(uint64).max)); // User only has 64 bits test super tokens

        // Send PIC bid from Alice
        _sendPICBid(alice, superToken, aliceBond, 0);

        vm.startPrank(alice);

        // Assert Alice is the current PIC
        assertTrue(toga.getCurrentPIC(superToken) == alice);

        // Get the bond amount
        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond == aliceBond);

        vm.stopPrank();

        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(bob, superToken, aliceBond - 1, 0);

        vm.expectEmit(true, true, true, true, address(toga));
        emit NewPIC(superToken, bob, bobBond, 0);

        _sendPICBid(bob, superToken, bobBond, 0);

        // Assert Bob becomes the current PIC
        assertTrue(toga.getCurrentPIC(superToken) == bob);

        // Assert the bond amount for Bob
        (, uint256 bond2, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond2 == bobBond);
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

    function testEnforceMaxExitRateLimit(uint256 bond, int96 exitRate) public {
        vm.assume(bond > 0);
        vm.assume(bond < uint256(type(uint64).max)); // User only has 64 bits test super tokens

        // with small bonds, opening the stream can fail due to CFA deposit having a flow of 1<<32 due to clipping
         vm.assume(bond > 1<<32 || exitRate == 0);
        vm.assume(exitRate >= 0);
        // satisfy maxExitRate constraints of the TOGA
        vm.assume(exitRate == toga.getMaxExitRateFor(superToken, bond));

        // upper limit: 1 wei/second
        vm.expectRevert("TOGA: exitRate too high");
        _sendPICBid(
            alice,
            superToken,
            BOND_AMOUNT_1E18,
            _shouldMaxExitRate(BOND_AMOUNT_1E18) + 1
        );

        _sendPICBid(alice, superToken, bond, exitRate);
        _assetNetflow(superToken, alice, exitRate);
    }

    function testMaxAndDefaultExitRateAreCalculatedCorrectly(uint256 bond) public {
        vm.assume(bond > 0);
        vm.assume(bond < uint256(type(uint64).max)); // User only has 64 bits test super tokens

        assertTrue(
            _shouldMaxExitRate(bond) ==
                toga.getMaxExitRateFor(superToken, bond)
        );
        assertTrue(
            _shouldDefaultExitRate(bond) ==
                toga.getDefaultExitRateFor(superToken, bond)
        );
        // the default exit rate needs to be equal or greater than the max exit rate
        assertTrue(
            toga.getMaxExitRateFor(superToken, bond) >=
                toga.getDefaultExitRateFor(superToken, bond)
        );
    }

    function testUseDefaultExitRateAsFallbackIfNoExitRateSpecified(uint256 bond) public {
        vm.assume(bond > 0);
        vm.assume(bond < INIT_SUPER_TOKEN_BALANCE); // User only has 64 bits test super tokens
        vm.assume(bond > 1<<32);

        _sendPICBid(alice, superToken, bond, abi.encode());
        _assetNetflow(
            superToken,
            alice,
            toga.getDefaultExitRateFor(superToken, bond)
        );
    }

    function testCannotBecomePICWithBidSmallerThanCurrentPICBond(uint256 bond, uint256 smallerBond) public {
        vm.assume(bond > 0);
        vm.assume(bond < uint256(type(uint64).max)); // User only has 64 bits test super tokens
        vm.assume(smallerBond > 0);
        vm.assume(smallerBond < bond); // User only has 64 bits test super tokens
        _sendPICBid(alice, superToken, bond, 0);

        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(bob, superToken, smallerBond, 0);
    }

    function testFirstBidderGetsTokensPreOwnedByContract(uint256 bond, uint256 outBidBond) public {
        vm.assume(bond > 0);
        vm.assume(bond > 1<<32);
        vm.assume(bond < INIT_SUPER_TOKEN_BALANCE / 2); // User only has 64 bits test super tokens

        deal(address(superToken), address(toga), 1e6);

        uint256 togaPrelimBal = superToken.balanceOf(address(toga));
        _sendPICBid(alice, superToken, bond, 0);
        (, uint256 aliceBond, ) = toga.getCurrentPICInfo(superToken);

        // the tokens previously collected in the contract are attributed to Alice's bond
        assertTrue(aliceBond == (togaPrelimBal + bond));

        vm.assume(outBidBond > aliceBond);
        vm.assume(outBidBond < INIT_SUPER_TOKEN_BALANCE); // User only has 64 bits test super tokens
        vm.assume(outBidBond > 1<<32);

        uint256 alicePreOutbidBal = superToken.balanceOf(alice);
        _sendPICBid(bob, superToken, outBidBond, 0);

        // the tokens previously collected are paid out to Alice if outbid
        assertTrue(
            superToken.balanceOf(alice) == (alicePreOutbidBal + aliceBond)
        );
    }

    function testCurrentPICCanIncreaseBond(uint256 bond, uint256 increaseBond) public {
        vm.assume(bond > 0);
        vm.assume(bond < INIT_SUPER_TOKEN_BALANCE / 2); // User only has 64 bits test super tokens
        vm.assume(bond > 1<<32);

        _sendPICBid(alice, superToken, bond, 0);
        uint256 aliceIntermediateBal = superToken.balanceOf(alice);
        vm.assume(increaseBond > 0);
        vm.assume(increaseBond < aliceIntermediateBal);
        vm.assume(increaseBond > 1<<32);

        vm.expectEmit(true, true, false, false, address(toga));
        emit BondIncreased(superToken, increaseBond);

        _sendPICBid(alice, superToken, increaseBond, 0);

        assertTrue(toga.getCurrentPIC(superToken) == alice);
        _assetNetflow(superToken, alice, 0);
        assertTrue(
            superToken.balanceOf(alice) ==
                (aliceIntermediateBal - increaseBond)
        );
        (, uint256 picBond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(picBond == bond + increaseBond);
    }

    function testPICCanChangeExitRate(int96 exitRate, int96 changeExitRate) public {
        uint256 bond = 1 ether;

        vm.assume(exitRate >= 0);
        // satisfy maxExitRate constraints of the TOGA
        vm.assume(exitRate <= toga.getMaxExitRateFor(superToken, bond));

        vm.assume(changeExitRate >= 0);
        vm.assume(changeExitRate <= toga.getMaxExitRateFor(superToken, bond));

        _sendPICBid(alice, superToken, bond, abi.encode());

        vm.expectRevert("TOGA: only PIC allowed");
        toga.changeExitRate(superToken, exitRate);
        vm.startPrank(alice);

        // don't allow negative exitRate
        vm.expectRevert("TOGA: negative exitRate not allowed");
        toga.changeExitRate(superToken, -1);

        vm.expectEmit(true, true, false, false, address(toga));
        emit ExitRateChanged(superToken, changeExitRate);
        
        toga.changeExitRate(superToken, changeExitRate);
        _assetNetflow(superToken, alice, changeExitRate);
    }

    function testPICClosesSteam(uint256 bond) public {
        vm.assume(bond > 0);
        vm.assume(bond < INIT_SUPER_TOKEN_BALANCE); // User only has 64 bits test super tokens
        vm.assume(bond > 1<<32);

        _sendPICBid(alice, superToken, bond, EXIT_RATE_1E3);
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

        _sendPICBid(bob, superToken, bond, EXIT_RATE_1E3);
        _assetNetflow(superToken, alice, 0);
        _assetNetflow(superToken, bob, EXIT_RATE_1E3);
    }

    function testCollectedRewardsAreAddedToThePICBond(uint256 bond, uint256 rewards) public {
        vm.assume(bond > 0);
        vm.assume(bond < INIT_SUPER_TOKEN_BALANCE); // User only has 64 bits test super tokens
        vm.assume(bond > 1<<32);
        vm.assume(rewards > 0);
        vm.assume(rewards < INIT_SUPER_TOKEN_BALANCE);

        _sendPICBid(alice, superToken, bond, 0);

        _transferToken(superToken, admin, address(toga), rewards);

        (, uint256 picBond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(picBond == bond + rewards);
    }

    function testBondIsConsumedByExitFlow(uint256 aliceBond) public {
        vm.assume(aliceBond > 0);
        vm.assume(aliceBond < INIT_SUPER_TOKEN_BALANCE / 2); // User only has type(uint64).max test super tokens
        vm.assume(aliceBond > 1<<32);

        int96 maxRate = _shouldMaxExitRate(aliceBond);
        _sendPICBid(alice, superToken, aliceBond, maxRate);
        _assetNetflow(superToken, alice, maxRate);

        // critical stream is liquidated - remaining bond goes to zero
        vm.warp(block.timestamp + 1e6);

        // A sentinel would do this
        vm.startPrank(admin);
        superToken.deleteFlow(address(toga), alice);
        vm.stopPrank();

        _assetNetflow(superToken, alice, 0);
        _assetNetflow(superToken, address(toga), 0);

        // this assumes the flow deletion was not triggered by the PIC - otherwise rewards would be accrued
        (, uint256 bond, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond == 0);

        // alice tries to re-establish stream - fail because no bond left
        vm.startPrank(alice);
        vm.expectRevert("TOGA: exitRate too high");
        toga.changeExitRate(superToken, _shouldDefaultExitRate(aliceBond));
        vm.stopPrank();

        deal(address(superToken), address(toga), 1e15);

        (, uint256 bond2, ) = toga.getCurrentPICInfo(superToken);
        assertTrue(bond2 >= 1e12);

        vm.startPrank(alice);
        toga.changeExitRate(superToken, _shouldMaxExitRate(bond2));
        vm.stopPrank();
        _assetNetflow(superToken, alice, _shouldMaxExitRate(bond2));


        uint256 alicePreBal = superToken.balanceOf(alice);
        (, uint256 aliceBondLeft, ) = toga.getCurrentPICInfo(superToken);

        // bob outbids
        vm.expectRevert("TOGA: bid too low");
        _sendPICBid(bob, superToken, aliceBondLeft - 1, abi.encode());
        _sendPICBid(bob, superToken, aliceBondLeft + 1, _shouldMaxExitRate(aliceBondLeft + 1));
        _assetNetflow(superToken, alice, 0);
        _assetNetflow(superToken, bob, _shouldMaxExitRate(aliceBondLeft + 1));

        assertTrue(
            (alicePreBal + aliceBondLeft) == superToken.balanceOf(alice)
        );
    }

    function testMultiplePICsInParallel(uint256 bond) public {
        vm.assume(bond > 0);
        vm.assume(bond < INIT_SUPER_TOKEN_BALANCE); // User only has 64 bits test super tokens
        vm.assume(bond > 1<<32);

        (IERC20 token2, ISuperToken superToken2) = sfDeployer
            .deployWrapperSuperToken("TEST2", "TEST2", 18, type(uint256).max);

        deal(address(superToken2), alice, INIT_SUPER_TOKEN_BALANCE);
        deal(address(superToken2), bob, INIT_SUPER_TOKEN_BALANCE);

        _sendPICBid(alice, superToken, bond, _shouldDefaultExitRate(bond));
        _sendPICBid(bob, superToken2, bond, _shouldDefaultExitRate(bond));

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
        _sendPICBid(alice, superToken2, bobBondLeft, 0);

        _sendPICBid(alice, superToken2, bobBondLeft + 1, 0);

        assertTrue((bobPreBal + bobBondLeft) == superToken2.balanceOf(bob));
    }
}
