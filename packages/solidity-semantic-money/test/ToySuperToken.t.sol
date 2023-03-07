// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "../src/ToySuperToken.sol";

contract ToySuperTokenTest is Test {
    address internal constant admin = address(0x420);
    address internal constant alice = address(0x421);
    address internal constant bob = address(0x422);
    address internal constant carol = address(0x423);
    address internal constant dan = address(0x424);
    address internal constant eve = address(0x425);
    address internal constant frank = address(0x426);
    address internal constant grace = address(0x427);
    address internal constant heidi = address(0x428);
    address internal constant ivan = address(0x429);
    uint internal immutable N_TESTERS;

    address[] internal TEST_ACCOUNTS = [admin,alice,bob,carol,dan,eve,frank,grace,heidi,ivan];
    ToySuperToken internal token;
    ToySuperTokenPool internal pl;

    constructor () {
        N_TESTERS = TEST_ACCOUNTS.length;
    }

    function setUp() public {
        token = new ToySuperToken();
        for (uint i = 1; i < N_TESTERS; ++i) {
            vm.startPrank(admin);
            token.transfer(TEST_ACCOUNTS[i], type(uint64).max);
            vm.stopPrank();
        }
        vm.startPrank(alice);
        pl = token.createPool();
        vm.stopPrank();
    }

    function testERC20Transfer(uint32 x) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        vm.startPrank(alice);
        token.transfer(bob, x);
        vm.stopPrank();
        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        assertEq(a1 - a2, x);
        assertEq(b2 - b1, x);
    }

    function testFlow(uint32 r, uint16 t2) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 t1 = block.timestamp;
        vm.startPrank(alice);
        token.flow(alice, bob, FlowId.wrap(0), FlowRate.wrap(int64(uint64(r))));
        vm.stopPrank();
        vm.warp(t1 + uint256(t2));
        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        assertEq(a1 - a2, uint256(r) * uint256(t2), "e1");
        assertEq(b2 - b1, uint256(r) * uint256(t2), "e2");
    }

    function testDistribute(int32 u1, int32 u2, uint64 x) external {
        vm.assume(u1 >= 0);
        vm.assume(u2 >= 0);
        uint tu = uint(int(u1) + int(u2));
        vm.assume(tu > 0);
        uint x1 = uint(x) / tu * tu;
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);

        vm.startPrank(alice);
        pl.updateMember(bob, Unit.wrap(u1));
        pl.updateMember(carol, Unit.wrap(u2));
        token.distribute(alice, pl, Value.wrap(int(uint(x))));
        vm.stopPrank();

        vm.startPrank(bob);
        token.connectPool(pl);
        vm.stopPrank();

        vm.startPrank(carol);
        token.connectPool(pl);
        vm.stopPrank();

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);
        assertEq(a1 - a2, x1, "e1");
        assertEq(b2 - b1 + c2 - c1, x1, "e2");
    }

    function testDistributeFlowBothConnected(int32 u1, int32 u2, uint32 r, uint16 t2) external {
        vm.assume(u1 >= 0);
        vm.assume(u2 >= 0);
        uint tu = uint(int(u1) + int(u2));
        vm.assume(tu > 0);
        int r1 = int(uint(r) / tu * tu);
        uint256 t1 = block.timestamp;

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(bob, Unit.wrap(u1));
        pl.updateMember(carol, Unit.wrap(u2));
        token.distributeFlow(alice, pl, FlowId.wrap(0), FlowRate.wrap(int128(uint128(r))));
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), int(tu), "e1");

        vm.startPrank(bob);
        token.connectPool(pl);
        vm.stopPrank();

        vm.startPrank(carol);
        token.connectPool(pl);
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), 0, "e2");

        vm.warp(t1 + uint256(t2));
        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);
        uint256 p2 = token.balanceOf(address(pl));
        assertEq(a1 - a2, uint256(r1) * uint256(t2), "e3");
        assertEq(b2 - b1 + c2 - c1 + p2 - p1, uint256(r1) * uint256(t2), "e4");
    }

    function testDistributeFlowWithSomeConnected(int32 u1, int32 u2, uint32 r, uint16 t2) external {
        vm.assume(u1 >= 0);
        vm.assume(u2 >= 0);
        uint tu = uint(int(u1) + int(u2));
        vm.assume(tu > 0);
        int r1 = int(uint(r) / tu * tu);
        uint256 t1 = block.timestamp;

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(bob, Unit.wrap(u1));
        pl.updateMember(carol, Unit.wrap(u2));
        token.distributeFlow(alice, pl, FlowId.wrap(0), FlowRate.wrap(int128(uint128(r))));
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), int(tu), "e1");

        vm.startPrank(bob);
        token.connectPool(pl);
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), int(u2), "e2");

        vm.warp(t1 + uint256(t2));
        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);
        uint256 p2 = token.balanceOf(address(pl));
        assertEq(a1 - a2, uint256(r1) * uint256(t2), "e3");
        assertEq(c2 - c1, 0, "e4");
        assertEq(b2 - b1 + c2 - c1 + p2 - p1, uint256(r1) * uint256(t2), "e5");
    }
}
