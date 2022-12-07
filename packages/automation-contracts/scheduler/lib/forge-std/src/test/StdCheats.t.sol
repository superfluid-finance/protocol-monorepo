// SPDX-License-Identifier: Unlicense
pragma solidity >=0.6.0 <0.9.0;

import "../Test.sol";

contract StdCheatsTest is Test {
    Bar test;
    
    function setUp() public {
        test = new Bar();
    }

    function testSkip() public {
        vm.warp(100);
        skip(25);
        assertEq(block.timestamp, 125);
    }

    function testRewind() public {
        vm.warp(100);
        rewind(25);
        assertEq(block.timestamp, 75);
    }

    function testHoax() public {
        hoax(address(1337));
        test.bar{value: 100}(address(1337));
    }

    function testHoaxOrigin() public {
        hoax(address(1337), address(1337));
        test.origin{value: 100}(address(1337));
    }

    function testHoaxDifferentAddresses() public {
        hoax(address(1337), address(7331));
        test.origin{value: 100}(address(1337), address(7331));
    }

    function testStartHoax() public {
        startHoax(address(1337));
        test.bar{value: 100}(address(1337));
        test.bar{value: 100}(address(1337));
        vm.stopPrank();
        test.bar(address(this));
    }

    function testStartHoaxOrigin() public {
        startHoax(address(1337), address(1337));
        test.origin{value: 100}(address(1337));
        test.origin{value: 100}(address(1337));
        vm.stopPrank();
        test.bar(address(this));
    }

    function testDeal() public {
        deal(address(this), 1 ether);
        assertEq(address(this).balance, 1 ether);
    }

    function testDealToken() public {
        Bar barToken = new Bar();
        address bar = address(barToken);
        deal(bar, address(this), 10000e18);
        assertEq(barToken.balanceOf(address(this)), 10000e18);
    }

    function testDealTokenAdjustTS() public {
        Bar barToken = new Bar();
        address bar = address(barToken);
        deal(bar, address(this), 10000e18, true);
        assertEq(barToken.balanceOf(address(this)), 10000e18);
        assertEq(barToken.totalSupply(), 20000e18);
        deal(bar, address(this), 0, true);
        assertEq(barToken.balanceOf(address(this)), 0);
        assertEq(barToken.totalSupply(), 10000e18);
    }

    function testDeployCode() public {
        address deployed = deployCode("StdCheats.t.sol:StdCheatsTest", bytes(""));
        assertEq(string(getCode(deployed)), string(getCode(address(this))));
    }

    function testDeployCodeNoArgs() public {
        address deployed = deployCode("StdCheats.t.sol:StdCheatsTest");
        assertEq(string(getCode(deployed)), string(getCode(address(this))));
    }

    function getCode(address who) internal view returns (bytes memory o_code) {
        /// @solidity memory-safe-assembly
        assembly {
            // retrieve the size of the code, this needs assembly
            let size := extcodesize(who)
            // allocate output byte array - this could also be done without assembly
            // by using o_code = new bytes(size)
            o_code := mload(0x40)
            // new "memory end" including padding
            mstore(0x40, add(o_code, and(add(add(size, 0x20), 0x1f), not(0x1f))))
            // store length in memory
            mstore(o_code, size)
            // actually retrieve the code, this needs assembly
            extcodecopy(who, add(o_code, 0x20), 0, size)
        }
    }
}

contract Bar {
    constructor() {
        /// `DEAL` STDCHEAT
        totalSupply = 10000e18;
        balanceOf[address(this)] = totalSupply;
    }

    /// `HOAX` STDCHEATS
    function bar(address expectedSender) public payable {
        require(msg.sender == expectedSender, "!prank");
    }
    function origin(address expectedSender) public payable {
        require(msg.sender == expectedSender, "!prank");
        require(tx.origin == expectedSender, "!prank");
    }
    function origin(address expectedSender, address expectedOrigin) public payable {
        require(msg.sender == expectedSender, "!prank");
        require(tx.origin == expectedOrigin, "!prank");
    }

    /// `DEAL` STDCHEAT
    mapping (address => uint256) public balanceOf;
    uint256 public totalSupply;
}
