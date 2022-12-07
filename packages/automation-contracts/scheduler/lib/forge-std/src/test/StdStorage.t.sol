// SPDX-License-Identifier: Unlicense
pragma solidity >=0.6.0 <0.9.0;

import "../Test.sol";

contract StdStorageTest is Test {
    using stdStorage for StdStorage;

    StorageTest test;

    function setUp() public {
        test = new StorageTest();
    }

    function testStorageHidden() public {
        assertEq(uint256(keccak256("my.random.var")), stdstore.target(address(test)).sig("hidden()").find());
    }

    function testStorageObvious() public {
        assertEq(uint256(0), stdstore.target(address(test)).sig("exists()").find());
    }

    function testStorageCheckedWriteHidden() public {
        stdstore.target(address(test)).sig(test.hidden.selector).checked_write(100);
        assertEq(uint256(test.hidden()), 100);
    }

    function testStorageCheckedWriteObvious() public {
        stdstore.target(address(test)).sig(test.exists.selector).checked_write(100);
        assertEq(test.exists(), 100);
    }

    function testStorageMapStructA() public {
        uint256 slot = stdstore
            .target(address(test))
            .sig(test.map_struct.selector)
            .with_key(address(this))
            .depth(0)
            .find();
        assertEq(uint256(keccak256(abi.encode(address(this), 4))), slot);
    }

    function testStorageMapStructB() public {
        uint256 slot = stdstore
            .target(address(test))
            .sig(test.map_struct.selector)
            .with_key(address(this))
            .depth(1)
            .find();
       assertEq(uint256(keccak256(abi.encode(address(this), 4))) + 1, slot);
    }

    function testStorageDeepMap() public {
        uint256 slot = stdstore
            .target(address(test))
            .sig(test.deep_map.selector)
            .with_key(address(this))
            .with_key(address(this))
            .find();
        assertEq(uint256(keccak256(abi.encode(address(this), keccak256(abi.encode(address(this), uint(5)))))), slot);
    }

    function testStorageCheckedWriteDeepMap() public {
        stdstore
            .target(address(test))
            .sig(test.deep_map.selector)
            .with_key(address(this))
            .with_key(address(this))
            .checked_write(100);
        assertEq(100, test.deep_map(address(this), address(this)));
    }

    function testStorageDeepMapStructA() public {
        uint256 slot = stdstore
            .target(address(test))
            .sig(test.deep_map_struct.selector)
            .with_key(address(this))
            .with_key(address(this))
            .depth(0)
            .find();
        assertEq(bytes32(uint256(keccak256(abi.encode(address(this), keccak256(abi.encode(address(this), uint(6)))))) + 0), bytes32(slot));
    }

    function testStorageDeepMapStructB() public {
        uint256 slot = stdstore
            .target(address(test))
            .sig(test.deep_map_struct.selector)
            .with_key(address(this))
            .with_key(address(this))
            .depth(1)
            .find();
        assertEq(bytes32(uint256(keccak256(abi.encode(address(this), keccak256(abi.encode(address(this), uint(6)))))) + 1), bytes32(slot));
    }

    function testStorageCheckedWriteDeepMapStructA() public {
        stdstore
            .target(address(test))
            .sig(test.deep_map_struct.selector)
            .with_key(address(this))
            .with_key(address(this))
            .depth(0)
            .checked_write(100);
        (uint256 a, uint256 b) = test.deep_map_struct(address(this), address(this));
        assertEq(100, a);
        assertEq(0, b);
    }

    function testStorageCheckedWriteDeepMapStructB() public {
        stdstore
            .target(address(test))
            .sig(test.deep_map_struct.selector)
            .with_key(address(this))
            .with_key(address(this))
            .depth(1)
            .checked_write(100);
        (uint256 a, uint256 b) = test.deep_map_struct(address(this), address(this));
        assertEq(0, a);
        assertEq(100, b);
    }

    function testStorageCheckedWriteMapStructA() public {
        stdstore
            .target(address(test))
            .sig(test.map_struct.selector)
            .with_key(address(this))
            .depth(0)
            .checked_write(100);
        (uint256 a, uint256 b) = test.map_struct(address(this));
        assertEq(a, 100);
        assertEq(b, 0);
    }

    function testStorageCheckedWriteMapStructB() public {
        stdstore
            .target(address(test))
            .sig(test.map_struct.selector)
            .with_key(address(this))
            .depth(1)
            .checked_write(100);
        (uint256 a, uint256 b) = test.map_struct(address(this));
        assertEq(a, 0);
        assertEq(b, 100);
    }

    function testStorageStructA() public {
        uint256 slot = stdstore.target(address(test)).sig(test.basic.selector).depth(0).find();
        assertEq(uint256(7), slot);
    }

    function testStorageStructB() public {
        uint256 slot = stdstore.target(address(test)).sig(test.basic.selector).depth(1).find();
        assertEq(uint256(7) + 1, slot);
    }

    function testStorageCheckedWriteStructA() public {
        stdstore.target(address(test)).sig(test.basic.selector).depth(0).checked_write(100);
        (uint256 a, uint256 b) = test.basic();
        assertEq(a, 100);
        assertEq(b, 1337);
    }

    function testStorageCheckedWriteStructB() public {
         stdstore.target(address(test)).sig(test.basic.selector).depth(1).checked_write(100);
        (uint256 a, uint256 b) = test.basic();
        assertEq(a, 1337);
        assertEq(b, 100);
    }

    function testStorageMapAddrFound() public {
        uint256 slot = stdstore.target(address(test)).sig(test.map_addr.selector).with_key(address(this)).find();
        assertEq(uint256(keccak256(abi.encode(address(this), uint(1)))), slot);
    }

    function testStorageMapUintFound() public {
        uint256 slot = stdstore.target(address(test)).sig(test.map_uint.selector).with_key(100).find();
        assertEq(uint256(keccak256(abi.encode(100, uint(2)))), slot);
    }

    function testStorageCheckedWriteMapUint() public {
        stdstore.target(address(test)).sig(test.map_uint.selector).with_key(100).checked_write(100);
        assertEq(100, test.map_uint(100));
    }

    function testStorageCheckedWriteMapAddr() public {
        stdstore.target(address(test)).sig(test.map_addr.selector).with_key(address(this)).checked_write(100);
        assertEq(100, test.map_addr(address(this)));
    }

    function testStorageCheckedWriteMapBool() public {
        stdstore.target(address(test)).sig(test.map_bool.selector).with_key(address(this)).checked_write(true);
        assertTrue(test.map_bool(address(this)));
    }

    function testFailStorageCheckedWriteMapPacked() public {
        // expect PackedSlot error but not external call so cant expectRevert
        stdstore.target(address(test)).sig(test.read_struct_lower.selector).with_key(address(uint160(1337))).checked_write(100);
    }

    function testStorageCheckedWriteMapPackedSuccess() public {
        uint256 full = test.map_packed(address(1337));
        // keep upper 128, set lower 128 to 1337
        full = (full & (uint256((1 << 128) - 1) << 128)) | 1337;
        stdstore.target(address(test)).sig(test.map_packed.selector).with_key(address(uint160(1337))).checked_write(full);
        assertEq(1337, test.read_struct_lower(address(1337)));
    }

    function testFailStorageConst() public {
        // vm.expectRevert(abi.encodeWithSignature("NotStorage(bytes4)", bytes4(keccak256("const()"))));
        stdstore.target(address(test)).sig("const()").find();
    }

    function testFailStorageNativePack() public {
        stdstore.target(address(test)).sig(test.tA.selector).find();
        stdstore.target(address(test)).sig(test.tB.selector).find();
        
        // these both would fail
        stdstore.target(address(test)).sig(test.tC.selector).find();
        stdstore.target(address(test)).sig(test.tD.selector).find();
    }
}

contract StorageTest {
    uint256 public exists = 1;
    mapping(address => uint256) public map_addr;
    mapping(uint256 => uint256) public map_uint;
    mapping(address => uint256) public map_packed;
    mapping(address => UnpackedStruct) public map_struct;
    mapping(address => mapping(address => uint256)) public deep_map;
    mapping(address => mapping(address => UnpackedStruct)) public deep_map_struct;
    UnpackedStruct public basic;

    uint248 public tA;
    bool public tB;


    bool public tC = false;
    uint248 public tD = 1;    


    struct UnpackedStruct {
        uint256 a;
        uint256 b;
    }

    mapping(address => bool) public map_bool;

    constructor() {
        basic = UnpackedStruct({
            a: 1337,
            b: 1337
        });

        uint256 two = (1<<128) | 1;
        map_packed[msg.sender] = two;
        map_packed[address(bytes20(uint160(1337)))] = 1<<128;
    }

    function read_struct_upper(address who) public view returns (uint256) {
        return map_packed[who] >> 128;
    }

    function read_struct_lower(address who) public view returns (uint256) {
        return map_packed[who] & ((1 << 128) - 1);
    }

    function hidden() public view returns (bytes32 t) {
        bytes32 slot = keccak256("my.random.var");
        /// @solidity memory-safe-assembly
        assembly {
            t := sload(slot)
        }
    }

    function const() public pure returns (bytes32 t) {
        t = bytes32(hex"1337");
    }
}
