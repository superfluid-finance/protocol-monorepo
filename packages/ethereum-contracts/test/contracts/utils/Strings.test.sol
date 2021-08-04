// SPDX-License-Identifier: AGPLv3
// solhint-disable
// ADAPTED FROM: https://github.com/Arachnid/solidity-stringutils/
pragma solidity 0.7.6;

import '../../../contracts/utils/Strings.sol';
import "truffle/Assert.sol";

contract TestStrings  {

	using Strings for *;


    function abs(int x) private pure returns (int) {
        if(x < 0)
            return -x;
        return x;
    }

    function sign(int x) private pure returns (int) {
        return x == 0 ? int(0) : (x < 0 ? -1 : int(1));
    }

    function assertEq0(uint a, uint b) internal {
        Assert.equal(a, b, "assertEq0 uint uint");
    }

    function assertEq0(int a, int b) internal {
        Assert.equal(a, b, "assertEq0 int int");
    }

    function assertEq0(string memory a, string memory b) internal {
        Assert.equal(a, b, "assertEq0 string string");
    }

    function assertEq0(Strings.slice memory a, Strings.slice memory b) internal {
    	Assert.equal(a.toString(), b.toString(), "assertEq0 Strings.slice Strings.slice");
    }

    function assertEq0(Strings.slice memory a, string memory b) internal {
        Assert.equal(a.toString(), b, "assertEq0 Strings.slice string");
    }

    function assertIsTrue(bool a) internal {
        Assert.isTrue(a, "assertIsTrue");
    }

	function testSliceToString() public {
		string memory test = "Hello, world!";
		assertEq0(test, test.toSlice().toString());
	}

    function testToSliceB32() public {
        assertEq0(bytes32("foobar").toSliceB32(), "foobar".toSlice());
    }

    function testCopy() public {
        string memory test = "Hello, world!";
        Strings.slice memory s1 = test.toSlice();
        Strings.slice memory s2 = s1.copy();
        s1._len = 0;
        assertEq0(s2._len, bytes(test).length);
    }

    function testLen() public {
        assertEq0("".toSlice().strlen(), 0);
        assertEq0("Hello, world!".toSlice().strlen(), 13);
        //Assert.equal("naïve".toSlice().len(), 5);
        //Assert.equal("こんにちは".toSlice().len(), 5);
    }

    function testEmpty() public {
        assertIsTrue("".toSlice().empty());
        assertIsTrue(!"x".toSlice().empty());
    }

    function testEquals() public {
        assertIsTrue("".toSlice().equals("".toSlice()));
        assertIsTrue("foo".toSlice().equals("foo".toSlice()));
        assertIsTrue(!"foo".toSlice().equals("bar".toSlice()));
    }

    function testCompare() public {
        assertEq0(sign("foobie".toSlice().compare("foobie".toSlice())), 0);
        assertEq0(sign("foobie".toSlice().compare("foobif".toSlice())), -1);
        assertEq0(sign("foobie".toSlice().compare("foobid".toSlice())), 1);
        assertEq0(sign("foobie".toSlice().compare("foobies".toSlice())), -1);
        assertEq0(sign("foobie".toSlice().compare("foobi".toSlice())), 1);
        assertEq0(sign("foobie".toSlice().compare("doobie".toSlice())), 1);
        assertEq0(sign("01234567890123456789012345678901".toSlice().compare("012345678901234567890123456789012".toSlice())), -1);
		assertEq0(sign("0123456789012345678901234567890123".toSlice().compare("1123456789012345678901234567890123".toSlice())), -1);
        assertEq0(sign("foo.bar".toSlice().split(".".toSlice()).compare("foo".toSlice())), 0);
    }

    function testStartsWith() public {
        Strings.slice memory s = "foobar".toSlice();
        assertIsTrue(s.startsWith("foo".toSlice()));
        assertIsTrue(!s.startsWith("oob".toSlice()));
        assertIsTrue(s.startsWith("".toSlice()));
        assertIsTrue(s.startsWith(s.copy().rfind("foo".toSlice())));
    }

    function testBeyond() public {
        Strings.slice memory s = "foobar".toSlice();
        assertEq0(s.beyond("foo".toSlice()), "bar");
        assertEq0(s, "bar");
        assertEq0(s.beyond("foo".toSlice()), "bar");
        assertEq0(s.beyond("bar".toSlice()), "");
        assertEq0(s, "");
    }

    function testEndsWith() public {
        Strings.slice memory s = "foobar".toSlice();
        assertIsTrue(s.endsWith("bar".toSlice()));
        assertIsTrue(!s.endsWith("oba".toSlice()));
        assertIsTrue(s.endsWith("".toSlice()));
        assertIsTrue(s.endsWith(s.copy().find("bar".toSlice())));
    }

    function testUntil() public {
        Strings.slice memory s = "foobar".toSlice();
        assertEq0(s.until("bar".toSlice()), "foo");
        assertEq0(s, "foo");
        assertEq0(s.until("bar".toSlice()), "foo");
        assertEq0(s.until("foo".toSlice()), "");
        assertEq0(s, "");
    }

    function testFind() public {
        assertEq0("abracadabra".toSlice().find("abracadabra".toSlice()), "abracadabra");
        assertEq0("abracadabra".toSlice().find("bra".toSlice()), "bracadabra");
        assertIsTrue("abracadabra".toSlice().find("rab".toSlice()).empty());
        assertIsTrue("12345".toSlice().find("123456".toSlice()).empty());
        assertEq0("12345".toSlice().find("".toSlice()), "12345");
        assertEq0("12345".toSlice().find("5".toSlice()), "5");
    }

    function testRfind() public {
        assertEq0("abracadabra".toSlice().rfind("bra".toSlice()), "abracadabra");
        assertEq0("abracadabra".toSlice().rfind("cad".toSlice()), "abracad");
        assertIsTrue("12345".toSlice().rfind("123456".toSlice()).empty());
        assertEq0("12345".toSlice().rfind("".toSlice()), "12345");
        assertEq0("12345".toSlice().rfind("1".toSlice()), "1");
    }

    function testSplit() public {
        Strings.slice memory s = "foo->bar->baz".toSlice();
        Strings.slice memory delim = "->".toSlice();
        assertEq0(s.split(delim), "foo");
        assertEq0(s, "bar->baz");
        assertEq0(s.split(delim), "bar");
        assertEq0(s.split(delim), "baz");
        assertIsTrue(s.empty());
        assertEq0(s.split(delim), "");
        assertEq0(".".toSlice().split(".".toSlice()), "");
    }

    function testRsplit() public {
        Strings.slice memory s = "foo->bar->baz".toSlice();
        Strings.slice memory delim = "->".toSlice();
        assertEq0(s.rsplit(delim), "baz");
        assertEq0(s.rsplit(delim), "bar");
        assertEq0(s.rsplit(delim), "foo");
        assertIsTrue(s.empty());
        assertEq0(s.rsplit(delim), "");
    }

    function testCount() public {
        assertEq0("1121123211234321".toSlice().count("1".toSlice()), 7);
        assertEq0("ababababa".toSlice().count("aba".toSlice()), 2);
    }

    function testContains() public {
        assertIsTrue("foobar".toSlice().contains("f".toSlice()));
        assertIsTrue("foobar".toSlice().contains("o".toSlice()));
        assertIsTrue("foobar".toSlice().contains("r".toSlice()));
        assertIsTrue("foobar".toSlice().contains("".toSlice()));
        assertIsTrue("foobar".toSlice().contains("foobar".toSlice()));
        assertIsTrue(!"foobar".toSlice().contains("s".toSlice()));
    }

    function testConcat() public {
        assertEq0("foo".toSlice().concat("bar".toSlice()), "foobar");
        assertEq0("".toSlice().concat("bar".toSlice()), "bar");
        assertEq0("foo".toSlice().concat("".toSlice()), "foo");
    }

    function testJoin() public {
        Strings.slice[] memory parts = new Strings.slice[](4);
        parts[0] = "zero".toSlice();
        parts[1] = "one".toSlice();
        parts[2] = "".toSlice();
        parts[3] = "two".toSlice();

        assertEq0(" ".toSlice().join(parts), "zero one  two");
        assertEq0("".toSlice().join(parts), "zeroonetwo");

        parts = new Strings.slice[](1);
        parts[0] = "zero".toSlice();
        assertEq0(" ".toSlice().join(parts), "zero");
    }
}
