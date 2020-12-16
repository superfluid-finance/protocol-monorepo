const { expectRevert } = require("@openzeppelin/test-helpers");

//const TestEnvironment = require("../../TestEnvironment");
//
// const {
//     web3tx,
//     toWad
// } = require("@decentral.ee/web3-helpers");

const DEFAULT_ADMIN_ROLE = "0x0000000000000000000000000000000000000000000000000000000000000000";


contract("Miscellaneous for test coverages", accounts => {

    const admin = accounts[0];
    const alice = accounts[1];

    describe("TestResolver", () => {
        const TestResolver = artifacts.require("TestResolver");

        it("TestResolver.set should only be called by admin", async () => {
            const resolver = await TestResolver.new({ from: admin });
            await expectRevert(
                resolver.set("alice", alice, { from: alice }),
                "Caller is not an admin");
            await resolver.grantRole(DEFAULT_ADMIN_ROLE, alice);
            await resolver.set("alice", alice, { from: alice });
            assert.equal(await resolver.get("alice"), alice);
        });
    });

    describe("FullUpgradableSuperTokenProxy", () => {
        const IERC20 = artifacts.require("IERC20");
        const FullUpgradableSuperTokenProxy = artifacts.require("FullUpgradableSuperTokenProxy");

        it("initialization checks", async () => {
            const proxy = await FullUpgradableSuperTokenProxy.new({ from: admin });
            const token = await IERC20.at(proxy.address);
            await expectRevert(token.transfer(alice, 1, { from: admin }), "Not initialized");
            await proxy.initialize({ from: admin });
            await expectRevert(proxy.initialize({ from: admin }), "Already initialized");
        });
    });

    describe("Utils", () => {
        it("Int96SafeMath", async () => {
            const MAX_INT96 = "39614081257132168796771975167";
            const MAX_INT96_DIV_2 = "19807040628566084398385987583";
            const MAX_INT96_DIV_2_PLUS_1 = "19807040628566084398385987584";
            const MAX_INT96_MINUS_1 = "39614081257132168796771975166";
            const MIN_INT96 = "-39614081257132168796771975168";
            const MIN_INT96_DIV_2 = "-19807040628566084398385987584";
            const MIN_INT96_DIV_2_MINUS_1 = "-19807040628566084398385987585";
            const MIN_INT96_PLUS_1 = "-39614081257132168796771975167";
            const UtilsTester = artifacts.require("UtilsTester");
            const tester = await UtilsTester.new();

            assert.equal(
                (await tester.getInt96Max()).toString(),
                MAX_INT96
            );
            assert.equal(
                (await tester.getInt96Min()).toString(),
                MIN_INT96
            );

            // testInt96SafeMathAdd
            assert.equal(
                (await tester.testInt96SafeMathMul(0, MAX_INT96)).toString(),
                "0"
            );
            assert.equal(
                (await tester.testInt96SafeMathMul(2, MAX_INT96_DIV_2)).toString(),
                MAX_INT96_MINUS_1
            );
            await expectRevert(
                tester.testInt96SafeMathMul(MAX_INT96_DIV_2_PLUS_1, 2),
                "testInt96SafeMathMul overflow");
            assert.equal(
                (await tester.testInt96SafeMathMul(2, MIN_INT96_DIV_2)).toString(),
                MIN_INT96
            );
            await expectRevert(
                tester.testInt96SafeMathMul(MIN_INT96_DIV_2_MINUS_1, 2),
                "testInt96SafeMathMul overflow");

            // testInt96SafeMathAdd
            assert.equal(
                (await tester.echoInt96(MAX_INT96)).toString(),
                MAX_INT96
            );
            assert.equal(
                (await tester.testInt96SafeMathAdd(MAX_INT96_MINUS_1, "1")).toString(),
                MAX_INT96
            );
            await expectRevert(
                tester.testInt96SafeMathAdd(MAX_INT96, "1"),
                "testInt96SafeMathAdd overflow");

            // testInt96SafeMathSub
            assert.equal(
                (await tester.echoInt96(MIN_INT96)).toString(),
                MIN_INT96
            );
            assert.equal(
                (await tester.testInt96SafeMathSub(MIN_INT96_PLUS_1, "1")).toString(),
                MIN_INT96
            );
            await expectRevert(
                tester.testInt96SafeMathSub(MIN_INT96, "1"),
                "testInt96SafeMathSub overflow");
        });

        it("UInt128SafeMath", async () => {
            const MAX_UINT128 = "340282366920938463463374607431768211455";
            const MAX_UINT128_MINUS_1 = "340282366920938463463374607431768211454";
            const UtilsTester = artifacts.require("UtilsTester");
            const tester = await UtilsTester.new();

            assert.equal(
                (await tester.getUint128Max()).toString(),
                MAX_UINT128
            );

            assert.equal(
                (await tester.testUInt128SafeMathAdd(MAX_UINT128_MINUS_1, "1")).toString(),
                MAX_UINT128
            );
            await expectRevert(
                tester.testUInt128SafeMathAdd("1", MAX_UINT128),
                "testUInt128SafeMathAdd overflow");
            await expectRevert(
                tester.testInt128SafeMathSub("0", "1"),
                "testInt96SafeMathSub overflow");
        });

    });

});
