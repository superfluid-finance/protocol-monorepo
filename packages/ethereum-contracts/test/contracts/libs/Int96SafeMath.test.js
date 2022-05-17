const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

const {toBN} = require("@decentral.ee/web3-helpers");

describe("Int96SafeMath", function () {
    const t = TestEnvironment.getSingleton();

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
    });

    it("Int96SafeMath edge cases", async () => {
        const MAX_INT96 = toBN("39614081257132168796771975167");
        const MAX_INT96_DIV_2 = toBN("19807040628566084398385987583");
        const MAX_INT96_DIV_2_PLUS_1 = toBN("19807040628566084398385987584");
        const MAX_INT96_MINUS_1 = toBN("39614081257132168796771975166");
        const MIN_INT96 = toBN("-39614081257132168796771975168");
        const MIN_INT96_DIV_2 = toBN("-19807040628566084398385987584");
        const MIN_INT96_DIV_2_MINUS_1 = toBN("-19807040628566084398385987585");
        const MIN_INT96_PLUS_1 = toBN("-39614081257132168796771975167");
        const UtilsTester = artifacts.require("UtilsTester");
        const tester = await UtilsTester.new();

        assert.equal((await tester.getInt96Max()).toString(), MAX_INT96);
        assert.equal((await tester.getInt96Min()).toString(), MIN_INT96);

        // doInt96SafeMathMul
        assert.equal(
            (await tester.doInt96SafeMathMul(0, MAX_INT96)).toString(),
            "0"
        );
        assert.equal(
            (await tester.doInt96SafeMathMul(2, MAX_INT96_DIV_2)).toString(),
            MAX_INT96_MINUS_1.toString()
        );
        await expectRevertedWith(
            tester.doInt96SafeMathMul(MAX_INT96_DIV_2_PLUS_1, 2),
            "doInt96SafeMathMul overflow"
        );
        assert.equal(
            (await tester.doInt96SafeMathMul(2, MIN_INT96_DIV_2)).toString(),
            MIN_INT96.toString()
        );
        await expectRevertedWith(
            tester.doInt96SafeMathMul(MIN_INT96_DIV_2_MINUS_1, 2),
            "doInt96SafeMathMul overflow"
        );
        await expectRevertedWith(
            tester.doInt96SafeMathMul("-1", MIN_INT96),
            "doInt96SafeMathMul overflow"
        );

        // doInt96SafeMathAdd
        assert.equal(
            (await tester.echoInt96(MAX_INT96)).toString(),
            MAX_INT96.toString()
        );
        assert.equal(
            (
                await tester.doInt96SafeMathAdd(MAX_INT96_MINUS_1, "1")
            ).toString(),
            MAX_INT96.toString()
        );
        await expectRevertedWith(
            tester.doInt96SafeMathAdd(MAX_INT96, "1"),
            "doInt96SafeMathAdd overflow"
        );

        // doInt96SafeMathSub
        assert.equal(
            (await tester.echoInt96(MIN_INT96)).toString(),
            MIN_INT96.toString()
        );
        assert.equal(
            (await tester.doInt96SafeMathSub(MIN_INT96_PLUS_1, "1")).toString(),
            MIN_INT96.toString()
        );
        await expectRevertedWith(
            tester.doInt96SafeMathSub(MIN_INT96, "1"),
            "doInt96SafeMathSub overflow"
        );
        // doInt96SafeMathDiv
        assert.equal(
            (await tester.doInt96SafeMathDiv(MAX_INT96, 1)).toString(),
            MAX_INT96.toString()
        );
        assert.equal(
            (await tester.doInt96SafeMathDiv(MAX_INT96_MINUS_1, 2)).toString(),
            MAX_INT96_DIV_2.toString()
        );
        assert.equal(
            (await tester.doInt96SafeMathDiv(MAX_INT96, MAX_INT96)).toString(),
            "1"
        );
        assert.equal(
            (await tester.doInt96SafeMathDiv(MIN_INT96, MIN_INT96)).toString(),
            "1"
        );
        assert.equal(
            (await tester.doInt96SafeMathDiv(MAX_INT96, MIN_INT96)).toString(),
            "0"
        );
        assert.equal(
            (await tester.doInt96SafeMathDiv(MIN_INT96, MAX_INT96)).toString(),
            "-1"
        );

        await expectRevertedWith(
            tester.doInt96SafeMathDiv(MIN_INT96, 0),
            "doInt96SafeMathDiv overflow"
        );

        await expectRevertedWith(
            tester.doInt96SafeMathDiv(MIN_INT96, -1),
            "doInt96SafeMathDiv overflow"
        );
    });
});
