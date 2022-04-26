const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

describe("UInt128SafeMath", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
    });

    it("UInt128SafeMath edge cases", async () => {
        const MAX_UINT128 = "340282366920938463463374607431768211455";
        const MAX_UINT128_MINUS_1 = "340282366920938463463374607431768211454";
        const UtilsTester = artifacts.require("UtilsTester");
        const tester = await UtilsTester.new();

        assert.equal((await tester.getUint128Max()).toString(), MAX_UINT128);

        assert.equal(
            (
                await tester.testUInt128SafeMathAdd(MAX_UINT128_MINUS_1, "1")
            ).toString(),
            MAX_UINT128
        );
        await expectRevertedWith(
            tester.testUInt128SafeMathAdd("1", MAX_UINT128),
            "testUInt128SafeMathAdd overflow"
        );
        await expectRevertedWith(
            tester.testInt128SafeMathSub("0", "1"),
            "testInt96SafeMathSub overflow"
        );
    });
});
