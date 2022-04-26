const TestEnvironment = require("../TestEnvironment");

const {toBN} = require("@decentral.ee/web3-helpers");
const {expectRevertedWith} = require("../utils/expectRevert");

const DEFAULT_ADMIN_ROLE =
    "0x0000000000000000000000000000000000000000000000000000000000000000";

describe("Miscellaneous for test coverages", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;

    let admin, alice;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({admin, alice} = t.aliases);
    });

    describe("UUPS", () => {
        const UUPSProxy = artifacts.require("UUPSProxy");
        const UUPSProxiableMock = artifacts.require("UUPSProxiableMock");

        it("UUPSProxy", async () => {
            const proxy = await UUPSProxy.new();
            const proxiable = await UUPSProxiableMock.at(proxy.address);
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const mock = await UUPSProxiableMock.new(uuid1, 1);
            await expectRevertedWith(
                proxy.initializeProxy(ZERO_ADDRESS),
                "UUPSProxy: zero address"
            );
            await proxy.initializeProxy(mock.address);
            await expectRevertedWith(
                proxy.initializeProxy(mock.address),
                "UUPSProxy: already initialized"
            );
            assert.equal(await proxiable.proxiableUUID(), uuid1);
        });

        it("UUPSProxiable", async () => {
            const proxy = await UUPSProxy.new();
            const proxiable = await UUPSProxiableMock.at(proxy.address);
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const uuid2 = web3.utils.sha3("UUPSProxiableMock2");
            const mock1a = await UUPSProxiableMock.new(uuid1, 1);
            const mock1b = await UUPSProxiableMock.new(uuid1, 2);
            const mock2 = await UUPSProxiableMock.new(uuid2, 1);

            assert.equal(await mock1a.getCodeAddress(), ZERO_ADDRESS);
            await expectRevertedWith(
                mock1a.updateCode(mock1a.address),
                "UUPSProxiable: not upgradable"
            );
            await proxiable.updateCode(mock1a.address);

            await proxy.initializeProxy(mock1a.address);
            assert.equal(await proxiable.proxiableUUID(), uuid1);
            assert.equal(await proxiable.waterMark(), 1);

            await proxiable.updateCode(mock1b.address);
            assert.equal(await proxiable.proxiableUUID(), uuid1);
            assert.equal(await proxiable.waterMark(), 2);

            await expectRevertedWith(
                proxiable.updateCode(mock2.address),
                "UUPSProxiable: not compatible logic"
            );
        });
    });

    describe("Resolver", () => {
        const Resolver = artifacts.require("Resolver");

        it("Resolver.set should only be called by admin", async () => {
            const resolver = await Resolver.new({from: admin});
            await expectRevertedWith(
                resolver.set("alice", alice, {from: alice}),
                "Caller is not an admin"
            );
            await resolver.grantRole(DEFAULT_ADMIN_ROLE, alice);
            await resolver.set("alice", alice, {from: alice});
            assert.equal(await resolver.get("alice"), alice);
        });
    });

    describe("Libs/CallUtils", () => {
        const CallUtilsMock = artifacts.require("CallUtilsMock");
        let callUtilsMock;

        before(async () => {
            callUtilsMock = await CallUtilsMock.new();
        });

        it("CallUtils.revertFromReturnedData", async () => {
            await expectRevertedWith(
                callUtilsMock.revertTest("revertEmpty()"),
                "CallUtils: target revert()"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertAssert()"),
                "CallUtils: target panicked: 0x01"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertOverflow()"),
                "CallUtils: target panicked: 0x11"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertDivByZero()"),
                "CallUtils: target panicked: 0x12"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertEnum()"),
                "CallUtils: target panicked: 0x21"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertPop()"),
                "CallUtils: target panicked: 0x31"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertArrayAccess()"),
                "CallUtils: target panicked: 0x32"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertBigArray()"),
                "CallUtils: target panicked: 0x41"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest(
                    "revertZeroInitializedFunctionPointer()"
                ),
                "CallUtils: target panicked: 0x51"
            );

            await expectRevertedWith(
                callUtilsMock.revertTest("revertString()"),
                "gm"
            );
            // TODO: Add revert custom error tests
        });

        it("CallUtils.isValidAbiEncodedBytes", async () => {
            await callUtilsMock.testIsValidAbiEncodedBytes();
        });
    });

    describe("Libs/Int96SafeMath", () => {
        it("Int96SafeMath common cases", async () => {
            const MAX_INT96 = toBN("39614081257132168796771975167");
            const MAX_INT96_DIV_2 = toBN("19807040628566084398385987583");
            const MAX_INT96_DIV_2_PLUS_1 = toBN(
                "19807040628566084398385987584"
            );
            const MAX_INT96_MINUS_1 = toBN("39614081257132168796771975166");
            const MIN_INT96 = toBN("-39614081257132168796771975168");
            const MIN_INT96_DIV_2 = toBN("-19807040628566084398385987584");
            const MIN_INT96_DIV_2_MINUS_1 = toBN(
                "-19807040628566084398385987585"
            );
            const MIN_INT96_PLUS_1 = toBN("-39614081257132168796771975167");
            const UtilsTester = artifacts.require("UtilsTester");
            const tester = await UtilsTester.new();

            assert.equal((await tester.getInt96Max()).toString(), MAX_INT96);
            assert.equal((await tester.getInt96Min()).toString(), MIN_INT96);

            // testInt96SafeMathMul
            assert.equal(
                (await tester.testInt96SafeMathMul(0, MAX_INT96)).toString(),
                "0"
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathMul(2, MAX_INT96_DIV_2)
                ).toString(),
                MAX_INT96_MINUS_1.toString()
            );
            await expectRevertedWith(
                tester.testInt96SafeMathMul(MAX_INT96_DIV_2_PLUS_1, 2),
                "testInt96SafeMathMul overflow"
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathMul(2, MIN_INT96_DIV_2)
                ).toString(),
                MIN_INT96.toString()
            );
            await expectRevertedWith(
                tester.testInt96SafeMathMul(MIN_INT96_DIV_2_MINUS_1, 2),
                "testInt96SafeMathMul overflow"
            );
            await expectRevertedWith(
                tester.testInt96SafeMathMul("-1", MIN_INT96),
                "testInt96SafeMathMul overflow"
            );

            // testInt96SafeMathAdd
            assert.equal(
                (await tester.echoInt96(MAX_INT96)).toString(),
                MAX_INT96.toString()
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathAdd(MAX_INT96_MINUS_1, "1")
                ).toString(),
                MAX_INT96.toString()
            );
            await expectRevertedWith(
                tester.testInt96SafeMathAdd(MAX_INT96, "1"),
                "testInt96SafeMathAdd overflow"
            );

            // testInt96SafeMathSub
            assert.equal(
                (await tester.echoInt96(MIN_INT96)).toString(),
                MIN_INT96.toString()
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathSub(MIN_INT96_PLUS_1, "1")
                ).toString(),
                MIN_INT96.toString()
            );
            await expectRevertedWith(
                tester.testInt96SafeMathSub(MIN_INT96, "1"),
                "testInt96SafeMathSub overflow"
            );
            // testInt96SafeMathDiv
            assert.equal(
                (await tester.testInt96SafeMathDiv(MAX_INT96, 1)).toString(),
                MAX_INT96.toString()
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathDiv(MAX_INT96_MINUS_1, 2)
                ).toString(),
                MAX_INT96_DIV_2.toString()
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathDiv(MAX_INT96, MAX_INT96)
                ).toString(),
                "1"
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathDiv(MIN_INT96, MIN_INT96)
                ).toString(),
                "1"
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathDiv(MAX_INT96, MIN_INT96)
                ).toString(),
                "0"
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathDiv(MIN_INT96, MAX_INT96)
                ).toString(),
                "-1"
            );

            await expectRevertedWith(
                tester.testInt96SafeMathDiv(MIN_INT96, 0),
                "testInt96SafeMathDiv overflow"
            );

            await expectRevertedWith(
                tester.testInt96SafeMathDiv(MIN_INT96, -1),
                "testInt96SafeMathDiv overflow"
            );
        });
    });

    describe("Libs/UInt128SafeMath", () => {
        it("UInt128SafeMath common cases", async () => {
            const MAX_UINT128 = "340282366920938463463374607431768211455";
            const MAX_UINT128_MINUS_1 =
                "340282366920938463463374607431768211454";
            const UtilsTester = artifacts.require("UtilsTester");
            const tester = await UtilsTester.new();

            assert.equal(
                (await tester.getUint128Max()).toString(),
                MAX_UINT128
            );

            assert.equal(
                (
                    await tester.testUInt128SafeMathAdd(
                        MAX_UINT128_MINUS_1,
                        "1"
                    )
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
});
