const { expectRevert } = require("@openzeppelin/test-helpers");

//const TestEnvironment = require("../../TestEnvironment");
//
// const {
//     web3tx,
//     toWad
// } = require("@decentral.ee/web3-helpers");

const DEFAULT_ADMIN_ROLE =
    "0x0000000000000000000000000000000000000000000000000000000000000000";
const ZERO_ADDRESS = "0x" + "0".repeat(40);

contract("Miscellaneous for test coverages", accounts => {
    const admin = accounts[0];
    const alice = accounts[1];

    describe("UUPS", () => {
        const UUPSProxy = artifacts.require("UUPSProxy");
        const UUPSProxiableMock = artifacts.require("UUPSProxiableMock");

        it("UUPSProxy", async () => {
            const proxy = await UUPSProxy.new();
            const proxiable = await UUPSProxiableMock.at(proxy.address);
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const mock = await UUPSProxiableMock.new(uuid1, 1);
            await expectRevert(
                proxy.initializeProxy(ZERO_ADDRESS),
                "UUPSProxy: zero address"
            );
            await proxy.initializeProxy(mock.address);
            await expectRevert(
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
            await expectRevert(
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

            await expectRevert(
                proxiable.updateCode(mock2.address),
                "UUPSProxiable: not compatible logic"
            );
        });
    });

    describe("TestResolver", () => {
        const TestResolver = artifacts.require("TestResolver");

        it("TestResolver.set should only be called by admin", async () => {
            const resolver = await TestResolver.new({ from: admin });
            await expectRevert(
                resolver.set("alice", alice, { from: alice }),
                "Caller is not an admin"
            );
            await resolver.grantRole(DEFAULT_ADMIN_ROLE, alice);
            await resolver.set("alice", alice, { from: alice });
            assert.equal(await resolver.get("alice"), alice);
        });
    });

    describe("FullUpgradableSuperTokenProxy", () => {
        const IERC20 = artifacts.require("IERC20");
        const FullUpgradableSuperTokenProxy = artifacts.require(
            "FullUpgradableSuperTokenProxy"
        );

        it("initialization checks", async () => {
            const proxy = await FullUpgradableSuperTokenProxy.new({
                from: admin
            });
            const token = await IERC20.at(proxy.address);
            await expectRevert(
                token.transfer(alice, 1, { from: admin }),
                "Not initialized"
            );
            await proxy.initialize({ from: admin });
            await expectRevert(
                proxy.initialize({ from: admin }),
                "Already initialized"
            );
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
                MAX_INT96_MINUS_1
            );
            await expectRevert(
                tester.testInt96SafeMathMul(MAX_INT96_DIV_2_PLUS_1, 2),
                "testInt96SafeMathMul overflow"
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathMul(2, MIN_INT96_DIV_2)
                ).toString(),
                MIN_INT96
            );
            await expectRevert(
                tester.testInt96SafeMathMul(MIN_INT96_DIV_2_MINUS_1, 2),
                "testInt96SafeMathMul overflow"
            );
            await expectRevert(
                tester.testInt96SafeMathMul("-1", MIN_INT96),
                "testInt96SafeMathMul overflow"
            );

            // testInt96SafeMathAdd
            assert.equal(
                (await tester.echoInt96(MAX_INT96)).toString(),
                MAX_INT96
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathAdd(MAX_INT96_MINUS_1, "1")
                ).toString(),
                MAX_INT96
            );
            await expectRevert(
                tester.testInt96SafeMathAdd(MAX_INT96, "1"),
                "testInt96SafeMathAdd overflow"
            );

            // testInt96SafeMathSub
            assert.equal(
                (await tester.echoInt96(MIN_INT96)).toString(),
                MIN_INT96
            );
            assert.equal(
                (
                    await tester.testInt96SafeMathSub(MIN_INT96_PLUS_1, "1")
                ).toString(),
                MIN_INT96
            );
            await expectRevert(
                tester.testInt96SafeMathSub(MIN_INT96, "1"),
                "testInt96SafeMathSub overflow"
            );
        });

        it("UInt128SafeMath", async () => {
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
            await expectRevert(
                tester.testUInt128SafeMathAdd("1", MAX_UINT128),
                "testUInt128SafeMathAdd overflow"
            );
            await expectRevert(
                tester.testInt128SafeMathSub("0", "1"),
                "testInt96SafeMathSub overflow"
            );
        });
    });
});
