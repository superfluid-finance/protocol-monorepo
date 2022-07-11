const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

describe("Miscellaneous for test coverages", function () {
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
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

        it("Can't initialize castrated UUPSProxiable", async () => {
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const mock1 = await UUPSProxiableMock.new(uuid1, 1);

            const uuid2 = web3.utils.sha3("UUPSProxiableMock2");
            const mock2 = await UUPSProxiableMock.new(uuid2, 1);

            // can initialize if not castrated
            await mock1.initialize();

            // cannot initialize if castrated
            await mock2.castrate();
            await expectRevertedWith(
                mock2.initialize(),
                "Initializable: contract is already initialized"
            );
        });
    });
});
