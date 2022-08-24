const {ethers} = require("hardhat");
const TestEnvironment = require("../../TestEnvironment");
const {
    expectRevertedWith,
    expectCustomError,
} = require("../../utils/expectRevert");

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
        let UUPSProxy, UUPSProxiableMock;

        before(async () => {
            UUPSProxiableMock = await ethers.getContractFactory(
                "UUPSProxiableMock"
            );
            UUPSProxy = await ethers.getContractFactory("UUPSProxy");
        });

        it("UUPSProxy", async () => {
            const proxy = await UUPSProxy.deploy();
            const proxiable = await UUPSProxiableMock.attach(proxy.address);
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const mock = await UUPSProxiableMock.deploy(uuid1, 1);
            await expectCustomError(
                proxy.initializeProxy(ZERO_ADDRESS),
                proxy,
                "UUPSProxy_ZeroAddress"
            );
            await proxy.initializeProxy(mock.address);
            await expectCustomError(
                proxy.initializeProxy(mock.address),
                proxy,
                "UUPSProxy_AlreadyInitialized"
            );
            assert.equal(await proxiable.proxiableUUID(), uuid1);
        });

        it("UUPSProxiable", async () => {
            const proxy = await UUPSProxy.deploy();
            const proxiable = await UUPSProxiableMock.attach(proxy.address);
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const uuid2 = web3.utils.sha3("UUPSProxiableMock2");
            const mock1a = await UUPSProxiableMock.deploy(uuid1, 1);
            const mock1b = await UUPSProxiableMock.deploy(uuid1, 2);
            const mock2 = await UUPSProxiableMock.deploy(uuid2, 1);

            assert.equal(await mock1a.getCodeAddress(), ZERO_ADDRESS);
            await expectCustomError(
                mock1a.updateCode(mock1a.address),
                proxiable,
                "UUPSProxiable_NotUpgradeable"
            );
            await proxiable.updateCode(mock1a.address);

            await proxy.initializeProxy(mock1a.address);
            assert.equal(await proxiable.proxiableUUID(), uuid1);
            assert.equal(await proxiable.waterMark(), 1);

            await proxiable.updateCode(mock1b.address);
            assert.equal(await proxiable.proxiableUUID(), uuid1);
            assert.equal(await proxiable.waterMark(), 2);

            await expectCustomError(
                proxiable.updateCode(mock2.address),
                proxiable,
                "UUPSProxiable_IncompatibleLogic"
            );

            await expectCustomError(
                proxiable.updateCode(proxiable.address),
                proxiable,
                "UUPSProxiable_ProxyLoop"
            );
        });

        it("Can't initialize castrated UUPSProxiable", async () => {
            const uuid1 = web3.utils.sha3("UUPSProxiableMock1");
            const mock1 = await UUPSProxiableMock.deploy(uuid1, 1);

            const uuid2 = web3.utils.sha3("UUPSProxiableMock2");
            const mock2 = await UUPSProxiableMock.deploy(uuid2, 1);

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
