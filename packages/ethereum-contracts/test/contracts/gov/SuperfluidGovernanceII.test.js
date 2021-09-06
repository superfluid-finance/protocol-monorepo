const { expectRevert } = require("@openzeppelin/test-helpers");
const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidGovernanceIIProxy = artifacts.require(
    "SuperfluidGovernanceIIProxy"
);
const SuperfluidGovernanceII = artifacts.require("SuperfluidGovernanceII");

const TestEnvironment = require("../../TestEnvironment");

contract("Superfluid Ownable Governance Contract", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 2), {
        isTruffle: true,
        useMocks: true,
    });
    const { alice } = t.aliases;
    const { ZERO_ADDRESS } = t.constants;
    const FAKE_TOKEN_ADDRESS1 = "0x" + "e".repeat(40);
    const FAKE_TOKEN_ADDRESS2 = "0x" + "f".repeat(40);
    const FAKE_ADDRESS1 = "0x" + "1".repeat(40);
    const FAKE_ADDRESS2 = "0x" + "2".repeat(40);
    const onlyOwnerReason = "SFGovII: only owner is authorized";

    let superfluid;
    let governance;

    before(async () => {
        await t.reset();
        ({ governance, superfluid } = t.contracts);
        const newGovProxy = await SuperfluidGovernanceIIProxy.new({
            from: alice,
        });
        const newGovLogic = await SuperfluidGovernanceII.new();
        await newGovProxy.initializeProxy(newGovLogic.address);
        await web3tx(
            governance.replaceGovernance,
            "governance.replaceGovernance"
        )(superfluid.address, newGovProxy.address);
        assert.equal(
            await superfluid.getGovernance.call(),
            newGovProxy.address
        );
        governance = await SuperfluidGovernanceII.at(newGovProxy.address);
    });

    it("#0.1 authorization checks", async () => {
        await expectRevert(
            governance.replaceGovernance(superfluid.address, ZERO_ADDRESS),
            onlyOwnerReason
        );
        await expectRevert(
            governance.registerAgreementClass(superfluid.address, ZERO_ADDRESS),
            onlyOwnerReason
        );
        await expectRevert(
            governance.updateContracts(
                superfluid.address,
                ZERO_ADDRESS,
                [],
                ZERO_ADDRESS
            ),
            onlyOwnerReason
        );
        await expectRevert(
            governance.updateSuperTokenLogic(superfluid.address, ZERO_ADDRESS),
            onlyOwnerReason
        );

        await expectRevert(
            governance.updateCode(FAKE_ADDRESS1),
            onlyOwnerReason
        );
    });

    describe("#1 upgradability", () => {
        it("#1.1 storage layout", async () => {
            const T = artifacts.require(
                "SuperfluidGovernanceIIUpgradabilityTester"
            );
            const tester = await T.new();
            await tester.validateStorageLayout.call();
        });

        it("#1.2 proxiable info", async () => {
            assert.equal(
                await governance.proxiableUUID.call(),
                web3.utils.sha3(
                    "org.superfluid-finance.contracts.SuperfluidGovernanceII.implementation"
                )
            );
        });

        it("#1.3 update the code", async () => {
            const newLogic = await SuperfluidGovernanceII.new();

            await web3tx(
                governance.updateCode,
                "governance.updateCode to new logic contract"
            )(newLogic.address, { from: alice });
            assert.equal(await governance.getCodeAddress(), newLogic.address);
        });

        it("#1.4 initializeProxy can't be invoked again", async () => {
            const govProxy = await SuperfluidGovernanceIIProxy.at(
                governance.address
            );
            const newGovLogic = await SuperfluidGovernanceII.new();
            await expectRevert(
                govProxy.initializeProxy(newGovLogic.address),
                "UUPSProxy: already initialized"
            );
        });
    });

    describe("#2 configurations", () => {
        it("#2.1 RewardAddress", async () => {
            await expectRevert(
                governance.setRewardAddress(
                    superfluid.address,
                    ZERO_ADDRESS,
                    FAKE_ADDRESS1
                ),
                onlyOwnerReason
            );
            await expectRevert(
                governance.clearRewardAddress(superfluid.address, ZERO_ADDRESS),
                onlyOwnerReason
            );

            await web3tx(
                governance.setRewardAddress,
                "governance.setRewardAddress DEFAULT FAKE_ADDRESS1"
            )(superfluid.address, ZERO_ADDRESS, FAKE_ADDRESS1, { from: alice });
            await web3tx(
                governance.setRewardAddress,
                "governance.setRewardAddress FAKE_TOKEN_ADDRESS1 FAKE_ADDRESS2"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, FAKE_ADDRESS2, {
                from: alice,
            });
            assert.equal(
                await governance.getRewardAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                FAKE_ADDRESS2
            );
            assert.equal(
                await governance.getRewardAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2
                ),
                FAKE_ADDRESS1
            );

            await web3tx(
                governance.clearRewardAddress,
                "governance.clearRewardAddress FAKE_TOKEN_ADDRESS1"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, { from: alice });
            assert.equal(
                await governance.getRewardAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                FAKE_ADDRESS1
            );
        });

        it("#2.2 CFAv1LiquidationPeriod", async () => {
            await expectRevert(
                governance.setCFAv1LiquidationPeriod(
                    superfluid.address,
                    ZERO_ADDRESS,
                    42
                ),
                onlyOwnerReason
            );
            await expectRevert(
                governance.clearCFAv1LiquidationPeriod(
                    superfluid.address,
                    ZERO_ADDRESS
                ),
                onlyOwnerReason
            );

            await web3tx(
                governance.setCFAv1LiquidationPeriod,
                "governance.setCFAv1LiquidationPeriod DEFAULT 42"
            )(superfluid.address, ZERO_ADDRESS, 42, { from: alice });
            await web3tx(
                governance.setCFAv1LiquidationPeriod,
                "governance.setCFAv1LiquidationPeriod FAKE_TOKEN_ADDRESS1 888"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, 888, {
                from: alice,
            });
            assert.equal(
                (
                    await governance.getCFAv1LiquidationPeriod(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS1
                    )
                ).toString(),
                "888"
            );
            assert.equal(
                (
                    await governance.getCFAv1LiquidationPeriod(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS2
                    )
                ).toString(),
                "42"
            );

            await web3tx(
                governance.clearCFAv1LiquidationPeriod,
                "governance.clearCFAv1LiquidationPeriod FAKE_TOKEN_ADDRESS1"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, { from: alice });
            assert.equal(
                (
                    await governance.getCFAv1LiquidationPeriod(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS1
                    )
                ).toString(),
                "42"
            );
        });

        it("#2.3 TrustedForwarders", async () => {
            await expectRevert(
                governance.enableTrustedForwarder(
                    superfluid.address,
                    ZERO_ADDRESS,
                    FAKE_ADDRESS1
                ),
                onlyOwnerReason
            );
            await expectRevert(
                governance.clearTrustedForwarder(
                    superfluid.address,
                    ZERO_ADDRESS,
                    FAKE_ADDRESS1
                ),
                onlyOwnerReason
            );

            await web3tx(
                governance.enableTrustedForwarder,
                "governance.enableTrustedForwarder DEFAULT FAKE_ADDRESS1"
            )(superfluid.address, ZERO_ADDRESS, FAKE_ADDRESS1, { from: alice });
            await web3tx(
                governance.enableTrustedForwarder,
                "governance.enableTrustedForwarder FAKE_TOKEN_ADDRESS1 FAKE_ADDRESS2"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, FAKE_ADDRESS2, {
                from: alice,
            });
            assert.isTrue(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    FAKE_ADDRESS1
                )
            );
            assert.isTrue(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    FAKE_ADDRESS2
                )
            );
            assert.isTrue(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2,
                    FAKE_ADDRESS1
                )
            );
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2,
                    FAKE_ADDRESS2
                )
            );

            await web3tx(
                governance.clearTrustedForwarder,
                "governance.clearTrustedForwarder FAKE_TOKEN_ADDRESS1 FAKE_ADDRESS2"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, FAKE_ADDRESS2, {
                from: alice,
            });
            assert.isTrue(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    FAKE_ADDRESS1
                )
            );
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    FAKE_ADDRESS2
                )
            );
            assert.isTrue(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2,
                    FAKE_ADDRESS1
                )
            );
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2,
                    FAKE_ADDRESS2
                )
            );

            await web3tx(
                governance.disableTrustedForwarder,
                "governance.disableTrustedForwarder DEFAULT FAKE_ADDRESS1"
            )(superfluid.address, ZERO_ADDRESS, FAKE_ADDRESS1, { from: alice });
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    FAKE_ADDRESS1
                )
            );
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    FAKE_ADDRESS2
                )
            );
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2,
                    FAKE_ADDRESS1
                )
            );
            assert.isFalse(
                await governance.isTrustedForwarder(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS2,
                    FAKE_ADDRESS2
                )
            );
        });

        it("#2.4 whiteListNewApp", async () => {
            await expectRevert(
                governance.whiteListNewApp(
                    superfluid.address,
                    web3.utils.sha3("test")
                ),
                onlyOwnerReason
            );
            await governance.whiteListNewApp(
                superfluid.address,
                web3.utils.sha3("test"),
                {
                    from: alice,
                }
            );
        });

        it("#2.5 authorizeAppFactory", async () => {
            const SuperAppFactoryMock = artifacts.require(
                "SuperAppFactoryMock"
            );
            const appFactory = await SuperAppFactoryMock.new();

            // checks for authorize
            await expectRevert(
                governance.authorizeAppFactory(
                    superfluid.address,
                    appFactory.address
                ),
                onlyOwnerReason
            );

            await expectRevert(
                governance.authorizeAppFactory(
                    superfluid.address,
                    FAKE_ADDRESS1,
                    { from: alice }
                ),
                "SFGov: factory must be a contract"
            );

            assert.isFalse(
                await governance.isAuthorizedAppFactory(
                    superfluid.address,
                    appFactory.address
                )
            );
            await governance.authorizeAppFactory(
                superfluid.address,
                appFactory.address,
                { from: alice }
            );
            assert.isTrue(
                await governance.isAuthorizedAppFactory(
                    superfluid.address,
                    appFactory.address
                )
            );

            // checks for unauthorize
            await expectRevert(
                governance.unauthorizeAppFactory(
                    superfluid.address,
                    appFactory.address
                ),
                onlyOwnerReason
            );
            await governance.unauthorizeAppFactory(
                superfluid.address,
                appFactory.address,
                { from: alice }
            );
            assert.isFalse(
                await governance.isAuthorizedAppFactory(
                    superfluid.address,
                    appFactory.address
                )
            );
        });
    });
});
