const {expectRevert} = require("@openzeppelin/test-helpers");
const {web3tx} = require("@decentral.ee/web3-helpers");
const SuperfluidGovernanceIIProxy = artifacts.require(
    "SuperfluidGovernanceIIProxy"
);
const SuperfluidGovernanceII = artifacts.require("SuperfluidGovernanceII");

const TestEnvironment = require("../../TestEnvironment");

describe("Superfluid Ownable Governance Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;
    const FAKE_TOKEN_ADDRESS1 = "0x" + "e".repeat(40);
    const FAKE_TOKEN_ADDRESS2 = "0x" + "f".repeat(40);
    const FAKE_ADDRESS1 = "0x" + "1".repeat(40);
    const FAKE_ADDRESS2 = "0x" + "2".repeat(40);
    const onlyOwnerReason = "SFGovII: only owner is authorized";

    let alice;
    let superfluid;
    let governance;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 2,
        });

        ({alice} = t.aliases);
        ({superfluid, governance} = t.contracts);

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
            governance.batchUpdateSuperTokenLogic(superfluid.address, [
                ZERO_ADDRESS,
            ]),
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
            )(newLogic.address, {from: alice});
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
            )(superfluid.address, ZERO_ADDRESS, FAKE_ADDRESS1, {from: alice});
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
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, {from: alice});
            assert.equal(
                await governance.getRewardAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                FAKE_ADDRESS1
            );
        });

        it("#2.2 TrustedForwarders", async () => {
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
            )(superfluid.address, ZERO_ADDRESS, FAKE_ADDRESS1, {from: alice});
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
            )(superfluid.address, ZERO_ADDRESS, FAKE_ADDRESS1, {from: alice});
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

        it("#2.3 PPPConfig", async () => {
            await expectRevert(
                governance.setPPPConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    420,
                    69
                ),
                onlyOwnerReason
            );

            await expectRevert(
                governance.clearPPPConfig(superfluid.address, ZERO_ADDRESS),
                onlyOwnerReason
            );

            await web3tx(
                governance.setPPPConfig,
                "governance.setPPPConfig DEFAULT 420 69"
            )(superfluid.address, ZERO_ADDRESS, 420, 69, {from: alice});
            await web3tx(
                governance.setPPPConfig,
                "governance.setPPPConfig FAKE_TOKEN_ADDRESS1 888 33"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, 888, 33, {
                from: alice,
            });
            let fakeTokenAddress1PPPConfig = await governance.getPPPConfig(
                superfluid.address,
                FAKE_TOKEN_ADDRESS1
            );
            const fakeTokenAddress2PPPConfig = await governance.getPPPConfig(
                superfluid.address,
                FAKE_TOKEN_ADDRESS2
            );
            assert.equal(
                fakeTokenAddress1PPPConfig.liquidationPeriod.toString(),
                "888"
            );
            assert.equal(
                fakeTokenAddress1PPPConfig.patricianPeriod.toString(),
                "33"
            );
            assert.equal(
                fakeTokenAddress2PPPConfig.liquidationPeriod.toString(),
                "420"
            );
            assert.equal(
                fakeTokenAddress2PPPConfig.patricianPeriod.toString(),
                "69"
            );
            await web3tx(
                governance.clearPPPConfig,
                "governance.clearPPPConfig FAKE_TOKEN_ADDRESS1"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, {from: alice});
            fakeTokenAddress1PPPConfig = await governance.getPPPConfig(
                superfluid.address,
                FAKE_TOKEN_ADDRESS1
            );
            assert.equal(
                fakeTokenAddress1PPPConfig.liquidationPeriod.toString(),
                "420"
            );
            assert.equal(
                fakeTokenAddress1PPPConfig.patricianPeriod.toString(),
                "69"
            );
        });
        it("#2.4 SuperTokenMinimumDeposit", async () => {
            await expectRevert(
                governance.setSuperTokenMinimumDeposit(
                    superfluid.address,
                    ZERO_ADDRESS,
                    42069
                ),
                onlyOwnerReason
            );
            await expectRevert(
                governance.clearSuperTokenMinimumDeposit(
                    superfluid.address,
                    ZERO_ADDRESS
                ),
                onlyOwnerReason
            );
            await web3tx(
                governance.setSuperTokenMinimumDeposit,
                "governance.setSuperTokenMinimumDeposit DEFAULT 42069"
            )(superfluid.address, ZERO_ADDRESS, 42069, {from: alice});
            await web3tx(
                governance.setSuperTokenMinimumDeposit,
                "governance.setSuperTokenMinimumDeposit FAKE_TOKEN_ADDRESS1 88833"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, 88833, {
                from: alice,
            });
            assert.equal(
                (
                    await governance.getSuperTokenMinimumDeposit(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS1
                    )
                ).toString(),
                "88833"
            );
            assert.equal(
                (
                    await governance.getSuperTokenMinimumDeposit(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS2
                    )
                ).toString(),
                "42069"
            );

            await web3tx(
                governance.clearSuperTokenMinimumDeposit,
                "governance.clearSuperTokenMinimumDeposit FAKE_TOKEN_ADDRESS1"
            )(superfluid.address, FAKE_TOKEN_ADDRESS1, {from: alice});
            assert.equal(
                (
                    await governance.getSuperTokenMinimumDeposit(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS1
                    )
                ).toString(),
                "42069"
            );

            await expectRevert(
                governance.batchUpdateSuperTokenMinimumDeposit(
                    superfluid.address,
                    [FAKE_TOKEN_ADDRESS1, FAKE_TOKEN_ADDRESS2],
                    [42033, 6988]
                ),
                onlyOwnerReason
            );
            await web3tx(
                governance.batchUpdateSuperTokenMinimumDeposit,
                "governance.batchUpdateSuperTokenMinimumDeposit FAKE_TOKEN_ADDRESS1, FAKE_TOKEN_ADDRESS_2"
            )(
                superfluid.address,
                [FAKE_TOKEN_ADDRESS1, FAKE_TOKEN_ADDRESS2],
                [42033, 6988],
                {from: alice}
            );

            assert.equal(
                (
                    await governance.getSuperTokenMinimumDeposit(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS1
                    )
                ).toString(),
                "42033"
            );
            assert.equal(
                (
                    await governance.getSuperTokenMinimumDeposit(
                        superfluid.address,
                        FAKE_TOKEN_ADDRESS2
                    )
                ).toString(),
                "6988"
            );
        });

        it("#2.5 whiteListNewApp", async () => {
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

        it("#2.6 authorizeAppFactory", async () => {
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
                    {from: alice}
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
                {from: alice}
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
                {from: alice}
            );
            assert.isFalse(
                await governance.isAuthorizedAppFactory(
                    superfluid.address,
                    appFactory.address
                )
            );
        });

        it("#2.7 external set/clear config", async () => {
            const SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY = web3.utils.keccak256(
                "org.superfluid-finance.superfluid.rewardAddress"
            );
            const SUPERTOKEN_MINIMUM_DEPOSIT_KEY = web3.utils.keccak256(
                "org.superfluid-finance.superfluid.superTokenMinimumDeposit"
            );

            // only owner can set config
            await expectRevert(
                governance.setConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY,
                    FAKE_ADDRESS1
                ),
                onlyOwnerReason
            );

            // only owner can clear config
            await expectRevert(
                governance.clearConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY
                ),
                onlyOwnerReason
            );

            await web3tx(
                governance.setConfig,
                "governance.setConfig FAKE_TOKEN_ADDRESS1 SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY FAKE_ADDRESS2"
            )(
                superfluid.address,
                FAKE_TOKEN_ADDRESS1,
                SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY,
                FAKE_ADDRESS2,
                {
                    from: alice,
                }
            );

            assert.equal(
                await governance.getRewardAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                FAKE_ADDRESS2
            );
            assert.equal(
                await governance.getConfigAsAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY
                ),
                FAKE_ADDRESS2
            );

            // clear address value
            await web3tx(
                governance.clearConfig,
                "governance.clearConfig FAKE_TOKEN_ADDRESS1 SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY"
            )(
                superfluid.address,
                FAKE_TOKEN_ADDRESS1,
                SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY,
                {
                    from: alice,
                }
            );

            assert.equal(
                await governance.getRewardAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                FAKE_ADDRESS1
            );

            assert.equal(
                await governance.getConfigAsAddress(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY
                ),
                FAKE_ADDRESS1
            );

            // only owner can set config
            await expectRevert(
                governance.setConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    SUPERTOKEN_MINIMUM_DEPOSIT_KEY,
                    42069
                ),
                onlyOwnerReason
            );

            // only owner can clear config
            await expectRevert(
                governance.clearConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    SUPERTOKEN_MINIMUM_DEPOSIT_KEY
                ),
                onlyOwnerReason
            );

            await web3tx(
                governance.setConfig,
                "governance.setConfig FAKE_TOKEN_ADDRESS1 SUPERTOKEN_MINIMUM_DEPOSIT_KEY 123456"
            )(
                superfluid.address,
                FAKE_TOKEN_ADDRESS1,
                SUPERTOKEN_MINIMUM_DEPOSIT_KEY,
                123456,
                {
                    from: alice,
                }
            );

            assert.equal(
                await governance.getConfigAsUint256(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    SUPERTOKEN_MINIMUM_DEPOSIT_KEY
                ),
                123456
            );
            assert.equal(
                await governance.getSuperTokenMinimumDeposit(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                123456
            );

            await web3tx(
                governance.clearConfig,
                "governance.clearConfig FAKE_TOKEN_ADDRESS1 SUPERTOKEN_MINIMUM_DEPOSIT_KEY"
            )(
                superfluid.address,
                FAKE_TOKEN_ADDRESS1,
                SUPERTOKEN_MINIMUM_DEPOSIT_KEY,
                {
                    from: alice,
                }
            );
            assert.equal(
                await governance.getConfigAsUint256(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1,
                    SUPERTOKEN_MINIMUM_DEPOSIT_KEY
                ),
                42069
            );
            assert.equal(
                await governance.getSuperTokenMinimumDeposit(
                    superfluid.address,
                    FAKE_TOKEN_ADDRESS1
                ),
                42069
            );
        });
    });
});
