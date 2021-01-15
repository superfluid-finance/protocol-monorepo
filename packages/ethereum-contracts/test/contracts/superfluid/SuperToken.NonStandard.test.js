const { expectRevert } = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad,
    toDecimals,
    toBN
} = require("@decentral.ee/web3-helpers");

const TestToken = artifacts.require("TestToken");
const ERC777SenderRecipientMock = artifacts.require(
    "ERC777SenderRecipientMock"
);

const TestEnvironment = require("../../TestEnvironment");

contract("SuperToken's Non Standard Functions", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 4), {
        isTruffle: true,
        useMocks: true
    });
    const { admin, alice, bob } = t.aliases;
    const { MAX_UINT256, ZERO_ADDRESS } = t.constants;

    let superfluid;
    let testToken;
    let superToken;

    before(async () => {
        await t.reset();
    });

    beforeEach(async function() {
        await t.createNewToken({ doUpgrade: false });
        ({ superfluid, testToken, superToken } = t.contracts);
    });

    describe("#1 upgradability", () => {
        it("#1.1 storage layout", async () => {
            await superToken.validateStorageLayout.call();
        });

        it("#1.2 proxiable info", async () => {
            assert.equal(
                await superToken.proxiableUUID.call(),
                web3.utils.sha3(
                    "org.superfluid-finance.contracts.SuperToken.implementation"
                )
            );
        });

        it("#1.3 only host can update the code", async () => {
            await expectRevert(
                superToken.updateCode(ZERO_ADDRESS),
                "only host can update code"
            );
        });

        it("#1.4 only can initialize once", async () => {
            await expectRevert(
                superToken.initialize(ZERO_ADDRESS, 18, "name", "symbol"),
                "Initializable: contract is already initialized"
            );
        });
    });

    describe("#2 SuperToken.upgrade/downgrade", () => {
        it("#2.1 - should upgrade if enough balance", async () => {
            const initialBalance = await testToken.balanceOf.call(alice);

            await web3tx(
                superToken.upgrade,
                "SuperToken.upgrade 2.0 tokens from alice"
            )(toWad(2), {
                from: alice
            });
            const { timestamp } = await web3.eth.getBlock("latest");

            const finalBalance = await testToken.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );
            const finalRealBalance = await superToken.realtimeBalanceOf.call(
                alice,
                timestamp
            );

            assert.equal(
                finalSuperTokenBalance.toString(),
                toWad(2).toString(),
                "SuperToken.balanceOf is wrong"
            );
            assert.equal(
                initialBalance.sub(finalBalance).toString(),
                toWad(2).toString(),
                "SuperToken.upgrade should manage underlying tokens"
            );
            assert.equal(
                finalRealBalance.availableBalance.toString(),
                finalSuperTokenBalance.toString(),
                "balanceOf should equal realtimeBalanceOf"
            );

            await t.validateSystemInvariance();
        });

        it("#2.2 - should not upgrade without enough underlying balance", async () => {
            const initialBalance = await testToken.balanceOf.call(alice);
            await expectRevert(
                web3tx(
                    superToken.upgrade,
                    "SuperToken.upgrade - bad balance"
                )(initialBalance.add(toBN(1)), { from: alice }),
                "ERC20: transfer amount exceeds balance"
            );
            await t.validateSystemInvariance();
        });

        it("#2.3 - should downgrade by single account", async () => {
            const initialBalance = await testToken.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice")(
                toWad(2),
                {
                    from: alice
                }
            );

            await web3tx(
                superToken.downgrade,
                "SuperToken.downgrade 2 from alice"
            )(toWad(1), {
                from: alice
            });

            const finalBalance = await testToken.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );

            assert.isOk(
                initialBalance.sub(finalBalance).toString(),
                toWad(1),
                "TestToken.balanceOf should recover after downgrade"
            );
            assert.equal(
                finalSuperTokenBalance.toString(),
                toWad("1"),
                "SuperToken.balanceOf is wrong"
            );

            await t.validateSystemInvariance();
        });

        it("#2.4 - should downgrade by multiple accounts", async () => {
            const initialBalanceAlice = await testToken.balanceOf.call(alice);
            const initialSuperBalanceAlice = await superToken.balanceOf.call(
                alice
            );

            await web3tx(superToken.upgrade, "upgrade 2 from alice")(toWad(2), {
                from: alice
            });
            await web3tx(superToken.upgrade, "upgrade 1 from bob")(toWad(1), {
                from: bob
            });

            const initialSuperBalanceBob = await superToken.balanceOf.call(bob);

            await web3tx(superToken.downgrade, "downgrade 2 from alice")(
                toWad(2),
                {
                    from: alice
                }
            );

            const finalBalanceAlice = await testToken.balanceOf.call(alice);
            const finalSuperBalanceAlice = await superToken.balanceOf.call(
                alice
            );
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(
                initialBalanceAlice.toString(),
                finalBalanceAlice.toString(),
                "TestToken.balanceOf - not correct for alice"
            );
            assert.equal(
                initialSuperBalanceAlice.toString(),
                finalSuperBalanceAlice.toString(),
                "SuperToken.balanceOf - not correct for user 1"
            );
            assert.equal(
                initialSuperBalanceBob.toString(),
                finalSuperBalanceBob.toString(),
                "SuperToken.balanceOf - not correct for user 2"
            );

            await t.validateSystemInvariance();
        });

        it("#2.5 - should not downgrade if there is no balance", async () => {
            await expectRevert(
                web3tx(
                    superToken.downgrade,
                    "SuperToken.downgrade - bad balance"
                )(toBN(1), {
                    from: alice
                }),
                "SuperfluidToken: burn amount exceeds balance"
            );
        });

        it("#2.6 - should convert from smaller underlying decimals", async () => {
            const token6D = await web3tx(TestToken.new, "TestToken.new")(
                "Test Token 6 Decimals",
                "TEST6D",
                6,
                {
                    from: bob
                }
            );
            await web3tx(token6D.mint, "Mint testToken for bob")(
                bob,
                toDecimals("100", 6),
                {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("100", 6)
            );

            const superToken6D = await t.sf.createERC20Wrapper(token6D);
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                "0"
            );

            await web3tx(
                token6D.approve,
                "TestToken.approve - from bob to SuperToken"
            )(superToken6D.address, MAX_UINT256, {
                from: bob
            });

            await web3tx(superToken6D.upgrade, "upgrade 1 from bob")(toWad(1), {
                from: bob
            });
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad(1).toString()
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("99", 6)
            );

            await web3tx(superToken6D.upgrade, "upgrade 0.1234567 from bob")(
                toWad("0.1234567"),
                {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("98.876544", 6)
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("1.123456").toString()
            );

            await web3tx(superToken6D.downgrade, "downgrade from bob")(
                toWad(1),
                {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("99.876544", 6)
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0.123456").toString()
            );

            // extra decimals should be discarded due to precision issue
            await web3tx(
                superToken6D.downgrade,
                "downgrade extra decimals from bob"
            )(toWad("0.10000012345"), {
                from: bob
            });
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("99.976544", 6)
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0.023456").toString()
            );

            // downgrade the rest
            await web3tx(superToken6D.downgrade, "downgrade the rest from bob")(
                toWad("0.023456"),
                {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("100", 6)
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0").toString()
            );
        });

        it("#2.7 - should convert from larger underlying decimals", async () => {
            const token20D = await web3tx(TestToken.new, "TestToken.new")(
                "Test Token 20 Decimals",
                "TEST20D",
                20,
                {
                    from: bob
                }
            );
            await web3tx(token20D.mint, "Mint testToken for bob")(
                bob,
                toDecimals("100", 20),
                {
                    from: bob
                }
            );
            assert.equal(
                (await token20D.balanceOf.call(bob)).toString(),
                toDecimals("100", 20)
            );

            const superToken6D = await t.sf.createERC20Wrapper(token20D);
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                "0"
            );

            await web3tx(
                token20D.approve,
                "TestToken.approve - from bob to SuperToken"
            )(superToken6D.address, MAX_UINT256, {
                from: bob
            });

            await web3tx(superToken6D.upgrade, "upgrade 1 from bob")(toWad(1), {
                from: bob
            });
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad(1).toString()
            );
            assert.equal(
                (await token20D.balanceOf.call(bob)).toString(),
                toDecimals("99", 20)
            );

            await web3tx(superToken6D.downgrade, "downgrade 1 from bob")(
                toWad(1),
                {
                    from: bob
                }
            );
            assert.equal(
                (await token20D.balanceOf.call(bob)).toString(),
                toDecimals("100", 20)
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0").toString()
            );
        });

        it("#2.8 - should upgradeTo if enough balance", async () => {
            const initialBalanceAlice = await testToken.balanceOf.call(alice);
            const initialBalanceBob = await testToken.balanceOf.call(bob);

            await web3tx(
                superToken.upgradeTo,
                "SuperToken.upgrade 2.0 tokens from alice to bob"
            )(bob, toWad(2), "0x", {
                from: alice
            });
            const { timestamp } = await web3.eth.getBlock("latest");

            const finalBalanceAlice = await testToken.balanceOf.call(alice);
            const finalSuperTokenBalanceAlice = await superToken.balanceOf.call(
                alice
            );
            const finalRealBalanceAlice = await superToken.realtimeBalanceOf.call(
                alice,
                timestamp
            );

            const finalBalanceBob = await testToken.balanceOf.call(bob);
            const finalSuperTokenBalanceBob = await superToken.balanceOf.call(
                bob
            );
            const finalRealBalanceBob = await superToken.realtimeBalanceOf.call(
                bob,
                timestamp
            );

            assert.equal(
                initialBalanceAlice.sub(finalBalanceAlice).toString(),
                toWad(2).toString(),
                "(alice) SuperToken.upgradeTo should manage underlying tokens"
            );
            assert.equal(
                finalSuperTokenBalanceAlice.toString(),
                "0",
                "(alice) SuperToken.balanceOf is wrong"
            );
            assert.equal(
                finalRealBalanceAlice.availableBalance.toString(),
                finalSuperTokenBalanceAlice.toString(),
                "(alice) balanceOf should equal realtimeBalanceOf"
            );

            assert.equal(
                initialBalanceBob.sub(finalBalanceBob).toString(),
                "0",
                "(bob) SuperToken.upgradeTo should not affect recipient"
            );
            assert.equal(
                finalSuperTokenBalanceBob.toString(),
                toWad(2).toString(),
                "(bob) SuperToken.balanceOf is wrong"
            );
            assert.equal(
                finalRealBalanceBob.availableBalance.toString(),
                finalSuperTokenBalanceBob.toString(),
                "(bob) balanceOf should equal realtimeBalanceOf"
            );

            await t.validateSystemInvariance();
        });

        it("#2.9 - upgradeTo should trigger tokensReceived", async () => {
            const mock = await ERC777SenderRecipientMock.new();
            await expectRevert(
                superToken.upgradeTo(mock.address, toWad(2), "0x", {
                    from: alice
                }),
                "SuperToken: not an ERC777TokensRecipient"
            );
            await web3tx(
                mock.registerRecipient,
                "registerRecipient"
            )(mock.address);
            await web3tx(
                superToken.upgradeTo,
                "SuperToken.upgrade 2.0 tokens from alice to bob"
            )(mock.address, toWad(2), "0x", {
                from: alice
            });
        });

        it("#2.10 upgrade and self-upgradeTo should not trigger tokenReceived", async () => {
            const mock = await ERC777SenderRecipientMock.new();
            await web3tx(testToken.transfer, "send token from alice to mock")(
                mock.address,
                toWad(2),
                {
                    from: alice
                }
            );
            await web3tx(
                mock.upgradeAll,
                "mock.upgradeAll"
            )(superToken.address);
            assert.equal(
                (await superToken.balanceOf.call(mock.address)).toString(),
                toWad(2).toString()
            );
            await web3tx(testToken.transfer, "send token from alice to mock")(
                mock.address,
                toWad(2),
                {
                    from: alice
                }
            );
            await web3tx(
                mock.upgradeAllToSelf,
                "mock.upgradeAllToSelf"
            )(superToken.address);
            assert.equal(
                (await superToken.balanceOf.call(mock.address)).toString(),
                toWad(4).toString()
            );
        });
    });

    describe("#3 SuperToken custom token support", () => {
        const ISuperTokenFactory = artifacts.require("ISuperTokenFactory");
        const CustomSuperTokenMock = artifacts.require("CustomSuperTokenMock");
        const CustomSuperTokenProxyMock = artifacts.require(
            "CustomSuperTokenProxyMock"
        );

        let customToken;

        beforeEach(async () => {
            customToken = await CustomSuperTokenMock.at(
                (
                    await web3tx(
                        CustomSuperTokenProxyMock.new,
                        "CustomSuperTokenProxyMock.new"
                    )(superfluid.address)
                ).address
            );
            const factory = await ISuperTokenFactory.at(
                await superfluid.getSuperTokenFactory()
            );
            await web3tx(
                factory.initializeCustomSuperToken,
                "initializeCustomSuperToken"
            )(customToken.address);
        });

        it("#3.1 Custom token storage should not overlap with super token", async () => {
            const a = await superToken.getLastSuperTokenStorageSlot();
            const b = await customToken.getFirstCustomTokenStorageSlot();
            console.log("lastSuperTokenStorageSlot", a.toString());
            console.log("firstCustomTokenStorageSlot", b.toString());
            assert.equal(Number(a.toString()) + 1, Number(b.toString()));
        });

        it("#3.2 Custom token functions can only be called by self", async () => {
            const reason = "SuperToken: only self allowed";
            await expectRevert(superToken.selfMint(alice, 100, "0x"), reason);
            await expectRevert(superToken.selfBurn(alice, 100, "0x"), reason);
        });

        it("#3.3 Custom token that mints/burn and disabling upgrade/downgrade", async () => {
            await expectRevert(
                customToken.upgrade(100),
                "SuperToken: no underlying token"
            );
            await expectRevert(
                customToken.downgrade(100),
                "SuperToken: no underlying token"
            );
            await web3tx(customToken.initialize, "customToken.initialize")(
                ZERO_ADDRESS,
                0,
                "Custom SuperTestToken",
                "CSTT"
            );

            await web3tx(customToken.selfMint, "customToken.selfMint")(
                alice,
                100,
                "0x"
            );
            assert.equal(
                (await customToken.balanceOf(alice)).toString(),
                "100"
            );
            assert.equal((await customToken.totalSupply()).toString(), "100");

            await expectRevert(
                customToken.callSelfBurn(alice, 101, "0x"),
                "SuperfluidToken: burn amount exceeds balance"
            );

            await web3tx(customToken.callSelfBurn, "customToken.callSelfBurn")(
                alice,
                100,
                "0x"
            );
            assert.equal((await customToken.balanceOf(alice)).toString(), "0");
            assert.equal((await customToken.totalSupply()).toString(), "0");
        });
    });

    describe("#10 misc", () => {
        it("#10.1 should return underlying token", async () => {
            assert.equal(
                await superToken.getUnderlyingToken.call(),
                t.contracts.testToken.address
            );
        });

        it("#10.2 transferAll", async () => {
            await t.upgradeBalance("alice", toWad(2));
            assert.equal(
                (await superToken.balanceOf.call(alice)).toString(),
                toWad(2).toString()
            );
            await web3tx(superToken.transferAll, "superToken.transferAll")(
                bob,
                { from: alice }
            );
            assert.equal(await superToken.balanceOf.call(alice), "0");
            assert.equal(
                (await superToken.balanceOf.call(bob)).toString(),
                toWad(2).toString()
            );
        });

        it("#10.3 batchCall should only be called by host", async function() {
            await expectRevert(
                superToken.operationApprove(alice, bob, "0"),
                "SuperfluidToken: Only host contract allowed"
            );
            await expectRevert(
                superToken.operationTransferFrom(alice, bob, admin, "0"),
                "SuperfluidToken: Only host contract allowed"
            );
            await expectRevert(
                superToken.operationUpgrade(alice, "0"),
                "SuperfluidToken: Only host contract allowed"
            );
            await expectRevert(
                superToken.operationDowngrade(alice, "0"),
                "SuperfluidToken: Only host contract allowed"
            );
        });
    });
});
