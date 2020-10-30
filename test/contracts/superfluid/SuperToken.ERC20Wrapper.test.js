const {
    expectRevert,
} = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad,
    toDecimals,
    toBN
} = require("@decentral.ee/web3-helpers");

const TestToken = artifacts.require("TestToken");
const ISuperToken = artifacts.require("ISuperToken");

const TestEnvironment = require("../../TestEnvironment");

contract("SuperToken's ERC20 Wrapper implementation", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 4));
    const { alice, bob } = t.aliases;
    const { MAX_UINT256 } = t.constants;

    let testToken;
    let superToken;
    let superfluid;

    before(async () => {
        await t.reset();
        ({
            superfluid,
        } = t.contracts);
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({
            testToken,
            superToken,
        } = t.contracts);
    });

    describe("#1 SuperToken wrapper basics", () => {
        it("#1.1 - should support upgradability", async () => {
            assert.equal(await superToken.proxiableUUID.call(),
                web3.utils.sha3("org.superfluid-finance.contracts.SuperToken.implementation"));
        });

        it("#1.2 should have immutable storage layout", async () => {
            const SuperTokenMock = artifacts.require("SuperTokenMock");
            const tester = await SuperTokenMock.new();
            await tester.validateStorageLayout.call();
        });

        it("#1.3 should return underlying token", async () => {
            assert.equal(await superToken.getUnderlyingToken.call(),
                t.contracts.testToken.address);
        });
    });

    describe("#2 SuperToken.upgrade/downgrade", () => {
        it("#2.1 - should upgrade if enough balance", async () => {
            const initialBalance = await testToken.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2.0 tokens from alice") (
                toWad(2), {
                    from: alice
                });
            const { timestamp } = await web3.eth.getBlock("latest");

            const finalBalance = await testToken.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(alice);
            const finalRealBalance = await superToken.realtimeBalanceOf.call(alice, timestamp);

            assert.equal(finalSuperTokenBalance.toString(), toWad(2).toString(),
                "SuperToken.balanceOf is wrong");
            assert.equal(initialBalance.sub(finalBalance).toString(), toWad(2).toString(),
                "SuperToken.upgrade should manage underlying tokens");
            assert.equal(finalRealBalance.availableBalance.toString(), finalSuperTokenBalance.toString(),
                "balanceOf should equal realtimeBalanceOf");

            await t.validateSystem();
        });

        it("#2.2 - should not upgrade without enough underlying balance", async() => {
            const initialBalance = await testToken.balanceOf.call(alice);
            await expectRevert(web3tx(superToken.upgrade, "SuperToken.upgrade - bad balance")(
                initialBalance.add(toBN(1)), {from: alice}), "ERC20: transfer amount exceeds balance");
            await t.validateSystem();
        });

        it("#2.3 - should downgrade by single account", async() => {
            const initialBalance = await testToken.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });

            await web3tx(superToken.downgrade, "SuperToken.downgrade 2 from alice") (
                toWad(1), {
                    from: alice
                });

            const finalBalance = await testToken.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(alice);

            assert.isOk(initialBalance.sub(finalBalance).toString(), toWad(1),
                "TestToken.balanceOf should recover after downgrade");
            assert.equal(finalSuperTokenBalance.toString(), toWad("1"),
                "SuperToken.balanceOf is wrong");

            await t.validateSystem();
        });

        it("#2.4 - should downgrade by multiple accounts", async () => {
            const initialBalanceAlice = await testToken.balanceOf.call(alice);
            const initialSuperBalanceAlice = await superToken.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "upgrade 2 from alice")(toWad(2), {from: alice});
            await web3tx(superToken.upgrade, "upgrade 1 from bob")(toWad(1), {from: bob});

            const initialSuperBalanceBob = await superToken.balanceOf.call(bob);

            await web3tx(superToken.downgrade, "downgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });

            const finalBalanceAlice = await testToken.balanceOf.call(alice);
            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(
                initialBalanceAlice.toString(),
                finalBalanceAlice.toString(),
                "TestToken.balanceOf - not correct for alice");
            assert.equal(
                initialSuperBalanceAlice.toString(),
                finalSuperBalanceAlice.toString(),
                "SuperToken.balanceOf - not correct for user 1");
            assert.equal(
                initialSuperBalanceBob.toString(),
                finalSuperBalanceBob.toString(),
                "SuperToken.balanceOf - not correct for user 2");

            await t.validateSystem();
        });

        it("#2.5 - should not downgrade if there is no balance", async () => {
            await expectRevert(web3tx(superToken.downgrade, "SuperToken.downgrade - bad balance")(
                toBN(1), {
                    from: alice
                }), "SuperToken: downgrade amount exceeds balance");
        });

        it("#2.6 - should convert from smaller underlying decimals", async () => {
            const token6D = await web3tx(TestToken.new, "TestToken.new")(
                "Test Token 6 Decimals", "TEST6D",
                {
                    from: bob
                });
            await web3tx(token6D.mint, "Mint testToken for bob")(
                bob,
                toDecimals("100", 6), {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("100", 6));

            superfluid.createERC20Wrapper(
                token6D.address,
                6,
                "Super Test Token 6D",
                "TEST6Dx",
            );
            const superToken6D = await ISuperToken.at(
                (await superfluid.getERC20Wrapper.call(
                    token6D.address,
                    "TEST6Dx"
                )).wrapperAddress
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                "0");

            await web3tx(token6D.approve, "TestToken.approve - from bob to SuperToken")(
                superToken6D.address,
                MAX_UINT256, {
                    from: bob
                }
            );

            await web3tx(superToken6D.upgrade, "upgrade 1 from bob")(
                toWad(1), {
                    from: bob
                }
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad(1).toString());
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("99", 6));

            await web3tx(superToken6D.upgrade, "upgrade 0.1234567 from bob")(
                toWad("0.1234567"), {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("98.876544", 6));
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("1.123456").toString());

            await web3tx(superToken6D.downgrade, "downgrade 1 from bob")(
                toWad(1), {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("99.876544", 6));
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0.123456").toString());

            await expectRevert(superToken6D.downgrade(
                toWad("0.1234561"), {
                    from: bob
                }
            ), "SuperToken: downgrade amount exceeds balance");
            await web3tx(superToken6D.downgrade, "downgrade 1 from bob")(
                toWad("0.123456"), {
                    from: bob
                }
            );
            assert.equal(
                (await token6D.balanceOf.call(bob)).toString(),
                toDecimals("100", 6));
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0").toString());
        });

        it("#2.7 - should convert from larger underlying decimals", async () => {
            const token20D = await web3tx(TestToken.new, "TestToken.new")(
                "Test Token 20 Decimals", "TEST20D",
                {
                    from: bob
                });
            await web3tx(token20D.mint, "Mint testToken for bob")(
                bob,
                toDecimals("100", 20), {
                    from: bob
                }
            );
            assert.equal(
                (await token20D.balanceOf.call(bob)).toString(),
                toDecimals("100", 20));

            superfluid.createERC20Wrapper(
                token20D.address,
                20,
                "Super Test Token 20D",
                "TEST20Dx",
            );
            const superToken6D = await ISuperToken.at(
                (await superfluid.getERC20Wrapper.call(
                    token20D.address,
                    "TEST20Dx"
                )).wrapperAddress
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                "0");

            await web3tx(token20D.approve, "TestToken.approve - from bob to SuperToken")(
                superToken6D.address,
                MAX_UINT256, {
                    from: bob
                }
            );

            await web3tx(superToken6D.upgrade, "upgrade 1 from bob")(
                toWad(1), {
                    from: bob
                }
            );
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad(1).toString());
            assert.equal(
                (await token20D.balanceOf.call(bob)).toString(),
                toDecimals("99", 20));

            await web3tx(superToken6D.downgrade, "downgrade 1 from bob")(
                toWad(1), {
                    from: bob
                }
            );
            assert.equal(
                (await token20D.balanceOf.call(bob)).toString(),
                toDecimals("100", 20));
            assert.equal(
                (await superToken6D.balanceOf.call(bob)).toString(),
                toWad("0").toString());
        });
    });

});
