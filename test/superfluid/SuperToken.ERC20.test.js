const {
    expectRevert,
    expectEvent
} = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad,
    toDecimals,
    toBN
} = require("@decentral.ee/web3-helpers");

const TestToken = artifacts.require("TestToken");
const ISuperToken = artifacts.require("ISuperToken");

const Tester = require("./Tester");

contract("SuperToken's ERC20 implementation", accounts => {

    const tester = new Tester(accounts.slice(0, 4));
    const { alice, bob } = tester.aliases;
    const { INIT_BALANCE, MAX_UINT256 } = tester.constants;
    const { ZERO_ADDRESS } = tester.constants;

    let token;
    let superToken;
    let cfa;
    let superfluid;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            token,
            superfluid,
            superToken,
            cfa,
        } = tester.contracts);
    });

    describe("#1 SuperToken.upgrade/downgrade", () => {
        it("#1.1 - should upgrade if enough balance", async () => {
            const initialBalance = await token.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2.0 tokens from alice") (
                toWad(2), {
                    from: alice
                });
            const { timestamp } = await web3.eth.getBlock("latest");

            const finalBalance = await token.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(alice);
            const finalRealBalance = await superToken.realtimeBalanceOf.call(alice, timestamp);

            assert.equal(finalSuperTokenBalance.toString(), toWad(2).toString(),
                "SuperToken.balanceOf is wrong");
            assert.equal(initialBalance.sub(finalBalance).toString(), toWad(2).toString(),
                "SuperToken.upgrade should manage underlying tokens");
            assert.equal(finalRealBalance.availableBalance.toString(), finalSuperTokenBalance.toString(),
                "balanceOf should equal realtimeBalanceOf");

            await tester.validateSystem();
        });

        it("#1.2 - should not upgrade without enough underlying balance", async() => {
            const initialBalance = await token.balanceOf.call(alice);
            await expectRevert(web3tx(superToken.upgrade, "SuperToken.upgrade - bad balance")(
                initialBalance.add(toBN(1)), {from: alice}), "ERC20: transfer amount exceeds balance");
            await tester.validateSystem();
        });

        it("#1.3 - should downgrade by single account", async() => {
            const initialBalance = await token.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });

            await web3tx(superToken.downgrade, "SuperToken.downgrade 2 from alice") (
                toWad(1), {
                    from: alice
                });

            const finalBalance = await token.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(alice);

            assert.isOk(initialBalance.sub(finalBalance).toString(), toWad(1),
                "TestToken.balanceOf should recover after downgrade");
            assert.equal(finalSuperTokenBalance.toString(), toWad("1"),
                "SuperToken.balanceOf is wrong");

            await tester.validateSystem();
        });

        it("#1.4 - should downgrade by multiple accounts", async () => {
            const initialBalanceAlice = await token.balanceOf.call(alice);
            const initialSuperBalanceAlice = await superToken.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "upgrade 2 from alice")(toWad(2), {from: alice});
            await web3tx(superToken.upgrade, "upgrade 1 from bob")(toWad(1), {from: bob});

            const initialSuperBalanceBob = await superToken.balanceOf.call(bob);

            await web3tx(superToken.downgrade, "downgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });

            const finalBalanceAlice = await token.balanceOf.call(alice);
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

            await tester.validateSystem();
        });

        it("#1.5 - should not downgrade if there is no balance", async () => {
            await expectRevert(web3tx(superToken.downgrade, "SuperToken.downgrade - bad balance")(
                toBN(1), {
                    from: alice
                }), "SuperToken: downgrade amount exceeds balance");
        });

        it("#1.6 - should convert from smaller underlying decimals", async () => {
            const token6D = await web3tx(TestToken.new, "TestToken.new")(
                "Test Token 6 Decimals", "TEST6D",
                {
                    from: bob
                });
            await web3tx(token6D.mint, "Mint token for bob")(
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

        it("#1.7 - should convert from larger underlying decimals", async () => {
            const token20D = await web3tx(TestToken.new, "TestToken.new")(
                "Test Token 20 Decimals", "TEST20D",
                {
                    from: bob
                });
            await web3tx(token20D.mint, "Mint token for bob")(
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

    describe("#4 SuperToken.transfer", () => {
        it("#4.1 - should transfer available amount", async() => {
            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await web3tx(superToken.transfer, "SuperToken.transfer 2 from alice to bob") (
                bob, toWad(0.5), {
                    from: alice
                });

            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(finalSuperBalanceAlice.toString(), toWad(1.5));
            assert.equal(finalSuperBalanceBob.toString(), toWad(0.5));

            await tester.validateSystem();
        });

        it("#4.2 - should not transfer unavailable balance", async() => {
            await web3tx(superToken.upgrade, "upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await expectRevert(
                web3tx(superToken.transfer, "transfer 2(+1wei) from alice to bob should fail")(
                    bob, toWad(2).add(toBN(1)), {from: alice}
                ), "SuperfluidToken: move amount exceeds balance");
            await tester.validateSystem();
        });

        it("#4.4 - should not transfer to zero address", async () => {
            await expectRevert(
                web3tx(superToken.transfer, "transfer to zero address")(
                    ZERO_ADDRESS, 1, {from: alice}),
                "SuperToken: transfer to zero address");
        });
    });

    describe("#5 SuperToken.approve", () => {
        it("#5.1 - should approve amount", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});
            const aliceSuperBalance = await superToken.balanceOf.call(alice);
            await web3tx(superToken.approve, "approve bob all alice balance")(
                bob, aliceSuperBalance, {from: alice});

            const fullAllowedBalanceBob = await superToken.allowance.call(alice, bob);
            assert.equal(aliceSuperBalance.toString(),
                fullAllowedBalanceBob.toString(),
                "Bob allowance is not alice full balance"
            );

            await web3tx(superToken.approve, "approve bob half of alice balance")(
                bob, aliceSuperBalance.div(toBN(2)), {from: alice});
            const halfAllowedBalanceBob = await superToken.allowance.call(alice, bob);
            assert.equal(aliceSuperBalance.div(toBN(2)).toString(),
                halfAllowedBalanceBob.toString(),
                "Bob allowance is not alice half balance"
            );

            await web3tx(superToken.approve, "unapprove bob")(
                bob, 0, {from: alice});
            const finalAllowedBalanceBob = await superToken.allowance.call(alice, bob);
            assert.equal(finalAllowedBalanceBob.toString(), 0, "bob final allowance should be zero");


        });

        it("#5.2 - should transfer approved amount reducing allowance amount", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});
            const aliceSuperBalance = await superToken.balanceOf.call(alice);
            await web3tx(superToken.approve, "approve bob all alice balance")(
                bob, aliceSuperBalance, {from: alice}
            );

            await superToken.transferFrom(alice, bob, aliceSuperBalance, {from: bob});
            const superBalanceBob = await superToken.balanceOf.call(bob);
            assert.equal(superBalanceBob.toString(),
                aliceSuperBalance.toString(),
                "bob didn't received all amount of alice"
            );

            await expectRevert(
                web3tx(superToken.transferFrom,
                    "SuperToken.transferFrom without allowance")(
                    alice,
                    bob,
                    1, {from: bob}
                ), "SuperfluidToken: move amount exceeds balance");
        });

        it("#5.3 - should not approve zero address", async () => {
            await expectRevert(
                web3tx(superToken.approve, "approve to zero address")(
                    ZERO_ADDRESS, 1, {from: alice}),
                "SuperToken: approve to zero address");
        });
    });

    describe("#7 SuperToken.send", () => {
        const ERC777SenderRecipientMock = artifacts.require("ERC777SenderRecipientMock");
        const data = web3.utils.sha3("OZ777TestData");

        it("#7.1 - should send available amount to EOA", async() => {
            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await web3tx(superToken.send, "SuperToken.transfer 2 from alice to bob") (
                bob, toWad(0.5), data, {
                    from: alice
                });

            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(finalSuperBalanceAlice.toString(), toWad(1.5));
            assert.equal(finalSuperBalanceBob.toString(), toWad(0.5));

            await tester.validateSystem();
        });

        it("#7.2 - should fail to send to non-ERC777TokensRecipient contract", async() => {
            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await expectRevert(superToken.send(
                cfa.address, toWad(0.5), data, {
                    from: alice
                }),
            "SuperToken: not an ERC777TokensRecipient");
        });

        it("#7.3 - can still transfer to non-ERC777TokensRecipient contract", async() => {
            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await web3tx(superToken.transfer, "SuperToken.transfer 2 from alice to random contract") (
                bob, toWad(0.5), {
                    from: alice
                });

            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(finalSuperBalanceAlice.toString(), toWad(1.5));
            assert.equal(finalSuperBalanceBob.toString(), toWad(0.5));

            await tester.validateSystem();
        });

        it("#7.3 - should send to ERC777TokensRecipient contract", async() => {
            let tx;

            const mock = await ERC777SenderRecipientMock.new();
            await mock.senderFor(mock.address);
            await mock.recipientFor(mock.address);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            tx = await web3tx(superToken.send, "SuperToken.transfer 2 from alice to mock contract") (
                mock.address, toWad(0.5), data, {
                    from: alice
                });
            await expectEvent.inTransaction(
                tx.tx,
                ERC777SenderRecipientMock,
                "TokensReceivedCalled", {
                    token: superToken.address,
                    operator: alice,
                    from: alice,
                    to: mock.address,
                    data,
                    operatorData: null
                });

            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceMock = await superToken.balanceOf.call(mock.address);

            assert.equal(finalSuperBalanceAlice.toString(), toWad(1.5));
            assert.equal(finalSuperBalanceMock.toString(), toWad(0.5));

            tx = await mock.send(
                superToken.address,
                alice,
                toWad(0.5),
                data
            );
            await expectEvent.inTransaction(
                tx.tx,
                ERC777SenderRecipientMock,
                "TokensToSendCalled", {
                    token: superToken.address,
                    operator: mock.address,
                    from: mock.address,
                    to: alice,
                    data,
                    operatorData: null
                });

            await tester.validateSystem();
        });

    });
});
