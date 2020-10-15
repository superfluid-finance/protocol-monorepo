const {
    expectRevert,
    expectEvent
} = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad,
} = require("@decentral.ee/web3-helpers");

const Tester = require("./Tester");

contract("SuperToken's ERC777 implementation", accounts => {

    const tester = new Tester(accounts.slice(0, 4));
    const { alice, bob } = tester.aliases;

    let superToken;
    let cfa;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            superToken,
            cfa,
        } = tester.contracts);
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
