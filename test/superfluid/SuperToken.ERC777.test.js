const {
    expectRevert,
    expectEvent,
    singletons
} = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-helpers");

const { expect } = require("chai");

// const {
// //   shouldBehaveLikeERC777DirectSendBurn,
// //   shouldBehaveLikeERC777OperatorSendBurn,
// //   shouldBehaveLikeERC777UnauthorizedOperatorSendBurn,
// //   shouldBehaveLikeERC777InternalMint,
//   // shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook,
// //   shouldBehaveLikeERC777SendBurnWithSendHook,
// } = require('./ERC777.behavior');

const Tester = require("./Tester");

contract("SuperToken's ERC777 implementation", accounts => {

    const tester = new Tester(accounts.slice(0, 4));
    const { alice, bob, carol} = tester.aliases;
    const anyone = bob;

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


    describe("#8 SuperToken with no default operators : ERC777 compliance", () => {
        const ERC777SenderRecipientMock = artifacts.require("ERC777SenderRecipientMock");
        const data = web3.utils.sha3("OZ777TestData");
        const operatorData = web3.utils.sha3("OZ777TestOperatorData");
        const defaultOperators = [];
        // const ERC777hash = web3.utils.soliditySha3("ERC777Token");

        beforeEach(async function () {
            this.erc1820 = await singletons.ERC1820Registry(tester.aliases.admin);
        });

        describe("#8.1 basic information", function () {
            it("#8.1.1 returns the name", async function () {
                expect(await superToken.name()).to.equal("Super Test Token");
            });

            it("#8.1.2 returns the symbol", async function () {
                expect(await superToken.symbol()).to.equal("TESTx");
            });

            it("#8.1.3 returns a granularity of 1", async function () {
                expect(await superToken.granularity(), 1);
            });

            it("#8.1.4 returns the default operators", async function () {
                expect(await superToken.defaultOperators()).to.deep.equal(defaultOperators);
            });

            it("#8.1.5 default operators are operators for all accounts", async function () {
                for (const operator of defaultOperators) {
                    expect(await superToken.isOperatorFor(operator, carol)).to.equal(true);
                }
            });

            it("#8.1.6 returns the total supply", async function () {
                expect(await superToken.totalSupply(), toWad(100));
            });

            it("#8.1.7 returns 18 when decimals is called", async function () {
                expect(await superToken.decimals(), 18);
            });

            // it('#8.1.8 the ERC777Token interface is registered in the registry', async function () {
            //   expect(await this.erc1820.getInterfaceImplementer(superToken.address, ERC777hash))
            //     .to.equal(superToken.address);
            // });
            //
            // it('#8.1.9 the ERC20Token interface is registered in the registry', async function () {
            //   expect(await this.erc1820.getInterfaceImplementer(superToken.address, ERC777hash))
            //     .to.equal(superToken.address);
            // });

        });

        describe("#8.2 balanceOf", function () {
            describe("#8.2.1 for an account with no tokens", function () {
                it("#8.2.1.1 returns zero", async function () {
                    expect(await superToken.balanceOf(carol), 0);
                });
            });

            describe("#8.2.2 for an account with tokens", function () {
                it("#8.2.2.1 returns their balance", async function () {
                    await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                        toWad(2), {
                            from: alice
                        });
                    expect(await superToken.balanceOf(alice), toWad(2));
                });
            });
        });

        // ********** //
        // context('#8.3 with no ERC777TokensSender and no ERC777TokensRecipient implementers', function () {
        //   describe('#8.3.1 send/burn', function () {
        //     shouldBehaveLikeERC777DirectSendBurn(alice, anyone, data);
        //
        //     context('#8.3.1.1 with self operator', function () {
        //       shouldBehaveLikeERC777OperatorSendBurn(alice, anyone, alice, data, operatorData);
        //     });
        //
        //     context('#8.3.1.2 with first default operator', function () {
        //       shouldBehaveLikeERC777OperatorSendBurn(alice, anyone, defaultOperatorA, data, operatorData);
        //     });
        //
        //     context('#8.3.1.3 with second default operator', function () {
        //       shouldBehaveLikeERC777OperatorSendBurn(alice, anyone, defaultOperatorB, data, operatorData);
        //     });
        //
        //     context('#8.3.1.4 before authorizing a new operator', function () {
        //       shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(alice, anyone, bob, data, operatorData);
        //     });
        //
        //     context('#8.3.1.5 with new authorized operator', function () {
        //       beforeEach(async function () {
        //         await superToken.authorizeOperator(bob, { from: alice });
        //       });
        //
        //       shouldBehaveLikeERC777OperatorSendBurn(alice, anyone, bob, data, operatorData);
        //
        //       context('#8.3.1.5.1 with revoked operator', function () {
        //         beforeEach(async function () {
        //           await superToken.revokeOperator(bob, { from: alice });
        //         });
        //
        //         shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(alice, anyone, bob, data, operatorData);
        //       });
        //     });
        //   });
        // });
        // ********** //

        describe("#8.4 operator management", function () {
            it("#8.4.1 accounts are their own operator", async function () {
                expect(await superToken.isOperatorFor(alice, alice)).to.equal(true);
            });

            it("#8.4.2 reverts when self-authorizing", async function () {
                await expectRevert(
                    superToken.authorizeOperator(alice, { from: alice }), "ERC777Operators: authorizing self.",
                );
            });

            // it('#8.4.3 reverts when self-revoking', async function () {
            //   await expectRevert(
            //     superToken.revokeOperator(alice, { from: alice }), 'ERC777: revoking self as operator',
            //   );
            // });

            it("#8.4.4 non-operators can be revoked", async function () {
                expect(await superToken.isOperatorFor(bob, alice)).to.equal(false);

                const { logs } = await superToken.revokeOperator(bob, { from: alice });
                expectEvent.inLogs(logs, "RevokedOperator", { operator: bob, tokenHolder: alice });

                expect(await superToken.isOperatorFor(bob, alice)).to.equal(false);
            });

            it("#8.4.5 non-operators can be authorized", async function () {
                expect(await superToken.isOperatorFor(bob, alice)).to.equal(false);

                const { logs } = await superToken.authorizeOperator(bob, { from: alice });
                expectEvent.inLogs(logs, "AuthorizedOperator", { operator: bob, tokenHolder: alice });

                expect(await superToken.isOperatorFor(bob, alice)).to.equal(true);
            });

            describe("#8.4.6 new operators", function () {
                beforeEach(async function () {
                    await superToken.authorizeOperator(bob, { from: alice });
                });

                it("#8.4.6.1 are not added to the default operators list", async function () {
                    expect(await superToken.defaultOperators()).to.deep.equal(defaultOperators);
                });

                it("#8.4.6.2 can be re-authorized", async function () {
                    const { logs } = await superToken.authorizeOperator(bob, { from: alice });
                    expectEvent.inLogs(logs, "AuthorizedOperator", { operator: bob, tokenHolder: alice });

                    expect(await superToken.isOperatorFor(bob, alice)).to.equal(true);
                });

                it("#8.4.6.3 can be revoked", async function () {
                    const { logs } = await superToken.revokeOperator(bob, { from: alice });
                    expectEvent.inLogs(logs, "RevokedOperator", { operator: bob, tokenHolder: alice });

                    expect(await superToken.isOperatorFor(bob, alice)).to.equal(false);
                });
            });

            describe("#8.4.7 default operators", function () {
                it("#8.4.7.1 can be re-authorized", async function () {
                    const { logs } = await superToken.authorizeOperator(accounts[4], { from: alice });
                    expectEvent.inLogs(logs, "AuthorizedOperator", { operator: accounts[4], tokenHolder: alice });

                    expect(await superToken.isOperatorFor(accounts[4], alice)).to.equal(true);
                });

                it("#8.4.7.2 can be revoked", async function () {
                    const { logs } = await superToken.revokeOperator(accounts[4], { from: alice });
                    expectEvent.inLogs(logs, "RevokedOperator", { operator: accounts[4], tokenHolder: alice });

                    expect(await superToken.isOperatorFor(accounts[4], alice)).to.equal(false);
                });

                // it('#8.4.7.3 cannot be revoked for themselves', async function () {
                //   await expectRevert(
                //     superToken.revokeOperator(accounts[4], { from: accounts[4] }),
                //     'ERC777: revoking self as operator',
                //   );
                // });

                describe("#8.4.7.4 with revoked default operator", function () {
                    beforeEach(async function () {
                        await superToken.revokeOperator(accounts[4], { from: alice });
                    });

                    // it('#8.4.7.4.1 default operator is not revoked for other holders', async function () {
                    //   expect(await superToken.isOperatorFor(accounts[4], carol)).to.equal(true);
                    // });

                    // it('#8.4.7.4.2 other default operators are not revoked', async function () {
                    //   expect(await superToken.isOperatorFor(accounts[5], alice)).to.equal(true);
                    // });

                    it("#8.4.7.4.3 default operators list is not modified", async function () {
                        expect(await superToken.defaultOperators()).to.deep.equal(defaultOperators);
                    });

                    it("#8.4.7.4.4 revoked default operator can be re-authorized", async function () {
                        const { logs } = await superToken.authorizeOperator(accounts[4], { from: alice });
                        expectEvent.inLogs(logs, "AuthorizedOperator", { operator: accounts[4], tokenHolder: alice });

                        expect(await superToken.isOperatorFor(accounts[4], alice)).to.equal(true);
                    });
                });

            });

            describe("#8.4.8 send and receive hooks", function () {
                const amount = toWad(1);
                const operator = accounts[4];


                describe("#8.4.8.1 tokensReceived", function () {
                    beforeEach(function () {
                        this.sender = alice;
                    });

                    describe("#8.4.8.1.1 with no ERC777TokensRecipient implementer", function () {
                        describe("#8.4.8.1.1.1 with contract recipient", function () {
                            beforeEach(async function () {
                                this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                                this.recipient = this.tokensRecipientImplementer.address;


                            });

                            it("#8.4.8.1.1.1.1 send reverts", async function () {
                                await expectRevert(
                                    superToken.send(this.recipient, amount, data, { from: alice }),
                                    "SuperfluidToken: move amount exceeds balance.",
                                );
                            });

                            it("#8.4.8.1.1.1.2 operatorSend reverts", async function () {
                                await expectRevert(
                                    superToken.operatorSend(this.sender,this.recipient,amount,data,operatorData,
                                        {from:operator}),
                                    "SuperToken: caller is not an operator for holder.",
                                );
                            });

                            // it('#8.4.8.1.1.1.3 mint (internal) reverts', async function () {
                            //   await expectRevert(
                            //     superToken.mintInternal(this.recipient, amount, data, operatorData, {from:operator}),
                            //     'ERC777: token recipient contract has no implementer for ERC777TokensRecipient',
                            //   );
                            // });

                            it("#8.4.8.1.1.1.4 (ERC20) transfer succeeds", async function () {
                                await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                                    toWad(2), {
                                        from: alice
                                    });
                                await superToken.transfer(this.recipient, amount, { from: alice });
                            });

                            it("#8.4.8.1.1.1.5 (ERC20) transferFrom succeeds", async function () {
                                await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                                    toWad(2), {
                                        from: alice
                                    });
                                const approved = anyone;
                                await superToken.approve(approved, amount, { from: this.sender });
                                await superToken.transferFrom(this.sender, this.recipient, amount, { from: approved });
                            });
                        });
                    });

                    describe("#8.4.8.1.2 with ERC777TokensRecipient implementer", function () {
                        describe("#8.4.8.1.2.1 with contract as implementer for an externally owned account",function(){
                            beforeEach(async function () {
                                this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                                this.recipient = anyone;

                                await this.tokensRecipientImplementer.recipientFor(this.recipient);

                                await this.erc1820.setInterfaceImplementer(
                                    this.recipient,
                                    web3.utils.soliditySha3("ERC777TokensRecipient"),
                                    this.tokensRecipientImplementer.address,
                                    { from: this.recipient },
                                );
                            });


                        });

                        describe("#8.4.8.1.2.2 with contract as implementer for another contract", function () {
                            beforeEach(async function () {
                                this.recipientContract = await ERC777SenderRecipientMock.new();
                                this.recipient = this.recipientContract.address;

                                this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                                await this.tokensRecipientImplementer.recipientFor(this.recipient);
                                await this.recipientContract.registerRecipient(this.tokensRecipientImplementer.address);
                            });


                        });

                        describe("#8.4.8.1.2.3 with contract as implementer for itself", function () {
                            beforeEach(async function () {
                                this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                                this.recipient = this.tokensRecipientImplementer.address;

                                await this.tokensRecipientImplementer.recipientFor(this.recipient);
                            });


                        });
                    });
                });

                describe("#8.4.8.2 tokensToSend", function () {
                    beforeEach(function () {
                        this.recipient = carol;
                    });

                    describe("#8.4.8.2.1 with a contract as implementer for an externally owned account", function () {
                        beforeEach(async function () {
                            this.tokensSenderImplementer = await ERC777SenderRecipientMock.new();
                            this.sender = alice;

                            await this.tokensSenderImplementer.senderFor(this.sender);

                            await this.erc1820.setInterfaceImplementer(
                                this.sender,
                                web3.utils.soliditySha3("ERC777TokensSender"), this.tokensSenderImplementer.address,
                                { from: this.sender },
                            );
                        });

                        // shouldBehaveLikeERC777SendBurnWithSendHook(operator, amount, data, operatorData);
                    });

                    describe("#8.4.8.2.2 with contract as implementer for another contract", function () {
                        beforeEach(async function () {
                            this.senderContract = await ERC777SenderRecipientMock.new();
                            this.sender = this.senderContract.address;

                            this.tokensSenderImplementer = await ERC777SenderRecipientMock.new();
                            await this.tokensSenderImplementer.senderFor(this.sender);
                            await this.senderContract.registerSender(this.tokensSenderImplementer.address);
                            await this.senderContract.recipientFor(this.sender);
                            await superToken.send(this.sender, amount, data, { from: alice });
                        });

                        // shouldBehaveLikeERC777SendBurnWithSendHook(operator, amount, data, operatorData);
                    });

                    describe("#8.4.8.2.3 with a contract as implementer for itself", function () {
                        beforeEach(async function () {
                            this.tokensSenderImplementer = await ERC777SenderRecipientMock.new();
                            this.sender = this.tokensSenderImplementer.address;

                            await this.tokensSenderImplementer.senderFor(this.sender);
                            await this.tokensSenderImplementer.recipientFor(this.sender);
                            await superToken.send(this.sender, amount, data, { from: alice });
                        });

                        // shouldBehaveLikeERC777SendBurnWithSendHook(operator, amount, data, operatorData);
                    });

                });

            });
        });

    });
});
