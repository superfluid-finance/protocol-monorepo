const TestEnvironment = require("../../TestEnvironment");

const {expectRevert, expectEvent} = require("@openzeppelin/test-helpers");
const {expect} = require("chai");

const {web3tx, toWad} = require("@decentral.ee/web3-helpers");

const {
    shouldBehaveLikeERC777DirectSendBurn,
    shouldBehaveLikeERC777OperatorSendBurn,
    shouldBehaveLikeERC777UnauthorizedOperatorSendBurn,
    shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook,
    shouldBehaveLikeERC777SendBurnWithSendHook,
} = require("./ERC777.behavior");

const ERC777SenderRecipientMock = artifacts.require(
    "ERC777SenderRecipientMock"
);
const SuperTokenMock = artifacts.require("SuperTokenMock");

describe("SuperToken's ERC777 implementation", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;
    const initialSupply = toWad(50);
    const testData = web3.utils.sha3("OZ777TestData");
    const operatorData = web3.utils.sha3("OZ777TestOperatorData");

    let holder, defaultOperatorA, defaultOperatorB, newOperator, anyone;
    let erc1820;

    before(async function () {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 6,
        });

        ({
            alice: holder,
            bob: defaultOperatorA,
            carol: defaultOperatorB,
            dan: newOperator,
            eve: anyone,
        } = t.aliases);
        this.token = await SuperTokenMock.at(t.sf.tokens.TESTx.address);
        ({erc1820} = t.contracts);

        await web3tx(
            this.token.upgrade,
            "Upgrade initialSupply amount of token for holder"
        )(initialSupply, {
            from: holder,
        });

        await t.pushEvmSnapshot();
    });

    after(async function () {
        await t.popEvmSnapshot();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    context("with default operators", async () => {
        let defaultOperators;

        before(() => {
            defaultOperators = [defaultOperatorA, defaultOperatorB];
        });

        beforeEach(async function () {
            await this.token.setupDefaultOperators(defaultOperators);
        });

        describe("basic information", function () {
            it("returns the name", async function () {
                expect(await this.token.name()).to.equal(
                    "Super TEST Fake Token"
                );
            });

            it("returns the symbol", async function () {
                expect(await this.token.symbol()).to.equal("TESTx");
            });

            it("returns decimals (non-ERC777 standard)", async function () {
                assert.equal(await this.token.decimals.call(), 18);
            });

            it("returns a granularity of 1", async function () {
                expect(await this.token.granularity(), 1);
            });

            it("returns the default operators", async function () {
                expect(await this.token.defaultOperators()).to.deep.equal(
                    defaultOperators
                );
            });

            it("default operators are operators for all accounts", async function () {
                for (const operator of defaultOperators) {
                    expect(
                        await this.token.isOperatorFor(operator, anyone)
                    ).to.equal(true);
                }
            });

            it("returns the total supply", async function () {
                expect(await this.token.totalSupply(), toWad(100));
            });

            it("returns 18 when decimals is called", async function () {
                expect(await this.token.decimals(), 18);
            });

            it("the ERC777Token interface is registered in the registry", async function () {
                expect(
                    await erc1820.getInterfaceImplementer(
                        this.token.address,
                        web3.utils.soliditySha3("ERC777Token")
                    )
                ).to.equal(this.token.address);
            });

            it("the ERC20Token interface is registered in the registry", async function () {
                expect(
                    await erc1820.getInterfaceImplementer(
                        this.token.address,
                        web3.utils.soliditySha3("ERC20Token")
                    )
                ).to.equal(this.token.address);
            });
        });

        // it("does not emit AuthorizedOperator events for default operators", async function () {
        //     await expectEvent.notEmitted.inConstruction(this.token, "AuthorizedOperator");
        // });

        context(
            "with no ERC777TokensSender and no ERC777TokensRecipient implementers",
            function () {
                describe("send/burn", function () {
                    context("direct operations", function () {
                        shouldBehaveLikeERC777DirectSendBurn(
                            () => ({
                                holder,
                                recipient: anyone,
                            }),
                            testData
                        );
                    });

                    context("with self operator", function () {
                        shouldBehaveLikeERC777OperatorSendBurn(
                            () => ({
                                holder,
                                recipient: anyone,
                                operator: holder,
                            }),
                            testData,
                            operatorData
                        );
                    });

                    context("with first default operator", function () {
                        shouldBehaveLikeERC777OperatorSendBurn(
                            () => ({
                                holder,
                                recipient: anyone,
                                operator: defaultOperatorA,
                            }),
                            testData,
                            operatorData
                        );
                    });

                    context("with second default operator", function () {
                        shouldBehaveLikeERC777OperatorSendBurn(
                            () => ({
                                holder,
                                recipient: anyone,
                                operator: defaultOperatorB,
                            }),
                            testData,
                            operatorData
                        );
                    });

                    context("before authorizing a new operator", function () {
                        shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(
                            () => ({
                                holder,
                                recipient: anyone,
                                operator: newOperator,
                            }),
                            testData,
                            operatorData
                        );
                    });

                    context("with new authorized operator", function () {
                        beforeEach(async function () {
                            await this.token.authorizeOperator(newOperator, {
                                from: holder,
                            });
                        });

                        shouldBehaveLikeERC777OperatorSendBurn(
                            () => ({
                                holder,
                                recipient: anyone,
                                operator: newOperator,
                            }),
                            testData,
                            operatorData
                        );

                        context("with revoked operator", function () {
                            beforeEach(async function () {
                                await this.token.revokeOperator(newOperator, {
                                    from: holder,
                                });
                            });

                            shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(
                                () => ({
                                    holder,
                                    recipient: anyone,
                                    operator: newOperator,
                                }),
                                testData,
                                operatorData
                            );
                        });
                    });
                });
            }
        );

        describe("operator management", function () {
            it("accounts are their own operator", async function () {
                expect(await this.token.isOperatorFor(holder, holder)).to.equal(
                    true
                );
            });

            it("reverts when self-authorizing", async function () {
                await expectRevert(
                    this.token.authorizeOperator(holder, {from: holder}),
                    "ERC777Operators: authorizing self as operator"
                );
            });

            it("reverts when self-revoking", async function () {
                await expectRevert(
                    this.token.revokeOperator(holder, {from: holder}),
                    "ERC777Operators: revoking self as operator"
                );
            });

            it("non-operators can be revoked", async function () {
                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(false);

                const {logs} = await this.token.revokeOperator(newOperator, {
                    from: holder,
                });
                expectEvent.inLogs(logs, "RevokedOperator", {
                    operator: newOperator,
                    tokenHolder: holder,
                });

                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(false);
            });

            it("non-operators can be authorized", async function () {
                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(false);

                const {logs} = await this.token.authorizeOperator(newOperator, {
                    from: holder,
                });
                expectEvent.inLogs(logs, "AuthorizedOperator", {
                    operator: newOperator,
                    tokenHolder: holder,
                });

                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(true);
            });

            describe("new operators", function () {
                beforeEach(async function () {
                    await this.token.authorizeOperator(newOperator, {
                        from: holder,
                    });
                });

                it("are not added to the default operators list", async function () {
                    expect(await this.token.defaultOperators()).to.deep.equal(
                        defaultOperators
                    );
                });

                it("can be re-authorized", async function () {
                    const {logs} = await this.token.authorizeOperator(
                        newOperator,
                        {
                            from: holder,
                        }
                    );
                    expectEvent.inLogs(logs, "AuthorizedOperator", {
                        operator: newOperator,
                        tokenHolder: holder,
                    });

                    expect(
                        await this.token.isOperatorFor(newOperator, holder)
                    ).to.equal(true);
                });

                it("can be revoked", async function () {
                    const {logs} = await this.token.revokeOperator(
                        newOperator,
                        {
                            from: holder,
                        }
                    );
                    expectEvent.inLogs(logs, "RevokedOperator", {
                        operator: newOperator,
                        tokenHolder: holder,
                    });

                    expect(
                        await this.token.isOperatorFor(newOperator, holder)
                    ).to.equal(false);
                });
            });

            describe("default operators", function () {
                it("can be re-authorized", async function () {
                    const {logs} = await this.token.authorizeOperator(
                        defaultOperatorA,
                        {
                            from: holder,
                        }
                    );
                    expectEvent.inLogs(logs, "AuthorizedOperator", {
                        operator: defaultOperatorA,
                        tokenHolder: holder,
                    });

                    expect(
                        await this.token.isOperatorFor(defaultOperatorA, holder)
                    ).to.equal(true);
                });

                it("can be revoked", async function () {
                    const {logs} = await this.token.revokeOperator(
                        defaultOperatorA,
                        {
                            from: holder,
                        }
                    );
                    expectEvent.inLogs(logs, "RevokedOperator", {
                        operator: defaultOperatorA,
                        tokenHolder: holder,
                    });

                    expect(
                        await this.token.isOperatorFor(defaultOperatorA, holder)
                    ).to.equal(false);
                });

                it("cannot be revoked for themselves", async function () {
                    await expectRevert(
                        this.token.revokeOperator(defaultOperatorA, {
                            from: defaultOperatorA,
                        }),
                        "ERC777Operators: revoking self as operator"
                    );
                });

                context("with revoked default operator", function () {
                    beforeEach(async function () {
                        await this.token.revokeOperator(defaultOperatorA, {
                            from: holder,
                        });
                    });

                    it("default operator is not revoked for other holders", async function () {
                        expect(
                            await this.token.isOperatorFor(
                                defaultOperatorA,
                                anyone
                            )
                        ).to.equal(true);
                    });

                    it("other default operators are not revoked", async function () {
                        expect(
                            await this.token.isOperatorFor(
                                defaultOperatorB,
                                holder
                            )
                        ).to.equal(true);
                    });

                    it("default operators list is not modified", async function () {
                        expect(
                            await this.token.defaultOperators()
                        ).to.deep.equal(defaultOperators);
                    });

                    it("revoked default operator can be re-authorized", async function () {
                        const {logs} = await this.token.authorizeOperator(
                            defaultOperatorA,
                            {from: holder}
                        );
                        expectEvent.inLogs(logs, "AuthorizedOperator", {
                            operator: defaultOperatorA,
                            tokenHolder: holder,
                        });

                        expect(
                            await this.token.isOperatorFor(
                                defaultOperatorA,
                                holder
                            )
                        ).to.equal(true);
                    });
                });
            });
        });

        describe("send and receive hooks", function () {
            const amount = toWad(1);
            let sender, operator;

            before(function () {
                sender = holder;
                operator = defaultOperatorA;
            });

            describe("tokensReceived", function () {
                describe("with no ERC777TokensRecipient implementer", function () {
                    describe("with contract recipient", function () {
                        let recipient;

                        beforeEach(async function () {
                            this.tokensRecipientImplementer =
                                await ERC777SenderRecipientMock.new();
                            recipient = this.tokensRecipientImplementer.address;
                        });

                        it("send reverts", async function () {
                            await expectRevert(
                                this.token.send(recipient, amount, testData, {
                                    from: holder,
                                }),
                                "SuperToken: not an ERC777TokensRecipient"
                            );
                        });

                        it("operatorSend reverts", async function () {
                            await expectRevert(
                                this.token.operatorSend(
                                    sender,
                                    recipient,
                                    amount,
                                    testData,
                                    operatorData,
                                    {from: operator}
                                ),
                                "SuperToken: not an ERC777TokensRecipient"
                            );
                        });

                        it("mint (internal) reverts", async function () {
                            await expectRevert(
                                this.token.mintInternal(
                                    recipient,
                                    amount,
                                    testData,
                                    operatorData,
                                    {from: operator}
                                ),
                                "SuperToken: not an ERC777TokensRecipient"
                            );
                        });

                        it("mint (internal) to zero address reverts", async function () {
                            await expectRevert(
                                this.token.mintInternal(
                                    ZERO_ADDRESS,
                                    amount,
                                    testData,
                                    operatorData,
                                    {from: operator}
                                ),
                                "SuperToken: mint to zero address"
                            );
                        });

                        it("(ERC20) transfer succeeds", async function () {
                            await web3tx(
                                this.token.upgrade,
                                "SuperToken.upgrade 2 from holder"
                            )(toWad(2), {
                                from: holder,
                            });
                            await this.token.transfer(recipient, amount, {
                                from: holder,
                            });
                        });

                        it("(ERC20) transferFrom succeeds", async function () {
                            await web3tx(
                                this.token.upgrade,
                                "SuperToken.upgrade 2 from holder"
                            )(toWad(2), {
                                from: holder,
                            });
                            const approved = anyone;
                            await this.token.approve(approved, amount, {
                                from: sender,
                            });
                            await this.token.transferFrom(
                                sender,
                                recipient,
                                amount,
                                {from: approved}
                            );
                        });
                    });
                });

                describe("with ERC777TokensRecipient implementer", function () {
                    describe("with contract as implementer for an externally owned account", function () {
                        let recipient;

                        beforeEach(async function () {
                            recipient = anyone;

                            this.tokensRecipientImplementer =
                                await ERC777SenderRecipientMock.new();

                            await this.tokensRecipientImplementer.recipientFor(
                                recipient
                            );

                            await erc1820.setInterfaceImplementer(
                                recipient,
                                web3.utils.soliditySha3(
                                    "ERC777TokensRecipient"
                                ),
                                this.tokensRecipientImplementer.address,
                                {from: recipient}
                            );
                        });

                        shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
                            () => ({sender, recipient, operator}),
                            amount,
                            testData,
                            operatorData
                        );
                    });

                    describe("with contract as implementer for another contract", function () {
                        let recipient;

                        beforeEach(async function () {
                            this.recipientContract =
                                await ERC777SenderRecipientMock.new();
                            recipient = this.recipientContract.address;

                            this.tokensRecipientImplementer =
                                await ERC777SenderRecipientMock.new();
                            await this.tokensRecipientImplementer.recipientFor(
                                recipient
                            );
                            await this.recipientContract.registerRecipient(
                                this.tokensRecipientImplementer.address
                            );
                        });

                        shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
                            () => ({sender, recipient, operator}),
                            amount,
                            testData,
                            operatorData
                        );
                    });

                    describe("with contract as implementer for itself", function () {
                        let recipient;

                        beforeEach(async function () {
                            this.tokensRecipientImplementer =
                                await ERC777SenderRecipientMock.new();
                            recipient = this.tokensRecipientImplementer.address;

                            await this.tokensRecipientImplementer.recipientFor(
                                recipient
                            );
                        });

                        shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
                            () => ({sender, recipient, operator}),
                            amount,
                            testData,
                            operatorData
                        );
                    });
                });
            });

            describe("tokensToSend", function () {
                let recipient;

                before(() => {
                    recipient = anyone;
                });

                describe("with a contract as implementer for an externally owned account", function () {
                    beforeEach(async function () {
                        this.tokensSenderImplementer =
                            await ERC777SenderRecipientMock.new();

                        await this.tokensSenderImplementer.senderFor(sender);

                        await erc1820.setInterfaceImplementer(
                            sender,
                            web3.utils.soliditySha3("ERC777TokensSender"),
                            this.tokensSenderImplementer.address,
                            {from: sender}
                        );
                    });

                    shouldBehaveLikeERC777SendBurnWithSendHook(
                        () => ({sender, recipient, operator}),
                        amount,
                        testData,
                        operatorData
                    );
                });

                describe("with contract as implementer for another contract", function () {
                    beforeEach(async function () {
                        this.senderContract =
                            await ERC777SenderRecipientMock.new();
                        sender = this.senderContract.address;

                        this.tokensSenderImplementer =
                            await ERC777SenderRecipientMock.new();
                        await this.tokensSenderImplementer.senderFor(sender);
                        await this.senderContract.registerSender(
                            this.tokensSenderImplementer.address
                        );
                        await this.senderContract.recipientFor(sender);
                        await this.token.send(sender, amount, testData, {
                            from: holder,
                        });
                    });

                    shouldBehaveLikeERC777SendBurnWithSendHook(
                        () => ({sender, recipient, operator}),
                        amount,
                        testData,
                        operatorData
                    );
                });

                describe("with a contract as implementer for itself", function () {
                    beforeEach(async function () {
                        this.tokensSenderImplementer =
                            await ERC777SenderRecipientMock.new();
                        sender = this.tokensSenderImplementer.address;

                        await this.tokensSenderImplementer.senderFor(sender);
                        await this.tokensSenderImplementer.recipientFor(sender);
                        await this.token.send(sender, amount, testData, {
                            from: holder,
                        });
                    });

                    shouldBehaveLikeERC777SendBurnWithSendHook(
                        () => ({sender, recipient, operator}),
                        amount,
                        testData,
                        operatorData
                    );
                });
            });
        });
    });

    context("with no default operators", function () {
        it("default operators list is empty", async function () {
            expect(await this.token.defaultOperators()).to.deep.equal([]);
        });
    });
});
