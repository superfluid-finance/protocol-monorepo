const TestEnvironment = require("../../TestEnvironment");

const { expectRevert, expectEvent } = require("@openzeppelin/test-helpers");
const { expect } = require("chai");

const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

const ERC777SenderRecipientMock = artifacts.require(
    "ERC777SenderRecipientMock"
);

const {
    shouldBehaveLikeERC777DirectSendBurn,
    shouldBehaveLikeERC777OperatorSendBurn,
    shouldBehaveLikeERC777UnauthorizedOperatorSendBurn,
    //shouldBehaveLikeERC777InternalMint,
    shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook,
    shouldBehaveLikeERC777SendBurnWithSendHook
} = require("./ERC777.behavior");

contract("SuperToken's ERC777 implementation", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 6), {
        isTruffle: true,
        useMocks: true
    });
    const [
        ,
        holder,
        defaultOperatorA,
        defaultOperatorB,
        newOperator,
        anyone
    ] = accounts;
    const defaultOperators = [defaultOperatorA, defaultOperatorB];
    const initialSupply = toWad(50);
    const { ZERO_ADDRESS } = t.constants;
    const data = web3.utils.sha3("OZ777TestData");
    const operatorData = web3.utils.sha3("OZ777TestOperatorData");

    let erc1820;

    before(async () => {
        await t.reset();
        ({ erc1820 } = t.contracts);
    });

    beforeEach(async function() {
        await t.createNewToken({ doUpgrade: false });
        this.token = t.contracts.superToken;
        await web3tx(
            this.token.upgrade,
            "Upgrade initialSupply amount of token for holder"
        )(initialSupply, {
            from: holder
        });
    });

    context("with default operators", async () => {
        beforeEach(async function() {
            await this.token.setupDefaultOperators(defaultOperators);
        });

        describe("basic information", function() {
            it("returns the name", async function() {
                expect(await this.token.name()).to.equal("Super Test Token");
            });

            it("returns the symbol", async function() {
                expect(await this.token.symbol()).to.equal("TESTx");
            });

            it("returns decimals (non-ERC777 standard)", async function() {
                assert.equal(await this.token.decimals.call(), 18);
            });

            it("returns a granularity of 1", async function() {
                expect(await this.token.granularity(), 1);
            });

            it("returns the default operators", async function() {
                expect(await this.token.defaultOperators()).to.deep.equal(
                    defaultOperators
                );
            });

            it("default operators are operators for all accounts", async function() {
                for (const operator of defaultOperators) {
                    expect(
                        await this.token.isOperatorFor(operator, anyone)
                    ).to.equal(true);
                }
            });

            it("returns the total supply", async function() {
                expect(await this.token.totalSupply(), toWad(100));
            });

            it("returns 18 when decimals is called", async function() {
                expect(await this.token.decimals(), 18);
            });

            it("the ERC777Token interface is registered in the registry", async function() {
                expect(
                    await erc1820.getInterfaceImplementer(
                        this.token.address,
                        web3.utils.soliditySha3("ERC777Token")
                    )
                ).to.equal(this.token.address);
            });

            it("the ERC20Token interface is registered in the registry", async function() {
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
            function() {
                describe("send/burn", function() {
                    shouldBehaveLikeERC777DirectSendBurn(holder, anyone, data);

                    context("with self operator", function() {
                        shouldBehaveLikeERC777OperatorSendBurn(
                            holder,
                            anyone,
                            holder,
                            data,
                            operatorData
                        );
                    });

                    context("with first default operator", function() {
                        shouldBehaveLikeERC777OperatorSendBurn(
                            holder,
                            anyone,
                            defaultOperatorA,
                            data,
                            operatorData
                        );
                    });

                    context("with second default operator", function() {
                        shouldBehaveLikeERC777OperatorSendBurn(
                            holder,
                            anyone,
                            defaultOperatorB,
                            data,
                            operatorData
                        );
                    });

                    context("before authorizing a new operator", function() {
                        shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(
                            holder,
                            anyone,
                            newOperator,
                            data,
                            operatorData
                        );
                    });

                    context("with new authorized operator", function() {
                        beforeEach(async function() {
                            await this.token.authorizeOperator(newOperator, {
                                from: holder
                            });
                        });

                        shouldBehaveLikeERC777OperatorSendBurn(
                            holder,
                            anyone,
                            newOperator,
                            data,
                            operatorData
                        );

                        context("with revoked operator", function() {
                            beforeEach(async function() {
                                await this.token.revokeOperator(newOperator, {
                                    from: holder
                                });
                            });

                            shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(
                                holder,
                                anyone,
                                newOperator,
                                data,
                                operatorData
                            );
                        });
                    });
                });
            }
        );

        describe("operator management", function() {
            it("accounts are their own operator", async function() {
                expect(await this.token.isOperatorFor(holder, holder)).to.equal(
                    true
                );
            });

            it("reverts when self-authorizing", async function() {
                await expectRevert(
                    this.token.authorizeOperator(holder, { from: holder }),
                    "ERC777Operators: authorizing self as operator"
                );
            });

            it("reverts when self-revoking", async function() {
                await expectRevert(
                    this.token.revokeOperator(holder, { from: holder }),
                    "ERC777Operators: revoking self as operator"
                );
            });

            it("non-operators can be revoked", async function() {
                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(false);

                const { logs } = await this.token.revokeOperator(newOperator, {
                    from: holder
                });
                expectEvent.inLogs(logs, "RevokedOperator", {
                    operator: newOperator,
                    tokenHolder: holder
                });

                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(false);
            });

            it("non-operators can be authorized", async function() {
                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(false);

                const { logs } = await this.token.authorizeOperator(
                    newOperator,
                    { from: holder }
                );
                expectEvent.inLogs(logs, "AuthorizedOperator", {
                    operator: newOperator,
                    tokenHolder: holder
                });

                expect(
                    await this.token.isOperatorFor(newOperator, holder)
                ).to.equal(true);
            });

            describe("new operators", function() {
                beforeEach(async function() {
                    await this.token.authorizeOperator(newOperator, {
                        from: holder
                    });
                });

                it("are not added to the default operators list", async function() {
                    expect(await this.token.defaultOperators()).to.deep.equal(
                        defaultOperators
                    );
                });

                it("can be re-authorized", async function() {
                    const { logs } = await this.token.authorizeOperator(
                        newOperator,
                        {
                            from: holder
                        }
                    );
                    expectEvent.inLogs(logs, "AuthorizedOperator", {
                        operator: newOperator,
                        tokenHolder: holder
                    });

                    expect(
                        await this.token.isOperatorFor(newOperator, holder)
                    ).to.equal(true);
                });

                it("can be revoked", async function() {
                    const { logs } = await this.token.revokeOperator(
                        newOperator,
                        {
                            from: holder
                        }
                    );
                    expectEvent.inLogs(logs, "RevokedOperator", {
                        operator: newOperator,
                        tokenHolder: holder
                    });

                    expect(
                        await this.token.isOperatorFor(newOperator, holder)
                    ).to.equal(false);
                });
            });

            describe("default operators", function() {
                it("can be re-authorized", async function() {
                    const { logs } = await this.token.authorizeOperator(
                        defaultOperatorA,
                        {
                            from: holder
                        }
                    );
                    expectEvent.inLogs(logs, "AuthorizedOperator", {
                        operator: defaultOperatorA,
                        tokenHolder: holder
                    });

                    expect(
                        await this.token.isOperatorFor(defaultOperatorA, holder)
                    ).to.equal(true);
                });

                it("can be revoked", async function() {
                    const { logs } = await this.token.revokeOperator(
                        defaultOperatorA,
                        {
                            from: holder
                        }
                    );
                    expectEvent.inLogs(logs, "RevokedOperator", {
                        operator: defaultOperatorA,
                        tokenHolder: holder
                    });

                    expect(
                        await this.token.isOperatorFor(defaultOperatorA, holder)
                    ).to.equal(false);
                });

                it("cannot be revoked for themselves", async function() {
                    await expectRevert(
                        this.token.revokeOperator(defaultOperatorA, {
                            from: defaultOperatorA
                        }),
                        "ERC777Operators: revoking self as operator"
                    );
                });

                context("with revoked default operator", function() {
                    beforeEach(async function() {
                        await this.token.revokeOperator(defaultOperatorA, {
                            from: holder
                        });
                    });

                    it("default operator is not revoked for other holders", async function() {
                        expect(
                            await this.token.isOperatorFor(
                                defaultOperatorA,
                                anyone
                            )
                        ).to.equal(true);
                    });

                    it("other default operators are not revoked", async function() {
                        expect(
                            await this.token.isOperatorFor(
                                defaultOperatorB,
                                holder
                            )
                        ).to.equal(true);
                    });

                    it("default operators list is not modified", async function() {
                        expect(
                            await this.token.defaultOperators()
                        ).to.deep.equal(defaultOperators);
                    });

                    it("revoked default operator can be re-authorized", async function() {
                        const {
                            logs
                        } = await this.token.authorizeOperator(
                            defaultOperatorA,
                            { from: holder }
                        );
                        expectEvent.inLogs(logs, "AuthorizedOperator", {
                            operator: defaultOperatorA,
                            tokenHolder: holder
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

        describe("send and receive hooks", function() {
            const amount = toWad(1);
            const operator = defaultOperatorA;

            describe("tokensReceived", function() {
                beforeEach(function() {
                    this.sender = holder;
                });

                describe("with no ERC777TokensRecipient implementer", function() {
                    describe("with contract recipient", function() {
                        beforeEach(async function() {
                            this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                            this.recipient = this.tokensRecipientImplementer.address;
                        });

                        it("send reverts", async function() {
                            await expectRevert(
                                this.token.send(this.recipient, amount, data, {
                                    from: holder
                                }),
                                "SuperToken: not an ERC777TokensRecipient"
                            );
                        });

                        it("operatorSend reverts", async function() {
                            await expectRevert(
                                this.token.operatorSend(
                                    this.sender,
                                    this.recipient,
                                    amount,
                                    data,
                                    operatorData,
                                    { from: operator }
                                ),
                                "SuperToken: not an ERC777TokensRecipient"
                            );
                        });

                        it("mint (internal) reverts", async function() {
                            await expectRevert(
                                this.token.mintInternal(
                                    this.recipient,
                                    amount,
                                    data,
                                    operatorData,
                                    { from: operator }
                                ),
                                "SuperToken: not an ERC777TokensRecipient"
                            );
                        });

                        it("mint (internal) to zero address reverts", async function() {
                            await expectRevert(
                                this.token.mintInternal(
                                    ZERO_ADDRESS,
                                    amount,
                                    data,
                                    operatorData,
                                    { from: operator }
                                ),
                                "SuperToken: mint to zero address"
                            );
                        });

                        it("(ERC20) transfer succeeds", async function() {
                            await web3tx(
                                this.token.upgrade,
                                "SuperToken.upgrade 2 from holder"
                            )(toWad(2), {
                                from: holder
                            });
                            await this.token.transfer(this.recipient, amount, {
                                from: holder
                            });
                        });

                        it("(ERC20) transferFrom succeeds", async function() {
                            await web3tx(
                                this.token.upgrade,
                                "SuperToken.upgrade 2 from holder"
                            )(toWad(2), {
                                from: holder
                            });
                            const approved = anyone;
                            await this.token.approve(approved, amount, {
                                from: this.sender
                            });
                            await this.token.transferFrom(
                                this.sender,
                                this.recipient,
                                amount,
                                { from: approved }
                            );
                        });
                    });
                });

                describe("with ERC777TokensRecipient implementer", function() {
                    describe("with contract as implementer for an externally owned account", function() {
                        beforeEach(async function() {
                            this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                            this.recipient = anyone;

                            await this.tokensRecipientImplementer.recipientFor(
                                this.recipient
                            );

                            await erc1820.setInterfaceImplementer(
                                this.recipient,
                                web3.utils.soliditySha3(
                                    "ERC777TokensRecipient"
                                ),
                                this.tokensRecipientImplementer.address,
                                { from: this.recipient }
                            );
                        });

                        shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
                            operator,
                            amount,
                            data,
                            operatorData
                        );
                    });

                    describe("with contract as implementer for another contract", function() {
                        beforeEach(async function() {
                            this.recipientContract = await ERC777SenderRecipientMock.new();
                            this.recipient = this.recipientContract.address;

                            this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                            await this.tokensRecipientImplementer.recipientFor(
                                this.recipient
                            );
                            await this.recipientContract.registerRecipient(
                                this.tokensRecipientImplementer.address
                            );
                        });

                        shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
                            operator,
                            amount,
                            data,
                            operatorData
                        );
                    });

                    describe("with contract as implementer for itself", function() {
                        beforeEach(async function() {
                            this.tokensRecipientImplementer = await ERC777SenderRecipientMock.new();
                            this.recipient = this.tokensRecipientImplementer.address;

                            await this.tokensRecipientImplementer.recipientFor(
                                this.recipient
                            );
                        });

                        shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
                            operator,
                            amount,
                            data,
                            operatorData
                        );
                    });
                });
            });

            describe("tokensToSend", function() {
                beforeEach(function() {
                    this.recipient = anyone;
                });

                describe("with a contract as implementer for an externally owned account", function() {
                    beforeEach(async function() {
                        this.tokensSenderImplementer = await ERC777SenderRecipientMock.new();
                        this.sender = holder;

                        await this.tokensSenderImplementer.senderFor(
                            this.sender
                        );

                        await erc1820.setInterfaceImplementer(
                            this.sender,
                            web3.utils.soliditySha3("ERC777TokensSender"),
                            this.tokensSenderImplementer.address,
                            { from: this.sender }
                        );
                    });

                    shouldBehaveLikeERC777SendBurnWithSendHook(
                        operator,
                        amount,
                        data,
                        operatorData
                    );
                });

                describe("with contract as implementer for another contract", function() {
                    beforeEach(async function() {
                        this.senderContract = await ERC777SenderRecipientMock.new();
                        this.sender = this.senderContract.address;

                        this.tokensSenderImplementer = await ERC777SenderRecipientMock.new();
                        await this.tokensSenderImplementer.senderFor(
                            this.sender
                        );
                        await this.senderContract.registerSender(
                            this.tokensSenderImplementer.address
                        );
                        await this.senderContract.recipientFor(this.sender);
                        await this.token.send(this.sender, amount, data, {
                            from: holder
                        });
                    });

                    shouldBehaveLikeERC777SendBurnWithSendHook(
                        operator,
                        amount,
                        data,
                        operatorData
                    );
                });

                describe("with a contract as implementer for itself", function() {
                    beforeEach(async function() {
                        this.tokensSenderImplementer = await ERC777SenderRecipientMock.new();
                        this.sender = this.tokensSenderImplementer.address;

                        await this.tokensSenderImplementer.senderFor(
                            this.sender
                        );
                        await this.tokensSenderImplementer.recipientFor(
                            this.sender
                        );
                        await this.token.send(this.sender, amount, data, {
                            from: holder
                        });
                    });

                    shouldBehaveLikeERC777SendBurnWithSendHook(
                        operator,
                        amount,
                        data,
                        operatorData
                    );
                });
            });
        });
    });

    context("with no default operators", function() {
        it("default operators list is empty", async function() {
            expect(await this.token.defaultOperators()).to.deep.equal([]);
        });
    });
});
