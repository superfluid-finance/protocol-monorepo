// NOTE: copied and modified from https://github.com/OpenZeppelin/openzeppelin-contracts/
const {
    BN,
    constants,
    expectEvent,
    expectRevert,
} = require("@openzeppelin/test-helpers");
const {expect} = require("chai");
const {ZERO_ADDRESS} = constants;

function shouldBehaveLikeERC20(errorPrefix, initialSupply, setupAccounts) {
    let initialHolder, recipient, anotherAccount;

    before(() => {
        ({initialHolder, recipient, anotherAccount} = setupAccounts());
    });

    describe("total supply", function () {
        it("returns the total amount of tokens", async function () {
            expect(await this.token.totalSupply()).to.be.bignumber.equal(
                initialSupply
            );
        });
    });

    describe("balanceOf", function () {
        describe("when the requested account has no tokens", function () {
            it("returns zero", async function () {
                expect(
                    await this.token.balanceOf(anotherAccount)
                ).to.be.bignumber.equal("0");
            });
        });

        describe("when the requested account has some tokens", function () {
            it("returns the total amount of tokens", async function () {
                expect(
                    await this.token.balanceOf(initialHolder)
                ).to.be.bignumber.equal(initialSupply);
            });
        });
    });

    describe("transfer", function () {
        shouldBehaveLikeERC20Transfer(
            errorPrefix,
            initialSupply,
            () => ({
                from: initialHolder,
                to: recipient,
            }),
            function (from, to, value) {
                return this.token.transfer(to, value, {from});
            }
        );
    });

    describe("transfer from", function () {
        let spender, tokenOwner, to;

        before(() => {
            spender = recipient;
            tokenOwner = initialHolder;
            to = anotherAccount;
        });

        describe("when the token owner is not the zero address", function () {
            describe("when the recipient is not the zero address", function () {
                describe("when the spender has enough approved balance", function () {
                    beforeEach(async function () {
                        await this.token.approve(spender, initialSupply, {
                            from: initialHolder,
                        });
                    });

                    describe("when the token owner has enough balance", function () {
                        const amount = initialSupply;

                        it("transfers the requested amount", async function () {
                            await this.token.transferFrom(
                                tokenOwner,
                                to,
                                amount,
                                {from: spender}
                            );

                            expect(
                                await this.token.balanceOf(tokenOwner)
                            ).to.be.bignumber.equal("0");

                            expect(
                                await this.token.balanceOf(to)
                            ).to.be.bignumber.equal(amount);
                        });

                        it("decreases the spender allowance", async function () {
                            await this.token.transferFrom(
                                tokenOwner,
                                to,
                                amount,
                                {from: spender}
                            );

                            expect(
                                await this.token.allowance(tokenOwner, spender)
                            ).to.be.bignumber.equal("0");
                        });

                        it("emits a transfer event", async function () {
                            const {logs} = await this.token.transferFrom(
                                tokenOwner,
                                to,
                                amount,
                                {from: spender}
                            );

                            expectEvent.inLogs(logs, "Transfer", {
                                from: tokenOwner,
                                to: to,
                                value: amount,
                            });
                        });

                        it("emits an approval event", async function () {
                            const {logs} = await this.token.transferFrom(
                                tokenOwner,
                                to,
                                amount,
                                {from: spender}
                            );

                            expectEvent.inLogs(logs, "Approval", {
                                owner: tokenOwner,
                                spender: spender,
                                value: await this.token.allowance(
                                    tokenOwner,
                                    spender
                                ),
                            });
                        });
                    });

                    describe("when the token owner does not have enough balance", function () {
                        const amount = initialSupply.addn(1);

                        it("reverts", async function () {
                            await expectRevert(
                                this.token.transferFrom(
                                    tokenOwner,
                                    to,
                                    amount,
                                    {from: spender}
                                ),
                                "SuperfluidToken: move amount exceeds balance"
                            );
                        });
                    });
                });

                describe("when the spender does not have enough approved balance", function () {
                    beforeEach(async function () {
                        await this.token.approve(
                            spender,
                            initialSupply.subn(1),
                            {from: tokenOwner}
                        );
                    });

                    describe("when the token owner has enough balance", function () {
                        const amount = initialSupply;

                        it("reverts", async function () {
                            await expectRevert(
                                this.token.transferFrom(
                                    tokenOwner,
                                    to,
                                    amount,
                                    {from: spender}
                                ),
                                "SuperToken: transfer amount exceeds allowance"
                            );
                        });
                    });

                    describe("when the token owner does not have enough balance", function () {
                        const amount = initialSupply.addn(1);

                        it("reverts", async function () {
                            await expectRevert(
                                this.token.transferFrom(
                                    tokenOwner,
                                    to,
                                    amount,
                                    {from: spender}
                                ),
                                "SuperfluidToken: move amount exceeds balance"
                            );
                        });
                    });
                });
            });

            describe("when the recipient is the zero address", function () {
                const amount = initialSupply;
                const to = ZERO_ADDRESS;

                beforeEach(async function () {
                    await this.token.approve(spender, amount, {
                        from: tokenOwner,
                    });
                });

                it("reverts", async function () {
                    await expectRevert(
                        this.token.transferFrom(tokenOwner, to, amount, {
                            from: spender,
                        }),
                        "SuperToken: transfer to zero address"
                    );
                });
            });
        });

        describe("when the token owner is the zero address", function () {
            const amount = 0;
            const tokenOwner = ZERO_ADDRESS;
            let to;

            before(() => {
                to = recipient;
            });

            it("reverts", async function () {
                await expectRevert(
                    this.token.transferFrom(tokenOwner, to, amount, {
                        from: spender,
                    }),
                    "SuperToken: transfer from zero address"
                );
            });
        });
    });

    describe("approve", function () {
        shouldBehaveLikeERC20Approve(
            errorPrefix,
            initialSupply,
            () => ({
                owner: initialHolder,
                spender: recipient,
            }),
            function (owner, spender, amount) {
                return this.token.approve(spender, amount, {from: owner});
            }
        );
    });
}

function shouldBehaveLikeERC20Transfer(
    errorPrefix,
    balance,
    setupAccounts,
    transfer
) {
    let from, to;

    before(() => {
        ({from, to} = setupAccounts());
    });
    describe("when the recipient is not the zero address", function () {
        describe("when the sender does not have enough balance", function () {
            const amount = balance.addn(1);

            it("reverts", async function () {
                await expectRevert(
                    transfer.call(this, from, to, amount),
                    "SuperfluidToken: move amount exceeds balance"
                );
            });
        });

        describe("when the sender transfers all balance", function () {
            const amount = balance;

            it("transfers the requested amount", async function () {
                await transfer.call(this, from, to, amount);

                expect(await this.token.balanceOf(from)).to.be.bignumber.equal(
                    "0"
                );

                expect(await this.token.balanceOf(to)).to.be.bignumber.equal(
                    amount
                );
            });

            it("emits a transfer event", async function () {
                const {logs} = await transfer.call(this, from, to, amount);

                expectEvent.inLogs(logs, "Transfer", {
                    from,
                    to,
                    value: amount,
                });
            });
        });

        describe("when the sender transfers zero tokens", function () {
            const amount = new BN("0");

            it("transfers the requested amount", async function () {
                await transfer.call(this, from, to, amount);

                expect(await this.token.balanceOf(from)).to.be.bignumber.equal(
                    balance
                );

                expect(await this.token.balanceOf(to)).to.be.bignumber.equal(
                    "0"
                );
            });

            it("emits a transfer event", async function () {
                const {logs} = await transfer.call(this, from, to, amount);

                expectEvent.inLogs(logs, "Transfer", {
                    from,
                    to,
                    value: amount,
                });
            });
        });
    });

    describe("when the recipient is the zero address", function () {
        it("reverts", async function () {
            await expectRevert(
                transfer.call(this, from, ZERO_ADDRESS, balance),
                "SuperToken: transfer to zero address"
            );
        });
    });
}

function shouldBehaveLikeERC20Approve(
    errorPrefix,
    supply,
    setupAccounts,
    approve
) {
    let owner, spender;

    before(() => {
        ({owner, spender} = setupAccounts());
    });

    describe("when the spender is not the zero address", function () {
        describe("when the sender has enough balance", function () {
            const amount = supply;

            it("emits an approval event", async function () {
                const {logs} = await approve.call(this, owner, spender, amount);

                expectEvent.inLogs(logs, "Approval", {
                    owner: owner,
                    spender: spender,
                    value: amount,
                });
            });

            describe("when there was no approved amount before", function () {
                it("approves the requested amount", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        await this.token.allowance(owner, spender)
                    ).to.be.bignumber.equal(amount);
                });
            });

            describe("when the spender had an approved amount", function () {
                beforeEach(async function () {
                    await approve.call(this, owner, spender, new BN(1));
                });

                it("approves the requested amount and replaces the previous one", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        await this.token.allowance(owner, spender)
                    ).to.be.bignumber.equal(amount);
                });
            });
        });

        describe("when the sender does not have enough balance", function () {
            const amount = supply.addn(1);

            it("emits an approval event", async function () {
                const {logs} = await approve.call(this, owner, spender, amount);

                expectEvent.inLogs(logs, "Approval", {
                    owner: owner,
                    spender: spender,
                    value: amount,
                });
            });

            describe("when there was no approved amount before", function () {
                it("approves the requested amount", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        await this.token.allowance(owner, spender)
                    ).to.be.bignumber.equal(amount);
                });
            });

            describe("when the spender had an approved amount", function () {
                beforeEach(async function () {
                    await approve.call(this, owner, spender, new BN(1));
                });

                it("approves the requested amount and replaces the previous one", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        await this.token.allowance(owner, spender)
                    ).to.be.bignumber.equal(amount);
                });
            });
        });
    });

    describe("when the spender is the zero address", function () {
        it("reverts", async function () {
            await expectRevert(
                approve.call(this, owner, ZERO_ADDRESS, supply),
                "SuperToken: approve to zero address"
            );
        });
    });
}

module.exports = {
    shouldBehaveLikeERC20,
    shouldBehaveLikeERC20Transfer,
    shouldBehaveLikeERC20Approve,
};
