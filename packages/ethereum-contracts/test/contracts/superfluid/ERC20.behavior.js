// NOTE: copied and modified from https://github.com/OpenZeppelin/openzeppelin-contracts/
const {constants} = require("@openzeppelin/test-helpers");
const {expect} = require("chai");
const {ethers} = require("hardhat");
const {ZERO_ADDRESS} = constants;
const {
    expectRevertedWith,
    expectCustomError,
} = require("../../utils/expectRevert");
const {toBN} = require("../utils/helpers");

function shouldBehaveLikeERC20(errorPrefix, initialSupply, setupAccounts) {
    let initialHolder, recipient, anotherAccount;

    before(() => {
        ({initialHolder, recipient, anotherAccount} = setupAccounts());
    });

    describe("total supply", function () {
        it("returns the total amount of tokens", async function () {
            expect((await this.token.totalSupply()).toString()).to.be.equal(
                initialSupply.toString()
            );
        });
    });

    describe("balanceOf", function () {
        describe("when the requested account has no tokens", function () {
            it("returns zero", async function () {
                expect(
                    (await this.token.balanceOf(anotherAccount)).toString()
                ).to.be.equal("0");
            });
        });

        describe("when the requested account has some tokens", function () {
            it("returns the total amount of tokens", async function () {
                expect(
                    (await this.token.balanceOf(initialHolder)).toString()
                ).to.be.equal(initialSupply);
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
            async function (from, to, value) {
                const signer = await ethers.getSigner(from);
                return this.token.connect(signer).transfer(to, value);
            }
        );
    });

    describe("transfer from", function () {
        let spender, tokenOwner, to;
        let spenderSigner, tokenOwnerSigner;

        before(async () => {
            spender = recipient;
            tokenOwner = initialHolder;
            to = anotherAccount;
            spenderSigner = await ethers.getSigner(spender);
            tokenOwnerSigner = await ethers.getSigner(tokenOwner);
        });

        describe("when the token owner is not the zero address", function () {
            describe("when the recipient is not the zero address", function () {
                describe("when the spender has enough approved balance", function () {
                    beforeEach(async function () {
                        await this.token
                            .connect(tokenOwnerSigner)
                            .approve(spender, initialSupply);
                    });

                    describe("when the token owner has enough balance", function () {
                        const amount = initialSupply;

                        it("transfers the requested amount", async function () {
                            await this.token
                                .connect(spenderSigner)
                                .transferFrom(tokenOwner, to, amount);

                            expect(
                                (
                                    await this.token.balanceOf(tokenOwner)
                                ).toString()
                            ).to.be.equal("0");

                            expect(
                                (await this.token.balanceOf(to)).toString()
                            ).to.be.equal(amount);
                        });

                        it("decreases the spender allowance", async function () {
                            await this.token
                                .connect(spenderSigner)
                                .transferFrom(tokenOwner, to, amount);

                            expect(
                                (
                                    await this.token.allowance(
                                        tokenOwner,
                                        spender
                                    )
                                ).toString()
                            ).to.be.equal("0");
                        });

                        it("emits a transfer event", async function () {
                            await expect(
                                this.token
                                    .connect(spenderSigner)
                                    .transferFrom(tokenOwner, to, amount)
                            )
                                .to.emit(this.token, "Transfer")
                                .withArgs(tokenOwner, to, amount);
                        });

                        it("emits an approval event", async function () {
                            await expect(
                                this.token
                                    .connect(spenderSigner)
                                    .transferFrom(tokenOwner, to, amount)
                            )
                                .to.emit(this.token, "Approval")
                                .withArgs(
                                    tokenOwner,
                                    spender,
                                    await this.token.allowance(
                                        tokenOwner,
                                        spender
                                    )
                                );
                        });
                    });

                    describe("when the token owner does not have enough balance", function () {
                        const amount = initialSupply.add(1);

                        it("reverts", async function () {
                            await expectRevertedWith(
                                this.token
                                    .connect(spenderSigner)
                                    .transferFrom(tokenOwner, to, amount),
                                "SuperfluidToken: move amount exceeds balance"
                            );
                        });
                    });
                });

                describe("when the spender does not have enough approved balance", function () {
                    beforeEach(async function () {
                        await this.token
                            .connect(tokenOwnerSigner)
                            .approve(spender, initialSupply.sub(1));
                    });

                    describe("when the token owner has enough balance", function () {
                        const amount = initialSupply;

                        it("reverts", async function () {
                            await expectRevertedWith(
                                this.token
                                    .connect(spenderSigner)
                                    .transferFrom(tokenOwner, to, amount),
                                "SuperToken: transfer amount exceeds allowance"
                            );
                        });
                    });

                    describe("when the token owner does not have enough balance", function () {
                        const amount = initialSupply.add(1);

                        it("reverts", async function () {
                            await expectRevertedWith(
                                this.token
                                    .connect(spenderSigner)
                                    .transferFrom(tokenOwner, to, amount),
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
                    await this.token.approve(spender, amount);
                });

                it("reverts", async function () {
                    await expectCustomError(
                        this.token
                            .connect(spenderSigner)
                            .transferFrom(tokenOwner, to, amount),
                        this.token,
                        "SuperToken_TransferToZeroAddressNotAllowed"
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
                await expectCustomError(
                    this.token
                        .connect(spenderSigner)
                        .transferFrom(tokenOwner, to, amount),
                    this.token,
                    "SuperToken_TransferFromZeroAddressNotAllowed"
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
            async function (owner, spender, amount) {
                const signer = await ethers.getSigner(owner);
                return this.token.connect(signer).approve(spender, amount);
            }
        );
    });
}

function shouldBehaveLikeERC20Transfer(
    _errorPrefix,
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
            const amount = toBN(balance).add(1);

            it("reverts", async function () {
                await expectRevertedWith(
                    transfer.call(this, from, to, amount),
                    "SuperfluidToken: move amount exceeds balance"
                );
            });
        });

        describe("when the sender transfers all balance", function () {
            const amount = balance;

            it("transfers the requested amount", async function () {
                await transfer.call(this, from, to, amount);

                expect(
                    (await this.token.balanceOf(from)).toString()
                ).to.be.equal("0");

                expect((await this.token.balanceOf(to)).toString()).to.be.equal(
                    amount
                );
            });

            it("emits a transfer event", async function () {
                await expect(transfer.call(this, from, to, amount))
                    .to.emit(this.token, "Transfer")
                    .withArgs(from, to, amount.toString);
            });
        });

        describe("when the sender transfers zero tokens", function () {
            const amount = toBN("0");

            it("transfers the requested amount", async function () {
                await transfer.call(this, from, to, amount);

                expect((await this.token.balanceOf(from)).toString()).to.equal(
                    balance
                );

                expect((await this.token.balanceOf(to)).toString()).to.equal(
                    "0"
                );
            });

            it("emits a transfer event", async function () {
                await expect(transfer.call(this, from, to, amount))
                    .to.emit(this.token, "Transfer")
                    .withArgs(from, to, amount);
            });
        });
    });

    describe("when the recipient is the zero address", function () {
        it("reverts", async function () {
            await expectRevertedWith(
                transfer.call(this, from, ZERO_ADDRESS, balance),
                "SuperToken_TransferToZeroAddressNotAllowed"
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
                await expect(approve.call(this, owner, spender, amount))
                    .to.emit(this.token, "Approval")
                    .withArgs(owner, spender, amount);
            });

            describe("when there was no approved amount before", function () {
                it("approves the requested amount", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        (await this.token.allowance(owner, spender)).toString()
                    ).to.equal(amount);
                });
            });

            describe("when the spender had an approved amount", function () {
                beforeEach(async function () {
                    await approve.call(this, owner, spender, toBN(1));
                });

                it("approves the requested amount and replaces the previous one", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        (await this.token.allowance(owner, spender)).toString()
                    ).to.equal(amount);
                });
            });
        });

        describe("when the sender does not have enough balance", function () {
            const amount = supply.add(1);

            it("emits an approval event", async function () {
                await expect(approve.call(this, owner, spender, amount))
                    .to.emit(this.token, "Approval")
                    .withArgs(owner, spender, amount);
            });

            describe("when there was no approved amount before", function () {
                it("approves the requested amount", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        (await this.token.allowance(owner, spender)).toString()
                    ).to.equal(amount);
                });
            });

            describe("when the spender had an approved amount", function () {
                beforeEach(async function () {
                    await approve.call(this, owner, spender, toBN(1));
                });

                it("approves the requested amount and replaces the previous one", async function () {
                    await approve.call(this, owner, spender, amount);

                    expect(
                        (await this.token.allowance(owner, spender)).toString()
                    ).to.equal(amount);
                });
            });
        });
    });

    describe("when the spender is the zero address", function () {
        it("reverts", async function () {
            await expectCustomError(
                approve.call(this, owner, ZERO_ADDRESS, supply),
                this.token,
                "SuperToken_ApproveToZeroAddressNotAllowed"
            );
        });
    });
}

module.exports = {
    shouldBehaveLikeERC20,
    shouldBehaveLikeERC20Transfer,
    shouldBehaveLikeERC20Approve,
};
