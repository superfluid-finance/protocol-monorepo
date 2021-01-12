const {
    constants,
    expectEvent,
    expectRevert
} = require("@openzeppelin/test-helpers");

const { expect } = require("chai");

const { web3tx, toBN } = require("@decentral.ee/web3-helpers");

const {
    shouldBehaveLikeERC20,
    shouldBehaveLikeERC20Approve
} = require("./ERC20.behavior");

const { ZERO_ADDRESS } = constants;

const TestEnvironment = require("../../TestEnvironment");

contract("SuperToken's ERC20 compliance", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 4), {
        isTruffle: true,
        useMocks: true
    });
    const { alice, bob, carol } = t.aliases;
    const initialSupply = toBN(100);

    before(async () => {
        await t.reset();
    });

    beforeEach(async function() {
        await t.createNewToken({ doUpgrade: false });
        this.token = t.contracts.superToken;
        await web3tx(
            this.token.upgrade,
            `Upgrade initialSupply amount of token for ${alice}`
        )(initialSupply, {
            from: alice
        });
    });

    describe("okay", () => {
        describe("let's try", () => {
            it("hehehe", () => {
                console.log("yea");
            });
        });
    });

    describe("ERC20 compliance", () => {
        shouldBehaveLikeERC20("SuperToken", initialSupply, alice, bob, carol);
    });

    describe("decrease allowance", function() {
        describe("when the spender is not the zero address", function() {
            const spender = bob;

            function shouldDecreaseApproval(amount) {
                describe("when there was no approved amount before", function() {
                    it("reverts", async function() {
                        await expectRevert(
                            this.token.decreaseAllowance(spender, amount, {
                                from: alice
                            }),
                            "SuperToken: decreased allowance below zero"
                        );
                    });
                });

                describe("when the spender had an approved amount", function() {
                    const approvedAmount = amount;

                    beforeEach(async function() {
                        ({ logs: this.logs } = await this.token.approve(
                            spender,
                            approvedAmount,
                            { from: alice }
                        ));
                    });

                    it("emits an approval event", async function() {
                        const {
                            logs
                        } = await this.token.decreaseAllowance(
                            spender,
                            approvedAmount,
                            { from: alice }
                        );

                        expectEvent.inLogs(logs, "Approval", {
                            owner: alice,
                            spender: bob,
                            value: toBN(0)
                        });
                    });

                    it("decreases the spender allowance subtracting the requested amount", async function() {
                        await this.token.decreaseAllowance(
                            spender,
                            approvedAmount.subn(1),
                            { from: alice }
                        );

                        expect(
                            await this.token.allowance(alice, spender),
                            toBN(1)
                        );
                    });

                    it("sets the allowance to zero when all allowance is removed", async function() {
                        await this.token.decreaseAllowance(
                            spender,
                            approvedAmount,
                            { from: alice }
                        );
                        expect(
                            await this.token.allowance(alice, spender),
                            toBN(0)
                        );
                    });

                    it("reverts when more than the full allowance is removed", async function() {
                        await expectRevert(
                            this.token.decreaseAllowance(
                                spender,
                                approvedAmount.addn(1),
                                { from: alice }
                            ),
                            "SuperToken: decreased allowance below zero"
                        );
                    });
                });
            }

            describe("when the sender has enough balance", function() {
                const amount = initialSupply;

                shouldDecreaseApproval(amount);
            });

            describe("when the sender does not have enough balance", function() {
                const amount = initialSupply.addn(1);

                shouldDecreaseApproval(amount);
            });
        });

        describe("when the spender is the zero address", function() {
            const amount = initialSupply;
            const spender = ZERO_ADDRESS;

            it("reverts", async function() {
                await expectRevert(
                    this.token.decreaseAllowance(spender, amount, {
                        from: alice
                    }),
                    "SuperToken: decreased allowance below zero"
                );
            });
        });
    });

    describe("increase allowance", function() {
        const amount = initialSupply;

        describe("when the spender is not the zero address", function() {
            const spender = bob;

            describe("when the sender has enough balance", function() {
                it("emits an approval event", async function() {
                    const { logs } = await this.token.increaseAllowance(
                        spender,
                        amount,
                        { from: alice }
                    );

                    expectEvent.inLogs(logs, "Approval", {
                        owner: alice,
                        spender: spender,
                        value: amount
                    });
                });

                describe("when there was no approved amount before", function() {
                    it("approves the requested amount", async function() {
                        await this.token.increaseAllowance(spender, amount, {
                            from: alice
                        });

                        expect(
                            await this.token.allowance(alice, spender),
                            toBN(amount)
                        );
                    });
                });

                describe("when the spender had an approved amount", function() {
                    beforeEach(async function() {
                        await this.token.approve(spender, toBN(1), {
                            from: alice
                        });
                    });

                    it("increases the spender allowance adding the requested amount", async function() {
                        await this.token.increaseAllowance(spender, amount, {
                            from: alice
                        });

                        expect(
                            await this.token.allowance(alice, spender),
                            toBN(amount.addn(1))
                        );
                    });
                });
            });

            describe("when the sender does not have enough balance", function() {
                const amount = initialSupply.addn(1);

                it("emits an approval event", async function() {
                    const { logs } = await this.token.increaseAllowance(
                        spender,
                        amount,
                        { from: alice }
                    );

                    expectEvent.inLogs(logs, "Approval", {
                        owner: alice,
                        spender: spender,
                        value: amount
                    });
                });

                describe("when there was no approved amount before", function() {
                    it("approves the requested amount", async function() {
                        await this.token.increaseAllowance(spender, amount, {
                            from: alice
                        });

                        expect(
                            await this.token.allowance(alice, spender),
                            toBN(amount)
                        );
                    });
                });

                describe("when the spender had an approved amount", function() {
                    beforeEach(async function() {
                        await this.token.approve(spender, toBN(1), {
                            from: alice
                        });
                    });

                    it("increases the spender allowance adding the requested amount", async function() {
                        await this.token.increaseAllowance(spender, amount, {
                            from: alice
                        });

                        expect(
                            await this.token.allowance(alice, spender),
                            toBN(amount.addn(1))
                        );
                    });
                });
            });
        });

        describe("when the spender is the zero address", function() {
            const spender = ZERO_ADDRESS;

            it("reverts", async function() {
                await expectRevert(
                    this.token.increaseAllowance(spender, amount, {
                        from: alice
                    }),
                    "SuperToken: approve to zero address"
                );
            });
        });
    });

    describe("_transfer", function() {
        // it is already tesetd under shouldBehaveLikeERC20
        // shouldBehaveLikeERC20Transfer("ERC20", alice, bob, initialSupply, function (from, to, amount) {
        //     return this.token.transferInternal(from, to, amount);
        // });

        describe("when the sender is the zero address", function() {
            it("reverts", async function() {
                await expectRevert(
                    this.token.transferInternal(
                        ZERO_ADDRESS,
                        bob,
                        initialSupply
                    ),
                    "SuperToken: transfer from zero address"
                );
            });
        });
    });

    describe("_approve", function() {
        shouldBehaveLikeERC20Approve(
            "ERC20",
            alice,
            bob,
            initialSupply,
            function(owner, spender, amount) {
                return this.token.approveInternal(owner, spender, amount);
            }
        );

        describe("when the owner is the zero address", function() {
            it("reverts", async function() {
                await expectRevert(
                    this.token.approveInternal(
                        ZERO_ADDRESS,
                        bob,
                        initialSupply
                    ),
                    "SuperToken: approve from zero address"
                );
            });
        });
    });
});
