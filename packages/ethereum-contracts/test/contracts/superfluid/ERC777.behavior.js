// NOTE: copied and modified from https://github.com/OpenZeppelin/openzeppelin-contracts/
const {BN, constants, expectEvent} = require("@openzeppelin/test-helpers");
const {ZERO_ADDRESS} = constants;

const {expect} = require("chai");
const {expectRevertedWith} = require("../../utils/expectRevert");

const ERC777SenderRecipientMock = artifacts.require(
    "ERC777SenderRecipientMock"
);

function shouldBehaveLikeERC777DirectSendBurn(setupAccounts, data) {
    _shouldBehaveLikeERC777DirectSend(setupAccounts, data);
    _shouldBehaveLikeERC777DirectBurn(setupAccounts, data);
}

function shouldBehaveLikeERC777OperatorSendBurn(
    setupAccounts,
    data,
    operatorData
) {
    _shouldBehaveLikeERC777OperatorSend(setupAccounts, data, operatorData);
    _shouldBehaveLikeERC777OperatorBurn(setupAccounts, data, operatorData);
}

function shouldBehaveLikeERC777UnauthorizedOperatorSendBurn(
    setupAccounts,
    data,
    operatorData
) {
    _shouldBehaveLikeERC777UnauthorizedOperatorSend(
        setupAccounts,
        data,
        operatorData
    );
    _shouldBehaveLikeERC777UnauthorizedOperatorBurn(
        setupAccounts,
        data,
        operatorData
    );
}

function _shouldBehaveLikeERC777DirectSend(setupAccounts, data) {
    let holder, recipient;

    before(function () {
        ({holder, recipient} = setupAccounts());
    });

    describe("direct send", function () {
        describe("when the sender has tokens", function () {
            _shouldDirectSendTokens(
                () => ({
                    from: holder,
                    to: recipient,
                }),
                new BN("0"),
                data
            );
            _shouldDirectSendTokens(
                () => ({
                    from: holder,
                    to: recipient,
                }),
                new BN("1"),
                data
            );

            it("reverts when sending more than the balance", async function () {
                const balance = await this.token.balanceOf(holder);
                await expectRevertedWith(
                    this.token.send(recipient, balance.addn(1), data, {
                        from: holder,
                    }),
                    "revert"
                );
            });

            it("reverts when sending to the zero address", async function () {
                await expectRevertedWith(
                    this.token.send(ZERO_ADDRESS, new BN("1"), data, {
                        from: holder,
                    }),
                    "revert"
                );
            });
        });

        describe("when the sender has no tokens", function () {
            beforeEach(async function () {
                await _removeBalance(this.token, holder);
            });

            _shouldDirectSendTokens(
                () => ({
                    from: holder,
                    to: recipient,
                }),
                new BN("0"),
                data
            );

            it("reverts when sending a non-zero amount", async function () {
                await expectRevertedWith(
                    this.token.send(recipient, new BN("1"), data, {
                        from: holder,
                    }),
                    "revert"
                );
            });
        });
    });
}

function _shouldBehaveLikeERC777OperatorSend(
    setupAccounts,
    data,
    operatorData
) {
    let holder, recipient, operator;

    before(function () {
        ({holder, recipient, operator} = setupAccounts());
    });

    describe("operator send", function () {
        describe("when the sender has tokens", async function () {
            _shouldOperatorSendTokens(
                () => ({
                    from: holder,
                    operator: operator,
                    to: recipient,
                }),
                new BN("0"),
                data,
                operatorData
            );
            _shouldOperatorSendTokens(
                () => ({
                    from: holder,
                    operator: operator,
                    to: recipient,
                }),
                new BN("1"),
                data,
                operatorData
            );

            it("reverts when sending more than the balance", async function () {
                const balance = await this.token.balanceOf(holder);
                await expectRevertedWith(
                    this.token.operatorSend(
                        holder,
                        recipient,
                        balance.addn(1),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });

            it("reverts when sending to the zero address", async function () {
                await expectRevertedWith(
                    this.token.operatorSend(
                        holder,
                        ZERO_ADDRESS,
                        new BN("1"),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });
        });

        describe("when the sender has no tokens", function () {
            beforeEach(async function () {
                await _removeBalance(this.token, holder);
            });

            _shouldOperatorSendTokens(
                () => ({
                    from: holder,
                    operator: operator,
                    to: recipient,
                }),
                new BN("0"),
                data,
                operatorData
            );

            it("reverts when sending a non-zero amount", async function () {
                await expectRevertedWith(
                    this.token.operatorSend(
                        holder,
                        recipient,
                        new BN("1"),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });

            it("reverts when sending from the zero address", async function () {
                // This is not yet reflected in the spec
                await expectRevertedWith(
                    this.token.operatorSend(
                        ZERO_ADDRESS,
                        recipient,
                        new BN("0"),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });
        });
    });
}

function _shouldBehaveLikeERC777UnauthorizedOperatorSend(
    setupAccounts,
    data,
    operatorData
) {
    let holder, recipient, operator;

    before(function () {
        ({holder, recipient, operator} = setupAccounts());
    });

    describe("operator send", function () {
        it("reverts", async function () {
            await expectRevertedWith(
                this.token.operatorSend(
                    holder,
                    recipient,
                    new BN("0"),
                    data,
                    operatorData,
                    {
                        from: operator,
                    }
                ),
                "revert"
            );
        });
    });
}

function _shouldBehaveLikeERC777DirectBurn(setupAccounts, data) {
    let holder;

    before(function () {
        ({holder} = setupAccounts());
    });

    describe("direct burn", function () {
        describe("when the sender has tokens", function () {
            _shouldDirectBurnTokens(
                () => ({
                    from: holder,
                }),
                new BN("0"),
                data
            );
            _shouldDirectBurnTokens(
                () => ({
                    from: holder,
                }),
                new BN("1"),
                data
            );

            it("reverts when burning more than the balance", async function () {
                const balance = await this.token.balanceOf(holder);
                await expectRevertedWith(
                    this.token.burn(balance.addn(1), data, {from: holder}),
                    "revert"
                );
            });
        });

        describe("when the sender has no tokens", function () {
            beforeEach(async function () {
                await _removeBalance(this.token, holder);
            });

            _shouldDirectBurnTokens(
                () => ({
                    from: holder,
                }),
                new BN("0"),
                data
            );

            it("reverts when burning a non-zero amount", async function () {
                await expectRevertedWith(
                    this.token.burn(new BN("1"), data, {from: holder}),
                    "revert"
                );
            });
        });
    });
}

function _shouldBehaveLikeERC777OperatorBurn(
    setupAccounts,
    data,
    operatorData
) {
    let holder, operator;

    before(function () {
        ({holder, operator} = setupAccounts());
    });

    describe("operator burn", function () {
        describe("when the sender has tokens", async function () {
            _shouldOperatorBurnTokens(
                () => ({
                    from: holder,
                    operator,
                }),
                new BN("0"),
                data,
                operatorData
            );
            _shouldOperatorBurnTokens(
                () => ({
                    from: holder,
                    operator,
                }),
                new BN("1"),
                data,
                operatorData
            );

            it("reverts when burning more than the balance", async function () {
                const balance = await this.token.balanceOf(holder);
                await expectRevertedWith(
                    this.token.operatorBurn(
                        holder,
                        balance.addn(1),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });
        });

        describe("when the sender has no tokens", function () {
            beforeEach(async function () {
                await _removeBalance(this.token, holder);
            });

            _shouldOperatorBurnTokens(
                () => ({
                    from: holder,
                    operator,
                }),
                new BN("0"),
                data,
                operatorData
            );

            it("reverts when burning a non-zero amount", async function () {
                await expectRevertedWith(
                    this.token.operatorBurn(
                        holder,
                        new BN("1"),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });

            it("reverts when burning from the zero address", async function () {
                // This is not yet reflected in the spec
                await expectRevertedWith(
                    this.token.operatorBurn(
                        ZERO_ADDRESS,
                        new BN("0"),
                        data,
                        operatorData,
                        {from: operator}
                    ),
                    "revert"
                );
            });
        });
    });
}

function _shouldBehaveLikeERC777UnauthorizedOperatorBurn(
    setupAccounts,
    data,
    operatorData
) {
    let holder, operator;

    before(function () {
        ({holder, operator} = setupAccounts());
    });

    describe("operator burn", function () {
        it("reverts", async function () {
            await expectRevertedWith(
                this.token.operatorBurn(
                    holder,
                    new BN("0"),
                    data,
                    operatorData,
                    {from: operator}
                ),
                "revert"
            );
        });
    });
}

function _shouldDirectSendTokens(setupAccounts, amount, data) {
    let from, to;

    before(function () {
        ({from, to} = setupAccounts());
    });

    _shouldSendTokens(
        () => ({
            from,
            operator: null,
            to,
        }),
        amount,
        data,
        null
    );
}

function _shouldOperatorSendTokens(setupAccounts, amount, data, operatorData) {
    let from, operator, to;

    before(function () {
        ({from, operator, to} = setupAccounts());
    });

    _shouldSendTokens(
        () => ({
            from,
            operator,
            to,
        }),
        amount,
        data,
        operatorData
    );
}

function _shouldSendTokens(setupAccounts, amount, data, operatorData) {
    let from, operator, to;
    let operatorCall;

    before(function () {
        ({from, operator, to} = setupAccounts());
        operatorCall = operator !== null;
    });

    it(`${
        operatorCall ? "operator " : ""
    }can send an amount of ${amount}`, async function () {
        const initialTotalSupply = await this.token.totalSupply();
        const initialFromBalance = await this.token.balanceOf(from);
        const initialToBalance = await this.token.balanceOf(to);

        let logs;
        if (!operatorCall) {
            ({logs} = await this.token.send(to, amount, data, {from}));
            expectEvent.inLogs(logs, "Sent", {
                operator: from,
                from,
                to,
                amount,
                data,
                operatorData: null,
            });
        } else {
            ({logs} = await this.token.operatorSend(
                from,
                to,
                amount,
                data,
                operatorData,
                {from: operator}
            ));
            expectEvent.inLogs(logs, "Sent", {
                operator,
                from,
                to,
                amount,
                data,
                operatorData,
            });
        }

        expectEvent.inLogs(logs, "Transfer", {
            from,
            to,
            value: amount,
        });

        const finalTotalSupply = await this.token.totalSupply();
        const finalFromBalance = await this.token.balanceOf(from);
        const finalToBalance = await this.token.balanceOf(to);

        expect(finalTotalSupply).to.be.bignumber.equal(initialTotalSupply);
        expect(finalToBalance.sub(initialToBalance)).to.be.bignumber.equal(
            amount
        );
        expect(finalFromBalance.sub(initialFromBalance)).to.be.bignumber.equal(
            amount.neg()
        );
    });
}

function _shouldDirectBurnTokens(setupAccounts, amount, data) {
    let from;

    before(() => {
        ({from} = setupAccounts());
    });

    _shouldBurnTokens(
        () => ({
            from,
            operator: null,
        }),
        amount,
        data,
        null
    );
}

function _shouldOperatorBurnTokens(setupAccounts, amount, data, operatorData) {
    let from, operator;

    before(() => {
        ({from, operator} = setupAccounts());
    });

    _shouldBurnTokens(
        () => ({
            from,
            operator,
        }),
        amount,
        data,
        operatorData
    );
}

function _shouldBurnTokens(setupAccounts, amount, data, operatorData) {
    let from, operator;
    let operatorCall;

    before(() => {
        ({from, operator} = setupAccounts());
        operatorCall = operator !== null;
    });

    it(`${
        operatorCall ? "operator " : ""
    }can burn an amount of ${amount}`, async function () {
        const initialTotalSupply = await this.token.totalSupply();
        const initialFromBalance = await this.token.balanceOf(from);

        let logs;
        if (!operatorCall) {
            ({logs} = await this.token.burn(amount, data, {from}));
            expectEvent.inLogs(logs, "Burned", {
                operator: from,
                from,
                amount,
                data,
                operatorData: null,
            });
        } else {
            ({logs} = await this.token.operatorBurn(
                from,
                amount,
                data,
                operatorData,
                {from: operator}
            ));
            expectEvent.inLogs(logs, "Burned", {
                operator,
                from,
                amount,
                data,
                operatorData,
            });
        }

        expectEvent.inLogs(logs, "Transfer", {
            from,
            to: ZERO_ADDRESS,
            value: amount,
        });

        const finalTotalSupply = await this.token.totalSupply();
        const finalFromBalance = await this.token.balanceOf(from);

        expect(finalTotalSupply.sub(initialTotalSupply)).to.be.bignumber.equal(
            amount.neg()
        );
        expect(finalFromBalance.sub(initialFromBalance)).to.be.bignumber.equal(
            amount.neg()
        );
    });
}

function shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook(
    setupAccounts,
    amount,
    data,
    operatorData
) {
    let operator, sender, recipient;

    beforeEach(() => {
        ({operator, sender, recipient} = setupAccounts());
    });

    describe("when TokensRecipient reverts", function () {
        beforeEach(async function () {
            await this.tokensRecipientImplementer.setShouldRevertReceive(true);
        });

        it("send reverts", async function () {
            await expectRevertedWith(
                _sendFromHolder(this.token, sender, recipient, amount, data),
                "revert"
            );
        });

        it("operatorSend reverts", async function () {
            await expectRevertedWith(
                this.token.operatorSend(
                    sender,
                    recipient,
                    amount,
                    data,
                    operatorData,
                    {from: operator}
                ),
                "revert"
            );
        });

        it("mint (internal) reverts", async function () {
            await expectRevertedWith(
                this.token.mintInternal(recipient, amount, data, operatorData, {
                    from: operator,
                }),
                "revert"
            );
        });
    });

    describe("when TokensRecipient does not revert", function () {
        beforeEach(async function () {
            await this.tokensRecipientImplementer.setShouldRevertSend(false);
        });

        it("TokensRecipient receives send data and is called after state mutation", async function () {
            const {tx} = await _sendFromHolder(
                this.token,
                sender,
                recipient,
                amount,
                data
            );

            const postSenderBalance = await this.token.balanceOf(sender);
            const postRecipientBalance = await this.token.balanceOf(recipient);

            await _assertTokensReceivedCalled(
                this.token,
                tx,
                sender,
                sender,
                recipient,
                amount,
                data,
                null,
                postSenderBalance,
                postRecipientBalance
            );
        });

        it("TokensRecipient receives operatorSend data and is called after state mutation", async function () {
            const {tx} = await this.token.operatorSend(
                sender,
                recipient,
                amount,
                data,
                operatorData,
                {from: operator}
            );

            const postSenderBalance = await this.token.balanceOf(sender);
            const postRecipientBalance = await this.token.balanceOf(recipient);

            await _assertTokensReceivedCalled(
                this.token,
                tx,
                operator,
                sender,
                recipient,
                amount,
                data,
                operatorData,
                postSenderBalance,
                postRecipientBalance
            );
        });

        it("TokensRecipient receives mint (internal) data and is called after state mutation", async function () {
            const {tx} = await this.token.mintInternal(
                recipient,
                amount,
                data,
                operatorData,
                {from: operator}
            );

            const postRecipientBalance = await this.token.balanceOf(recipient);

            await _assertTokensReceivedCalled(
                this.token,
                tx,
                operator,
                ZERO_ADDRESS,
                recipient,
                amount,
                data,
                operatorData,
                new BN("0"),
                postRecipientBalance
            );
        });
    });
}

function shouldBehaveLikeERC777SendBurnWithSendHook(
    setupAccounts,
    amount,
    data,
    operatorData
) {
    let sender, recipient, operator;

    beforeEach(() => {
        ({sender, recipient, operator} = setupAccounts());
    });

    describe("when TokensSender reverts", function () {
        beforeEach(async function () {
            await this.tokensSenderImplementer.setShouldRevertSend(true);
        });

        it("send reverts", async function () {
            await expectRevertedWith(
                _sendFromHolder(this.token, sender, recipient, amount, data),
                "revert"
            );
        });

        it("operatorSend reverts", async function () {
            await expectRevertedWith(
                this.token.operatorSend(
                    sender,
                    recipient,
                    amount,
                    data,
                    operatorData,
                    {from: operator}
                ),
                "revert"
            );
        });

        it("burn reverts", async function () {
            await expectRevertedWith(
                _burnFromHolder(this.token, sender, amount, data),
                "revert"
            );
        });

        it("operatorBurn reverts", async function () {
            await expectRevertedWith(
                this.token.operatorBurn(sender, amount, data, operatorData, {
                    from: operator,
                }),
                "revert"
            );
        });
    });

    describe("when TokensSender does not revert", function () {
        beforeEach(async function () {
            await this.tokensSenderImplementer.setShouldRevertSend(false);
        });

        it("TokensSender receives send data and is called before state mutation", async function () {
            const preSenderBalance = await this.token.balanceOf(sender);
            const preRecipientBalance = await this.token.balanceOf(recipient);

            const {tx} = await _sendFromHolder(
                this.token,
                sender,
                recipient,
                amount,
                data
            );

            await _assertTokensToSendCalled(
                this.token,
                tx,
                sender,
                sender,
                recipient,
                amount,
                data,
                null,
                preSenderBalance,
                preRecipientBalance
            );
        });

        it("TokensSender receives operatorSend data and is called before state mutation", async function () {
            const preSenderBalance = await this.token.balanceOf(sender);
            const preRecipientBalance = await this.token.balanceOf(recipient);

            const {tx} = await this.token.operatorSend(
                sender,
                recipient,
                amount,
                data,
                operatorData,
                {from: operator}
            );

            await _assertTokensToSendCalled(
                this.token,
                tx,
                operator,
                sender,
                recipient,
                amount,
                data,
                operatorData,
                preSenderBalance,
                preRecipientBalance
            );
        });

        it("TokensSender receives burn data and is called before state mutation", async function () {
            const preSenderBalance = await this.token.balanceOf(sender);

            const {tx} = await _burnFromHolder(
                this.token,
                sender,
                amount,
                data,
                {from: sender}
            );

            await _assertTokensToSendCalled(
                this.token,
                tx,
                sender,
                sender,
                ZERO_ADDRESS,
                amount,
                data,
                null,
                preSenderBalance
            );
        });

        it("TokensSender receives operatorBurn data and is called before state mutation", async function () {
            const preSenderBalance = await this.token.balanceOf(sender);

            const {tx} = await this.token.operatorBurn(
                sender,
                amount,
                data,
                operatorData,
                {from: operator}
            );

            await _assertTokensToSendCalled(
                this.token,
                tx,
                operator,
                sender,
                ZERO_ADDRESS,
                amount,
                data,
                operatorData,
                preSenderBalance
            );
        });
    });
}

async function _removeBalance(token, holder) {
    await token.burn(await token.balanceOf(holder), "0x", {
        from: holder,
    });
    expect(await token.balanceOf(holder)).to.be.bignumber.equal("0");
}

async function _assertTokensReceivedCalled(
    token,
    txHash,
    operator,
    from,
    to,
    amount,
    data,
    operatorData,
    fromBalance,
    toBalance = "0"
) {
    await expectEvent.inTransaction(
        txHash,
        ERC777SenderRecipientMock,
        "TokensReceivedCalled",
        {
            operator,
            from,
            to,
            amount,
            data,
            operatorData,
            token: token.address,
            fromBalance,
            toBalance,
        }
    );
}

async function _assertTokensToSendCalled(
    token,
    txHash,
    operator,
    from,
    to,
    amount,
    data,
    operatorData,
    fromBalance,
    toBalance = "0"
) {
    await expectEvent.inTransaction(
        txHash,
        ERC777SenderRecipientMock,
        "TokensToSendCalled",
        {
            operator,
            from,
            to,
            amount,
            data,
            operatorData,
            token: token.address,
            fromBalance,
            toBalance,
        }
    );
}

async function _sendFromHolder(token, holder, to, amount, data) {
    if ((await web3.eth.getCode(holder)).length <= "0x".length) {
        return token.send(to, amount, data, {from: holder});
    } else {
        // assume holder is ERC777SenderRecipientMock contract
        return (await ERC777SenderRecipientMock.at(holder)).send(
            token.address,
            to,
            amount,
            data
        );
    }
}

async function _burnFromHolder(token, holder, amount, data) {
    if ((await web3.eth.getCode(holder)).length <= "0x".length) {
        return token.burn(amount, data, {from: holder});
    } else {
        // assume holder is ERC777SenderRecipientMock contract
        return (await ERC777SenderRecipientMock.at(holder)).burn(
            token.address,
            amount,
            data
        );
    }
}

module.exports = {
    shouldBehaveLikeERC777DirectSendBurn,
    shouldBehaveLikeERC777OperatorSendBurn,
    shouldBehaveLikeERC777UnauthorizedOperatorSendBurn,
    shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook,
    shouldBehaveLikeERC777SendBurnWithSendHook,
};
