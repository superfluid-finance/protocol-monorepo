// NOTE: copied and modified from https://github.com/OpenZeppelin/openzeppelin-contracts/
const {BN, constants, expectEvent} = require("@openzeppelin/test-helpers");
const {ZERO_ADDRESS} = constants;

const {expect} = require("chai");
const {ethers} = require("hardhat");
const {
    expectRevertedWith,
    expectCustomError,
} = require("../../utils/expectRevert");

const ERC777SenderRecipientMock = artifacts.require(
    "ERC777SenderRecipientMock"
);

async function getEthersContract(artifact, address) {
    return await ethers.getContractAt(artifact, address);
}

function shouldBehaveLikeERC777DirectSendBurn(setupAccounts, data) {
    _shouldBehaveLikeERC777DirectSend(setupAccounts, data);
    _shouldBehaveLikeERC777DirectBurn(setupAccounts, data);
}

function shouldBehaveLikeERC777OperatorSendBurn(
    setupAccounts,
    data,
    operatorData,
    isDefaultOperator
) {
    _shouldBehaveLikeERC777OperatorSend(
        setupAccounts,
        data,
        operatorData,
        isDefaultOperator
    );
    _shouldBehaveLikeERC777OperatorBurn(
        setupAccounts,
        data,
        operatorData,
        isDefaultOperator
    );
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
    let holderSigner;
    let tokenContract;

    before(async function () {
        ({holder, recipient} = setupAccounts());
        holderSigner = await ethers.getSigner(holder);
        tokenContract = await getEthersContract(
            "SuperTokenMock",
            this.token.address
        );
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
                await expectCustomError(
                    tokenContract
                        .connect(holderSigner)
                        .send(recipient, balance.addn(1).toString(), data),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_MOVE_INSUFFICIENT_BALANCE
                );
            });

            it("reverts when sending to the zero address", async function () {
                await expectCustomError(
                    tokenContract
                        .connect(holderSigner)
                        .send(ZERO_ADDRESS, new BN("1").toString(), data),
                    tokenContract,
                    "ZERO_ADDRESS",
                    this.testenv.customErrorCode
                        .SUPER_TOKEN_TRANSFER_TO_ZERO_ADDRESS
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
                await expectCustomError(
                    tokenContract
                        .connect(holderSigner)
                        .send(recipient, new BN("1").toString(), data),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_MOVE_INSUFFICIENT_BALANCE
                );
            });
        });
    });
}

function _shouldBehaveLikeERC777OperatorSend(
    setupAccounts,
    data,
    operatorData,
    isDefaultOperator
) {
    let holder, recipient, operator;
    let tokenContract;

    before(async function () {
        ({holder, recipient, operator} = setupAccounts());
        tokenContract = await getEthersContract(
            "SuperTokenMock",
            this.token.address
        );
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
                const operatorSigner = await ethers.getSigner(operator);
                const balance = await this.token.balanceOf(holder);
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorSend(
                            holder,
                            recipient,
                            balance.addn(1).toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_MOVE_INSUFFICIENT_BALANCE
                );
            });

            it("reverts when sending to the zero address", async function () {
                const operatorSigner = await ethers.getSigner(operator);
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorSend(
                            holder,
                            ZERO_ADDRESS,
                            new BN("1").toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    "ZERO_ADDRESS",
                    this.testenv.customErrorCode
                        .SUPER_TOKEN_TRANSFER_TO_ZERO_ADDRESS
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
                const operatorSigner = await ethers.getSigner(operator);
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorSend(
                            holder,
                            recipient,
                            new BN("1").toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_MOVE_INSUFFICIENT_BALANCE
                );
            });

            it("reverts when sending from the zero address", async function () {
                const operatorSigner = await ethers.getSigner(operator);
                // This is not yet reflected in the spec
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorSend(
                            ZERO_ADDRESS,
                            recipient,
                            new BN("0").toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    isDefaultOperator
                        ? "ZERO_ADDRESS"
                        : "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER",
                    isDefaultOperator
                        ? this.testenv.customErrorCode
                              .SUPER_TOKEN_TRANSFER_FROM_ZERO_ADDRESS
                        : undefined
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
            const operatorSigner = await ethers.getSigner(operator);
            const tokenContract = await getEthersContract(
                "SuperTokenMock",
                this.token.address
            );
            await expectCustomError(
                tokenContract
                    .connect(operatorSigner)
                    .operatorSend(
                        holder,
                        recipient,
                        new BN("0").toString(),
                        data,
                        operatorData
                    ),
                tokenContract,
                "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER"
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
                const holderSigner = await ethers.getSigner(holder);

                const tokenContract = await getEthersContract(
                    "SuperTokenMock",
                    this.token.address
                );
                await expectCustomError(
                    tokenContract
                        .connect(holderSigner)
                        .burn(balance.addn(1).toString(), data),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_BURN_INSUFFICIENT_BALANCE
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
                const holderSigner = await ethers.getSigner(holder);
                const tokenContract = await getEthersContract(
                    "SuperTokenMock",
                    this.token.address
                );
                await expectCustomError(
                    tokenContract
                        .connect(holderSigner)
                        .burn(new BN("1").toString(), data),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_BURN_INSUFFICIENT_BALANCE
                );
            });
        });
    });
}

function _shouldBehaveLikeERC777OperatorBurn(
    setupAccounts,
    data,
    operatorData,
    isDefaultOperator
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
                const operatorSigner = await ethers.getSigner(operator);
                const tokenContract = await getEthersContract(
                    "SuperTokenMock",
                    this.token.address
                );
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorBurn(
                            holder,
                            balance.addn(1).toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_BURN_INSUFFICIENT_BALANCE
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
                const tokenContract = await getEthersContract(
                    "SuperTokenMock",
                    this.token.address
                );
                const operatorSigner = await ethers.getSigner(operator);
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorBurn(
                            holder,
                            new BN("1").toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    "INSUFFICIENT_BALANCE",
                    this.testenv.customErrorCode
                        .SF_TOKEN_BURN_INSUFFICIENT_BALANCE
                );
            });

            it("reverts when burning from the zero address", async function () {
                const tokenContract = await getEthersContract(
                    "SuperTokenMock",
                    this.token.address
                );
                const operatorSigner = await ethers.getSigner(operator);
                // This is not yet reflected in the spec
                await expectCustomError(
                    tokenContract
                        .connect(operatorSigner)
                        .operatorBurn(
                            ZERO_ADDRESS,
                            new BN("0").toString(),
                            data,
                            operatorData
                        ),
                    tokenContract,
                    isDefaultOperator
                        ? "ZERO_ADDRESS"
                        : "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER",
                    isDefaultOperator
                        ? this.testenv.customErrorCode
                              .SUPER_TOKEN_BURN_FROM_ZERO_ADDRESS
                        : undefined
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
            const tokenContract = await getEthersContract(
                "SuperTokenMock",
                this.token.address
            );
            const operatorSigner = await ethers.getSigner(operator);
            await expectCustomError(
                tokenContract
                    .connect(operatorSigner)
                    .operatorBurn(
                        holder,
                        new BN("0").toString(),
                        data,
                        operatorData
                    ),
                tokenContract,
                "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER"
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
    let tokenContract;

    before(async function () {
        tokenContract = await getEthersContract(
            "SuperTokenMock",
            this.token.address
        );
    });

    beforeEach(() => {
        ({operator, sender, recipient} = setupAccounts());
    });

    describe("when TokensRecipient reverts", function () {
        beforeEach(async function () {
            await this.tokensRecipientImplementer.setShouldRevertReceive(true);
        });

        it("send reverts", async function () {
            await expectRevertedWith(
                _sendFromHolder(
                    tokenContract,
                    sender,
                    recipient,
                    amount.toString(),
                    data,
                    tokenContract,
                    true
                ),
                "_shouldRevertReceive"
            );
        });

        it("operatorSend reverts", async function () {
            const operatorSigner = await ethers.getSigner(operator);
            await expectRevertedWith(
                tokenContract
                    .connect(operatorSigner)
                    .operatorSend(
                        sender,
                        recipient,
                        amount.toString(),
                        data,
                        operatorData
                    ),
                "_shouldRevertReceive"
            );
        });

        it("mint (internal) reverts", async function () {
            const operatorSigner = await ethers.getSigner(operator);
            await expectRevertedWith(
                tokenContract
                    .connect(operatorSigner)
                    .mintInternal(
                        recipient,
                        amount.toString(),
                        data,
                        operatorData
                    ),
                "_shouldRevertReceive"
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
    let tokenContract;

    beforeEach(() => {
        ({sender, recipient, operator} = setupAccounts());
    });

    describe("when TokensSender reverts", function () {
        before(async function () {
            tokenContract = await getEthersContract(
                "SuperTokenMock",
                this.token.address
            );
        });

        beforeEach(async function () {
            await this.tokensSenderImplementer.setShouldRevertSend(true);
        });

        it("send reverts", async function () {
            await expectRevertedWith(
                _sendFromHolder(
                    this.token,
                    sender,
                    recipient,
                    amount.toString(),
                    data,
                    tokenContract,
                    true
                ),
                "_shouldRevertSend"
            );
        });

        it("operatorSend reverts", async function () {
            const operatorSigner = await ethers.getSigner(operator);
            await expectRevertedWith(
                tokenContract
                    .connect(operatorSigner)
                    .operatorSend(
                        sender,
                        recipient,
                        amount.toString(),
                        data,
                        operatorData
                    ),
                "_shouldRevertSend"
            );
        });

        it("burn reverts", async function () {
            await expectRevertedWith(
                _burnFromHolder(
                    this.token,
                    sender,
                    amount.toString(),
                    data,
                    tokenContract,
                    true
                ),
                "_shouldRevertSend"
            );
        });

        it("operatorBurn reverts", async function () {
            const operatorSigner = await ethers.getSigner(operator);
            await expectRevertedWith(
                tokenContract
                    .connect(operatorSigner)
                    .operatorBurn(
                        sender,
                        amount.toString(),
                        data,
                        operatorData
                    ),
                "_shouldRevertSend"
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

async function _sendFromHolder(
    token,
    holder,
    to,
    amount,
    data,
    ethersToken,
    ethersERC777
) {
    if ((await web3.eth.getCode(holder)).length <= "0x".length) {
        if (ethersToken && ethersToken.from == null) {
            return ethersToken
                .connect(await ethers.getSigner(holder))
                .send(to, amount, data);
        } else {
            return token.send(to, amount, data, {from: holder});
        }
    } else {
        // assume holder is ERC777SenderRecipientMock contract
        if (ethersERC777) {
            return (
                await ethers.getContractAt("ERC777SenderRecipientMock", holder)
            ).send(token.address, to, amount, data);
        } else {
            return (await ERC777SenderRecipientMock.at(holder)).send(
                token.address,
                to,
                amount,
                data
            );
        }
    }
}

async function _burnFromHolder(
    token,
    holder,
    amount,
    data,
    ethersToken,
    ethersERC777
) {
    if ((await web3.eth.getCode(holder)).length <= "0x".length) {
        if (ethersToken && ethersToken.from == null) {
            return ethersToken
                .connect(await ethers.getSigner(holder))
                .burn(amount, data);
        } else {
            return token.burn(amount, data, {from: holder});
        }
    } else {
        // assume holder is ERC777SenderRecipientMock contract
        if (ethersERC777) {
            return (
                await ethers.getContractAt("ERC777SenderRecipientMock", holder)
            ).burn(token.address, amount, data);
        } else {
            return (await ERC777SenderRecipientMock.at(holder)).burn(
                token.address,
                amount,
                data
            );
        }
    }
}

module.exports = {
    shouldBehaveLikeERC777DirectSendBurn,
    shouldBehaveLikeERC777OperatorSendBurn,
    shouldBehaveLikeERC777UnauthorizedOperatorSendBurn,
    shouldBehaveLikeERC777SendBurnMintInternalWithReceiveHook,
    shouldBehaveLikeERC777SendBurnWithSendHook,
};
