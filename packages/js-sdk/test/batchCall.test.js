const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");

const {
    getErrorResponse,
    getMissingArgumentError,
    getBatchCallHelpText,
} = require("../src/utils/error");

const {batchCall} = require("../src/batchCall");

describe("batchCall helper class", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let adminAddress, aliceAddress;
    let sf;
    let testToken;
    let superToken;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: false,
            web3,
            nAccounts: 2,
        });

        ({admin: adminAddress, alice: aliceAddress} = t.aliases);
        testToken = t.sf.tokens.TEST;
        superToken = t.sf.tokens.TESTx;

        sf = t.sf;
        sf.batchCall = (calls) => {
            console.log(batchCall({agreements: sf.agreements, calls: calls}));
            return sf.host.batchCall(
                batchCall({agreements: sf.agreements, calls: calls})
            );
        };
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    const exampleERC20TransferFromData = {
        token: "0x111...",
        amount: "1000000000000000000",
        sender: "0xaaa...",
        recipient: "0xbbb...",
    };

    describe("Generic bad-case", () => {
        it("Calls not provided", async () => {
            try {
                await sf.batchCall({});
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        "You must provide an array of calls",
                        "batchCall"
                    )
                );
            }
        });

        it("Data not provided", async () => {
            try {
                await sf.batchCall([{type: 1}]);
            } catch (err) {
                assert.equal(
                    err.message,
                    // eslint-disable-next-line
                    'Error: @superfluid-finance/js-sdk batchCall: You did not provide a required argument for "data"  in item #0 in your batch call array. Please see https://docs.superfluid.finance/superfluid/docs/batch-call for more help'
                );
            }
        });

        it("Type not provided", async () => {
            try {
                await sf.batchCall([{data: exampleERC20TransferFromData}]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "type",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });

        it("Invalid operation", async () => {
            const NO_OP_STRING = "NO_OP";
            const NO_OP_NUMBER = 10000;
            try {
                await sf.batchCall([
                    {
                        type: NO_OP_NUMBER,
                        data: exampleERC20TransferFromData,
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        `You provided an invalid operation type "${NO_OP_NUMBER}"${getBatchCallHelpText(
                            0
                        )}`,
                        "batchCall"
                    )
                );
            }

            try {
                await sf.batchCall([
                    {
                        type: NO_OP_STRING,
                        data: exampleERC20TransferFromData,
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        `You provided an invalid operation type "${NO_OP_STRING}"${getBatchCallHelpText(
                            0
                        )}`,
                        "batchCall"
                    )
                );
            }
        });
    });
    describe("ERC20 bad-case", () => {
        it("amount not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "ERC20_APPROVE",
                        data: {
                            token: "0x",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "amount",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
        it("token not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "ERC20_APPROVE",
                        data: {
                            amount: "1",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "token",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
        it("ERC20_APPROVE spender not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "ERC20_APPROVE",
                        data: {
                            token: "0x",
                            amount: "1",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "spender",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });

        it("ERC20_TRANSFER_FROM sender not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "ERC20_TRANSFER_FROM",
                        data: {
                            token: "0x",
                            amount: "1",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "sender",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
    });
    describe("Super Token bad-case", () => {
        it("amount not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "SUPERTOKEN_UPGRADE",
                        data: {
                            token: "0x",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "amount",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });

        it("token not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "SUPERTOKEN_UPGRADE",
                        data: {
                            amount: "1",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "token",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
    });
    describe("Call Agreement bad-case", () => {
        it("agreementType not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "SUPERFLUID_CALL_AGREEMENT",
                        data: {
                            method: "createFlow",
                            arguments: [
                                "0x", // Token address
                                "0x", // Flow recipient
                                "1", // Flow rate
                                "0x",
                            ],
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "agreementType",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });

        it("Invalid agreementType", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "SUPERFLUID_CALL_AGREEMENT",
                        data: {
                            agreementType: "NO_OP",
                            method: "createFlow",
                            arguments: [
                                "0x", // Token address
                                "0x", // Flow recipient
                                "1", // Flow rate
                                "0x",
                            ],
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        "You provided an invalid agreementType" +
                            getBatchCallHelpText(0),
                        "batchCall"
                    )
                );
            }
        });

        it("method not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "SUPERFLUID_CALL_AGREEMENT",
                        data: {
                            agreementType: "CFA",
                            arguments: [
                                "0x", // Token address
                                "0x", // Flow recipient
                                "1", // Flow rate
                                "0x",
                            ],
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "method",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
        it("arguments not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "SUPERFLUID_CALL_AGREEMENT",
                        data: {
                            agreementType: "CFA",
                            method: "createFlow",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "arguments",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
    });
    describe("Super App bad-case", () => {
        it("superApp not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "CALL_APP_ACTION",
                        data: {
                            callData: "0x",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "superApp",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });

        it("callData not provided", async () => {
            try {
                await sf.batchCall([
                    {
                        type: "CALL_APP_ACTION",
                        data: {
                            superApp: "CFA",
                        },
                    },
                ]);
            } catch (err) {
                assert.equal(
                    err.message,
                    getErrorResponse(
                        getMissingArgumentError(
                            "callData",
                            getBatchCallHelpText(0)
                        ),
                        "batchCall"
                    )
                );
            }
        });
    });

    describe("Tokens Operations", () => {
        it("Upgrade and Approve", async () => {
            await testToken.approve(superToken.address, "1000000000000000000");
            await sf.batchCall([
                {
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        token: superToken.address,
                        amount: "1000000000000000000",
                    },
                },
                {
                    type: "ERC20_APPROVE",
                    data: {
                        token: superToken.address,
                        spender: aliceAddress,
                        amount: "1000000000000000000",
                    },
                },
            ]);
            await superToken.transferFrom(
                adminAddress,
                aliceAddress,
                "1000000000000000000",
                {from: aliceAddress}
            );
            assert.equal(
                (await superToken.balanceOf(aliceAddress)).toString(),
                "1000000000000000000",
                "no right balance amount"
            );
        });

        it("Get funds and start stream", async () => {
            await sf.batchCall([
                {
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        token: superToken.address,
                        amount: "1000000000000000000",
                    },
                },
                {
                    type: "SUPERFLUID_CALL_AGREEMENT",
                    data: {
                        agreementType: "CFA",
                        method: "createFlow",
                        arguments: [
                            superToken.address,
                            aliceAddress,
                            "1000000000",
                            "0x",
                        ],
                    },
                },
            ]);
        });
    });
});
