const { toBN, toWad } = require("@decentral.ee/web3-helpers");
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");
const {
    getErrorResponse,
    getMissingArgumentError,
    getBatchCallHelpText,
} = require("@superfluid-finance/js-sdk/src/utils/error");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);
const expect = chai.expect;

contract("batchCall helper class", (accounts) => {
    const t = new TestEnvironment(accounts.slice(0, 4), { isTruffle: true });
    const {
        admin: adminAddress,
        alice: aliceAddress,
        bob: bobAddress,
        carol: carolAddress,
    } = t.aliases;

    let sf;
    let superToken;
    let alic;
    let bob;
    let carol;

    before(async () => {
        await t.reset();
        sf = t.sf;
    });

    beforeEach(async () => {
        await t.createNewToken({ doUpgrade: true });
        ({ superToken } = t.contracts);
        alice = sf.user({ address: aliceAddress, token: superToken.address });
        bob = sf.user({ address: bobAddress, token: superToken.address });
        carol = sf.user({ address: carolAddress, token: superToken.address });
    });

    const exampleERC20TransferFromData = {
        token: "0x111...",
        amount: "1000000000000000000",
        sender: "0xaaa...",
        recipient: "0xbbb...",
    };

    describe("Generic bad-case", () => {
        it("Data not provided", async () => {
            await expect(sf.batchCall({ type: 1 })).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("data", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("Type not provided", async () => {
            await expect(
                sf.batchCall({ data: exampleERC20TransferFromData })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("type", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("Invalid operation", async () => {
            const NO_OP_STRING = "NO_OP";
            const NO_OP_NUMBER = 9999;
            await expect(
                sf.batchCall({
                    type: NO_OP_NUMBER,
                    data: exampleERC20TransferFromData,
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    `You provided an invalid operation type "${NO_OP_NUMBER}"${getBatchCallHelpText(
                        0
                    )}`,
                    "batchCall"
                )
            );
            await expect(
                sf.batchCall({
                    type: NO_OP_STRING,
                    data: exampleERC20TransferFromData,
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    `You provided an invalid operation type "${NO_OP_STRING}"${getBatchCallHelpText(
                        0
                    )}`,
                    "batchCall"
                )
            );
        });
    });
    describe("ERC20 bad-case", () => {
        it("Amount not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "ERC20_APPROVE",
                    data: {
                        token: "0x",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("amount", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("Token not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "ERC20_APPROVE",
                    data: {
                        amount: "1",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("token", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("ERC20_APPROVE spender not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "ERC20_APPROVE",
                    data: {
                        token: "0x",
                        amount: "1",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("spender", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("ERC20_TRANSFER_FROM sender not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "ERC20_TRANSFER_FROM",
                    data: {
                        token: "0x",
                        amount: "1",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("sender", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
    });
    describe("Super Token bad-case", () => {
        it("Amount not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        token: "0x",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("amount", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("Token not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        amount: "1",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("token", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
    });
    describe("Call Agreement bad-case", () => {
        it("agreementType not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        method: "createFlow",
                        arguments: [
                            "0x", // Token address
                            "0x", // Flow recipient
                            "1", // Flow rate
                            "0x",
                        ],
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError(
                        "agreementType",
                        getBatchCallHelpText(0)
                    ),
                    "batchCall"
                )
            );
        });
        it("Invalid agreementType", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
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
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    "You provided an invalid agreementType" +
                        getBatchCallHelpText(0),
                    "batchCall"
                )
            );
        });
        it("method not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        agreementType: "CFA",
                        arguments: [
                            "0x", // Token address
                            "0x", // Flow recipient
                            "1", // Flow rate
                            "0x",
                        ],
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError("method", getBatchCallHelpText(0)),
                    "batchCall"
                )
            );
        });
        it("arguments not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        agreementType: "CFA",
                        method: "createFlow",
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError(
                        "arguments",
                        getBatchCallHelpText(0)
                    ),
                    "batchCall"
                )
            );
        });
        it("arguments not provided", async () => {
            await expect(
                sf.batchCall({
                    type: "SUPERTOKEN_UPGRADE",
                    data: {
                        agreementType: "CFA",
                        method: "createFlow",
                        arguments: [],
                    },
                })
            ).to.be.rejectedWith(
                getErrorResponse(
                    getMissingArgumentError(
                        "arguments",
                        getBatchCallHelpText(0)
                    ),
                    "batchCall"
                )
            );
        });
    });
});
