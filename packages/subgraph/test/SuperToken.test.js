const { web3tx, toBN, toWad } = require("@decentral.ee/web3-helpers");

const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const traveler = require("ganache-time-traveler");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);
const expect = chai.expect;

const INIT_BALANCE = toWad(100);
const TEST_TRAVEL_TIME = 3600 * 24; // 24 hours

const emptyIda = {
    ida: {
        subscriptions: [],
    },
};

contract("ConstantFlowAgreementV1", accounts => {
    const errorHandler = err => {
        if (err) throw err;
    };

    const [
        adminAddress,
        aliceAddress,
        bobAddress,
        carolAddress,
        danAddress,
    ] = accounts;

    let sf;
    let dai;
    let daix;
    let superToken;
    let alice;
    let bob;
    let carol;

    before(async function() {
        sf = new SuperfluidSDK.Framework({
            web3,
            version: "test",
            tokens: ["fDAI"],
        });
        await sf.initialize();

        const daiAddress = await sf.tokens.fDAI.address;
        dai = await sf.contracts.TestToken.at(daiAddress);
        daix = sf.tokens.fDAIx;
        await Promise.all(
            accounts.map(async (account, i) => {
                await web3tx(dai.mint, `Account ${i} mints many dai`)(
                    accounts[i],
                    INIT_BALANCE,
                    { from: account }
                );
                await web3tx(
                    dai.approve,
                    `Account ${i} approves daix`
                )(daix.address, toWad(100), { from: account });
            })
        );

        await daix.upgrade(INIT_BALANCE, { from: aliceAddress });
        await daix.upgrade(INIT_BALANCE, { from: bobAddress });
        await daix.upgrade(INIT_BALANCE, { from: carolAddress });

        alice = sf.user({ address: aliceAddress, token: daix.address });
        bob = sf.user({ address: bobAddress, token: daix.address });
        carol = sf.user({ address: carolAddress, token: daix.address });
    });

    describe("downgrade", () => {
        it("downgrades tokens", async () => {
            const tx = await daix.downgrade(100, { from: aliceAddress });
        });
    });
});
