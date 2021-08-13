const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const chai = require("chai");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);

const INIT_BALANCE = toWad(100);

contract("ConstantFlowAgreementV1", (accounts) => {
    const [aliceAddress, bobAddress, carolAddress] = accounts;

    let sf;
    let dai;
    let daix;
    // let alice;
    // let bob;
    // let carol;

    before(async function () {
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
                await web3tx(dai.approve, `Account ${i} approves daix`)(
                    daix.address,
                    toWad(100),
                    { from: account }
                );
            })
        );

        await daix.upgrade(INIT_BALANCE, { from: aliceAddress });
        await daix.upgrade(INIT_BALANCE, { from: bobAddress });
        await daix.upgrade(INIT_BALANCE, { from: carolAddress });

        // alice = sf.user({ address: aliceAddress, token: daix.address });
        // bob = sf.user({ address: bobAddress, token: daix.address });
        // carol = sf.user({ address: carolAddress, token: daix.address });
    });

    describe("downgrade", () => {
        it("downgrades tokens", async () => {
            await daix.downgrade(100, { from: aliceAddress });
        });
    });
});
