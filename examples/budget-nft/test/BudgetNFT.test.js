const { web3tx, toWad, wad4human } = require("@decentral.ee/web3-helpers");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const BudgetNFT = artifacts.require("BudgetNFT");

const traveler = require("ganache-time-traveler");
const TEST_TRAVEL_TIME = 3600 * 2; // 1 hours

contract("BudgetNFT", (accounts) => {
    const errorHandler = (err) => {
        if (err) throw err;
    };

    const names = ["Admin", "Alice", "Bob", "Carol", "Dan", "Emma", "Frank"];
    accounts = accounts.slice(0, names.length);

    let sf;
    let dai;
    let daix;
    let app;
    const u = {}; // object with all users
    const aliases = {};

    before(async function () {
        //process.env.RESET_SUPERFLUID_FRAMEWORK = 1;
        await deployFramework(errorHandler, {
            web3,
            from: accounts[0],
            newTestResolver: true
        });

        await deployTestToken(errorHandler, [":", "fDAI"], {
            web3,
            from: accounts[0],
        });
        await deploySuperToken(errorHandler, [":", "fDAI"], {
            web3,
            from: accounts[0],
        });

        sf = new SuperfluidSDK.Framework({
            web3,
            version: "test",
            tokens: ["fDAI"],
        });

        await sf.initialize();
        daix = sf.tokens.fDAIx;
        dai = await sf.contracts.TestToken.at(await sf.tokens.fDAI.address);

        for (var i = 0; i < names.length; i++) {
            u[names[i].toLowerCase()] = sf.user({
                address: accounts[i],
                token: daix.address,
            });
            u[names[i].toLowerCase()].alias = names[i];
            aliases[u[names[i].toLowerCase()].address] = names[i];
        }
        for (const [, user] of Object.entries(u)) {
            if (user.alias === "App") return;
            await web3tx(dai.mint, `${user.alias} mints many dai`)(
                user.address,
                toWad(100000000000),
                {
                    from: user.address,
                }
            );
            await web3tx(dai.approve, `${user.alias} approves daix`)(
                daix.address,
                toWad(100000000000),
                {
                    from: user.address,
                }
            );
        }
        //u.zero = { address: ZERO_ADDRESS, alias: "0x0" };
        console.log(u.admin.address);
        console.log(sf.host.address);
        console.log(sf.agreements.cfa.address);
        console.log(daix.address);
        app = await BudgetNFT.new(
            "BudgetNFT",
            "BNFT",
            sf.host.address,
            sf.agreements.cfa.address,
            daix.address,
            { from: u.admin.address }
        );

        await upgrade([u.admin]);
        await checkBalances([u.admin]);
        // await logUsers();
        //admin sends flow into nft contract
        await u.admin.flow({ flowRate: toWad(1).toString(), recipient: app.address });
        console.log("go forward in time");
        await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
        // await logUsers();

        u.app = sf.user({ address: app.address, token: daix.address });
        u.app.alias = "App";
        await checkBalance(u.app);
    });

    beforeEach(async function () {
        for (const [, user] of Object.entries(u)) {
            if (user.alias === "App") return;
            await web3tx(dai.mint, `${user.alias} mints many dai`)(
                user.address,
                toWad(100000000000),
                {
                    from: user.address,
                }
            );
            await web3tx(dai.approve, `${user.alias} approves daix`)(
                daix.address,
                toWad(100000000000),
                {
                    from: user.address,
                }
            );
        }
    });

    async function checkBalance(user) {
        console.log("Balance of ", user.alias);
        console.log("DAIx: ", (await daix.balanceOf(user.address)).toString());
    }

    async function checkBalances(accounts) {
        for (let i = 0; i < accounts.length; ++i) {
            await checkBalance(accounts[i]);
        }
    }

    async function checkBalance(user) {
        console.log("Balance of ", user.alias);
        console.log("DAIx: ", (await daix.balanceOf(user.address)).toString());
    }

    async function checkBalances(accounts) {
        for (let i = 0; i < accounts.length; ++i) {
            await checkBalance(accounts[i]);
        }
    }

    async function upgrade(accounts) {
        for (let i = 0; i < accounts.length; ++i) {
            await web3tx(
                daix.upgrade,
                `${accounts[i].alias} upgrades many DAIx`
            )(toWad(100000), { from: accounts[i].address });
            await checkBalance(accounts[i]);
        }
    }

    async function logUsers() {
        let string = "user\t\ttokens\t\tnetflow\n";
        let p = 0;
        for (const [, user] of Object.entries(u)) {
            if (await hasFlows(user)) {
                p++;
                string += `${user.alias}\t\t${wad4human(
                    await daix.balanceOf(user.address)
                )}\t\t${wad4human((await user.details()).cfa.netFlow)}
            `;
            }
        }
        if (p == 0) return console.warn("no users with flows");
        console.log("User logs:");
        console.log(string);
    }

    async function hasFlows(user) {
        const { inFlows, outFlows } = (await user.details()).cfa.flows;
        return inFlows.length + outFlows.length > 0;
    }

    async function transferNFT(to) {
        const receiver = to.address || to;
        const owner = await checkOwner();
        console.log("got owner from checkOwner(): ", owner);
        console.log("receiver: ", receiver);
        if (receiver === owner) {
            console.log("user === owner");
            return false;
        }
        await app.transferFrom(owner, receiver, 1, { from: owner });
        console.log(
            "token transferred, new owner: ",
            receiver,
            " = ",
            aliases[receiver]
        );
        return true;
    }

    describe("issue NFT", async function () {
        it("Case #1 - NFT is issued to Alice", async () => {
            const { alice } = u;
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);

            //key action - NFT is issued to alice w flowrate
            await app.issueNFT(alice.address, toWad(0.001).toString(), { from: u.admin.address });

            const aliceFlow = (await alice.details()).cfa.netFlow;

            console.log(`Alice Flow Rate is now: ${aliceFlow}`);

            const appFlow = (await u.app.details()).cfa.netFlow;
            const adminFlow = (await u.admin.details()).cfa.netFlow;

            //make sure that alice receives correct flow rate
            assert.equal(
                aliceFlow,
                toWad(0.001),
                "Alice flow rate is inaccurate"
            );
            //make sure app has right flow rate
            assert.equal(
                Number(appFlow),
                (Number(adminFlow) * - 1) - Number(aliceFlow),
                "app net flow is incorrect"
            );

            //burn NFT created in this test
            await app.burnNFT(0, { from: u.admin.address });
        });

        it("Case #2 - NFT is edited", async () => {
            const { alice } = u;
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);

            //key action - NFT is issued to alice w flowrate
            await app.issueNFT(alice.address, toWad(0.001).toString(), { from: u.admin.address });

            //key action #2 = NFT flowRate is edited. first param here is tokenId, which is now 1
            await app.editNFT(1, toWad(0.002).toString(), { from: u.admin.address });


            const aliceFlow = (await alice.details()).cfa.netFlow;

            const appFlow = (await u.app.details()).cfa.netFlow;
            const adminFlow = (await u.admin.details()).cfa.netFlow;

            //burn NFT created in this test
            await app.burnNFT(1, { from: u.admin.address });
            const aliceFlowAfterBurned = (await alice.details()).cfa.netFlow;
            console.log("Alice flow rate after ID #1 is burned " + aliceFlowAfterBurned);
            //make sure that alice receives correct flow rate
            assert.equal(
                aliceFlow,
                toWad(0.002).toString(),
                "Alice flow rate is inaccurate"
            );
            //make sure app has right flow rate
            assert.equal(
                Number(appFlow),
                (Number(adminFlow) * - 1) - Number(aliceFlow),
                "app net flow is incorrect"
            );

        });
    });

    describe("burn NFT", async function () {
        it("Case #1 - NFT is issued to Alice, then burned", async () => {
            const { alice } = u;
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);

            //key action - NFT is issued to alice w flowrate
            await app.issueNFT(alice.address, toWad(0.001).toString(), { from: u.admin.address });

            //key action #2 - NFT is burned, which should turn off flow rate (this token id is number 2)
            await app.burnNFT(2, { from: u.admin.address });

            const aliceFlowAfterBurned = (await alice.details()).cfa.netFlow;
            console.log("Alice flow rate after ID #2 is burned " + aliceFlowAfterBurned);

            const aliceFlow = (await alice.details()).cfa.netFlow;

            const appFlow = (await u.app.details()).cfa.netFlow;
            const adminFlow = (await u.admin.details()).cfa.netFlow;

            //make sure that alice receives correct flow rate
            assert.equal(
                aliceFlow,
                0,
                "Alice flow rate is inaccurate, should be zero"
            );
            //make sure app has right flow rate
            assert.equal(
                Number(appFlow),
                (Number(adminFlow) * - 1),
                "app net flow is incorrect"
            );
        });
    })

    describe("split and merge NFTs", async function () {
        it("Case #1 - NFT is issued to Alice, then split", async () => {
            const { alice } = u;
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);
            await logUsers();

            //key action - NFT is issued to alice w flowrate
            await app.issueNFT(alice.address, toWad(0.001).toString(), { from: u.admin.address });

            //key action #2 - NFT is split, which should cut flow rate in half from each NFT. this token ID is number 3
            await app.splitStream(3, toWad(0.0005).toString(), { from: alice.address });

            const aliceFlow = (await alice.details()).cfa.netFlow;

            //make sure that alice receives correct flow rate
            assert.equal(
                aliceFlow,
                toWad(0.001),
                "Alice flow rate is inaccurate, should be the same at first"
            );

            //key action #3 - new NFT creating in split is burned, leaving only 1/2 of alice flow rate left
            //NFT being burned is Alice's 4th token
            await app.burnNFT(4, { from: u.admin.address });

            const aliceUpdatedFlow = (await alice.details()).cfa.netFlow;

            assert.equal(
                aliceUpdatedFlow,
                toWad(0.0005).toString(),
                "Alice flow rate is inaccurate, should be 1/2 original"
            );

            const appFlow = (await u.app.details()).cfa.netFlow;
            const adminFlow = (await u.admin.details()).cfa.netFlow;
            // make sure app has right flow rate
            assert.equal(
                Number(appFlow) + Number(toWad(0.0005)),
                (Number(adminFlow) * - 1),
                "app net flow is incorrect"
            );

            //burn NFT created in this test - #4 is already burned, so need to also burn 3
            await app.burnNFT(3, { from: u.admin.address });

        });

        it("Case #2 - NFT is issued to Alice, split, then merged again", async () => {
            const { alice } = u;
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);

            //key action - NFT is issued to alice w flowrate
            await app.issueNFT(alice.address, toWad(0.001).toString(), { from: u.admin.address });

            //key action #2 - NFT is split, which should cut flow rate in half from each NFT
            await app.splitStream(5, toWad(0.0005).toString(), { from: alice.address });

            const aliceFlow = (await alice.details()).cfa.netFlow;

            console.log(`Alice Flow Rate is now: ${aliceFlow}`);

            //make sure that alice receives correct flow rate
            assert.equal(
                aliceFlow,
                toWad(0.001),
                "Alice flow rate is inaccurate, should be the same at first"
            );

            //key action #3 - 2 new NFTs are merged, alice should still have 100% of flow rate
            //note: the newly split NFT from previous action in this case is now ID #6. it is also burned by this action
            await app.mergeStreams(5, 6, { from: alice.address });

            const aliceUpdatedFlow = (await alice.details()).cfa.netFlow;
            const adminFlow = (await u.admin.details()).cfa.netFlow;
            const appUpdatedFlow = (await u.app.details()).cfa.netFlow;

            assert.equal(
                aliceUpdatedFlow,
                toWad(0.001),
                "Alice flow rate is inaccurate, should be 100% of original"
            );
            // make sure app has right flow rate
            assert.equal(
                Number(appUpdatedFlow) + Number(toWad(0.001)),
                (Number(adminFlow) * - 1),
                "app net flow is incorrect"
            );

            //burn NFT created in this test
            await app.burnNFT(5, { from: u.admin.address });
        });
    })

});