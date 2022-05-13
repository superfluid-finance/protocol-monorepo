const { web3tx, toWad, wad4human } = require("@decentral.ee/web3-helpers");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const TradeableCashflow = artifacts.require("TradeableCashflow");

const traveler = require("ganache-time-traveler");
const TEST_TRAVEL_TIME = 3600 * 2; // 1 hours

contract("TradeableCashflow", (accounts) => {
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
        });
    });

    beforeEach(async function () {
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
                toWad(100000000),
                {
                    from: user.address,
                }
            );
            await web3tx(dai.approve, `${user.alias} approves daix`)(
                daix.address,
                toWad(100000000),
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
        app = await TradeableCashflow.new(
            u.admin.address,
            "TradeableCashflow",
            "TCF",
            sf.host.address,
            sf.agreements.cfa.address,
            daix.address
        );

        u.app = sf.user({ address: app.address, token: daix.address });
        u.app.alias = "App";
        await checkBalance(u.app);
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

    async function upgrade(accounts) {
        for (let i = 0; i < accounts.length; ++i) {
            await web3tx(
                daix.upgrade,
                `${accounts[i].alias} upgrades many DAIx`
            )(toWad(100000000), { from: accounts[i].address });
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

    async function appStatus() {
        const isApp = await sf.host.isApp(u.app.address);
        const isJailed = await sf.host.isAppJailed(app.address);
        !isApp && console.error("App is not an App");
        isJailed && console.error("app is Jailed");
        await checkBalance(u.app);
        await checkOwner();
    }
    async function checkOwner() {
        const owner = await app.ownerOf("1");
        console.log("Contract Owner: ", aliases[owner], " = ", owner);
        return owner.toString();
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
    describe("sending flows", async function () {
        it("Case #1 - Alice sends a flow", async () => {
            const { alice } = u;
            const appInitialBalance = await daix.balanceOf(app.address);
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);
            await appStatus();
            await logUsers();
            await alice.flow({ flowRate: toWad(0.001).toString(), recipient: u.app });
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();

            alice.flow({ recipient: u.app, flowRate: "0" });

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            const appFinalBalance = await daix.balanceOf(app.address);
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            assert.equal(
                appInitialBalance.toString(),
                appFinalBalance.toString(),
                "balances aren't equal"
            );
        });
        it("Case #2 - Alice upates flows to the contract", async () => {
            const { alice } = u;
            const appInitialBalance = await daix.balanceOf(app.address);
            await upgrade([alice]);
            await checkBalances([alice, u.admin]);
            await appStatus();
            await logUsers();
            await alice.flow({ flowRate: toWad(0.00001).toString(), recipient: u.app });
            assert.equal(
                ((await alice.details()).cfa.netFlow * -1).toString(),
                (await u.admin.details()).cfa.netFlow.toString(),
                "netflows aren't equal"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            await alice.flow({ flowRate: toWad(0.00002).toString(), recipient: u.app });
            assert.equal(
                ((await alice.details()).cfa.netFlow * -1).toString(),
                (await u.admin.details()).cfa.netFlow.toString(),
                "netflows aren't equal"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();

            await alice.flow({ recipient: u.app, flowRate: "0" });
            assert.equal(
                ((await alice.details()).cfa.netFlow * -1).toString(),
                (await u.admin.details()).cfa.netFlow.toString(),
                "netflows aren't equal"
            );

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            const appFinalBalance = await daix.balanceOf(app.address);
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            assert.equal(
                appInitialBalance.toString(),
                appFinalBalance.toString(),
                "balances aren't equal"
            );
        });

        it("Case #3 - Multiple users send flows to the contract", async () => {
            const { alice, bob, carol, dan, admin } = u;
            const appInitialBalance = await daix.balanceOf(app.address);
            await upgrade([alice, bob, carol, dan]);
            await checkBalances([alice, bob, carol, dan, admin]);
            await appStatus();
            await logUsers();
            await alice.flow({ flowRate: toWad(0.00001).toString(), recipient: u.app });
            await bob.flow({ flowRate: toWad(0.000015).toString(), recipient: u.app });
            await carol.flow({ flowRate: toWad(0.000021).toString(), recipient: u.app });
            await dan.flow({ flowRate: toWad(0.000051).toString(), recipient: u.app });
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            await alice.flow({ flowRate: toWad(0.002).toString(), recipient: u.app });
            await bob.flow({ flowRate: toWad(0.000115).toString(), recipient: u.app });
            await carol.flow({ flowRate: toWad(0.00121).toString(), recipient: u.app });
            await dan.flow({ flowRate: toWad(0.000151).toString(), recipient: u.app });
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            const appFinalBalance = await daix.balanceOf(app.address);
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            assert.equal(
                appInitialBalance.toString(),
                appFinalBalance.toString(),
                "balances aren't equal"
            );
        });

        // to be fixed
        it.skip("Case #4 - Owner deletes the flow", async () => {
            const { alice, admin } = u;
            await upgrade([alice]);
            await logUsers();
            await alice.flow({ flowRate: toWad(0.001).toString(), recipient: u.app });
            assert.equal(
                await sf.cfa.getNetFlow({
                    superToken: daix.address,
                    account: u.app.address,
                }),
                "0"
            );
            assert.equal(
                await sf.cfa.getNetFlow({
                    superToken: daix.address,
                    account: admin.address,
                }),
                toWad(0.001).toString()
            );
            await sf.cfa.deleteFlow({
                superToken: daix.address,
                sender: u.app.address,
                receiver: admin.address,
                by: admin.address,
            });
            assert.isFalse(await sf.host.isAppJailed.call(u.app.address));
            assert.equal(
                await sf.cfa.getNetFlow({
                    superToken: daix.address,
                    account: admin.address,
                }),
                "0"
            );
            assert.equal(
                await sf.cfa.getNetFlow({
                    superToken: daix.address,
                    account: u.app.address,
                }),
                "0"
            );
        });
    });

    describe("Changing owner", async function () {
        it("Case #4 - When the owner changes, the flow changes", async () => {
            const { alice, bob, carol, dan, admin } = u;
            const appInitialBalance = await daix.balanceOf(app.address);
            await upgrade([alice, bob, carol]);
            await checkBalances([alice, bob, carol, dan, admin]);
            await appStatus();
            (await transferNFT(dan)) || (await transferNFT(admin));
            await logUsers();
            await alice.flow({ flowRate: toWad(0.00001).toString(), recipient: u.app });
            await bob.flow({ flowRate: toWad(0.000015).toString(), recipient: u.app });
            await carol.flow({ flowRate: toWad(0.000021).toString(), recipient: u.app });
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            // change owners
            (await transferNFT(admin)) || (await transferNFT(dan));
            await dan.flow({ flowRate: "0", recipient: u.app }); //close the flows to the app if there are any
            assert.equal(
                (await u.dan.details()).cfa.netFlow,
                0,
                "Dan's flowRate should be zero"
            );
            await app.transferFrom(u.admin.address, u.dan.address, 1);
            assert.equal(
                (await u.admin.details()).cfa.netFlow,
                0,
                "Admin's flowRate not zero"
            );
            await appStatus();
            await logUsers();
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            const appFinalBalance = await daix.balanceOf(app.address);
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            assert.equal(
                appInitialBalance.toString(),
                appFinalBalance.toString(),
                "balances aren't equal"
            );
        });
    });

    describe("Sending and receiving at the same time", async function () {
        it("Case #5 - Dan is a contributor, then becomes the owner too", async () => {
            const { alice, bob, carol, dan, admin } = u;
            const appInitialBalance = await daix.balanceOf(app.address);
            await transferNFT(admin);
            await upgrade([alice, bob, carol, dan]);
            await checkBalances([alice, bob, carol, dan, admin]);
            await appStatus();
            await logUsers();
            await alice.flow({ flowRate: toWad(0.00001).toString(), recipient: u.app });
            await bob.flow({ flowRate: toWad(0.000015).toString(), recipient: u.app });
            await carol.flow({ flowRate: toWad(0.000021).toString(), recipient: u.app });
            await dan.flow({ flowRate: toWad(0.000021).toString(), recipient: u.app });
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            // change owners
            await transferNFT(dan);
            assert.equal(
                (await u.admin.details()).cfa.netFlow.toString(),
                0,
                "Admin's flowRate not zero"
            );
            assert.equal(
                (
                    (await u.app.details()).cfa.flows.outFlows[0].flowRate -
                    ((await u.dan.details()).cfa.flows.outFlows[0].flowRate ||
                        0)
                ).toString(),
                (await u.dan.details()).cfa.netFlow.toString(),
                "Dan's flowRate doesn't add up"
            );
            await appStatus();
            await logUsers();
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await appStatus();
            await logUsers();
            const appFinalBalance = await daix.balanceOf(app.address);
            assert.equal(
                (await u.app.details()).cfa.netFlow,
                0,
                "App flowRate not zero"
            );
            assert.equal(
                appInitialBalance.toString(),
                appFinalBalance.toString(),
                "balances aren't equal"
            );
        });
    });

    describe.skip("Fuzzy testing", async function () {
        it("Case #6 - Random testing", async () => {
            const { alice, bob, carol, dan, emma, frank, admin } = u;
            const accounts = [alice, bob, carol, dan, emma, frank, admin];
            const appInitialBalance = await daix.balanceOf(app.address);
            await upgrade([admin, alice, bob, carol, dan, emma, frank]);
            await checkBalances([alice, bob, carol, dan, emma, frank, admin]);
            await appStatus();
            await logUsers();
            for (var i = 0; i < 5; i++) {
                for (var user of accounts) {
                    var seed = Math.floor(Math.random() * 3);
                    const defaultAction = user.flow({
                        recipient: u.app,
                        flowRate: toWad(
                            Math.round(Math.random() * 1000) / 1000000 + 0.001
                        ).toString(),
                    });
                    switch (seed) {
                        case 1:
                            (await user.details()).cfa.flows.outFlows.length > 0
                                ? await user.flow({
                                      recipient: u.app,
                                      flowRate: "0",
                                  })
                                : await user.flow({
                                      recipient: u.app,
                                      flowRate: toWad(
                                          Math.round(Math.random() * 1000) /
                                              1000000 +
                                              0.001
                                      ).toString(),
                                  });
                            break;
                        case 2:
                            if (await transferNFT(user.address)) break;
                            else await defaultAction();
                            break;
                        default:
                            await defaultAction();
                    }
                    console.log("go forward in time");
                    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
                    await appStatus();
                    await logUsers();
                    assert.equal(
                        (await u.app.details()).cfa.netFlow,
                        0,
                        "App flowRate not zero"
                    );
                    assert.equal(
                        appInitialBalance.toString(),
                        (await daix.balanceOf(app.address)).toString(),
                        "App balance has changed"
                    );
                }
            }
        }).timeout(10000000);
    });
});

// Check if the owner can be a payer at the same time
