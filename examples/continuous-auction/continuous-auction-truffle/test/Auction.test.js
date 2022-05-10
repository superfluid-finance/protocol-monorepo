const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const Auction = artifacts.require("Auction");

const traveler = require("ganache-time-traveler");
const TEST_TRAVEL_TIME = 3600 * 2; // 1 hours

contract("Auction functional", accounts => {
    const errorHandler = err => {
        if (err) throw err;
    };

    const ZERO_ADDRESS = "0x" + "0".repeat(40);
    accounts = accounts.slice(0, 7);
    const [admin, alice, bob, chris, dave, emma, frank] = accounts;
    const users = {};
    users[admin] = "Admin";
    users[alice] = "Alice";
    users[bob] = "Bob";
    users[chris] = "Chris";
    users[dave] = "Dave";
    users[emma] = "Emma";
    users[frank] = "Frank";
    users[ZERO_ADDRESS] = "0x0";

    let sf;
    let dai;
    let daix;
    let app;
    const markup = 110000;
    var minStep;

    before(async function() {
        await deployFramework(errorHandler, {
            web3,
            from: accounts[0]
        });
    });

    beforeEach(async function() {
        await deployTestToken(errorHandler, [":", "fDAI"], {
            web3,
            from: admin
        });
        await deploySuperToken(errorHandler, [":", "fDAI"], {
            web3,
            from: admin
        });

        sf = new SuperfluidSDK.Framework({
            web3,
            version: "test",
            tokens: ["fDAI"]
        });
        await sf.initialize();

        const daiAddress = await sf.tokens.fDAI.address;
        dai = await sf.contracts.TestToken.at(daiAddress);
        for (let i = 0; i < accounts.length; ++i) {
            await web3tx(dai.mint, `Account ${i} mints many dai`)(
                accounts[i],
                toWad(10000000),
                { from: accounts[i] }
            );
        }

        daix = sf.tokens.fDAIx;

        var testUnderlying = await daix.getUnderlyingToken();
        console.log("Under: ", testUnderlying, " DAIx: ", daix.address);

        app = await web3tx(Auction.new, "Deploy Auction")(
            sf.host.address,
            sf.agreements.cfa.address,
            daix.address,
            markup.toString() // STEP AMOUNT
        );
        console.log("minStep: ", (await app.minStep.call()).toString());

        users[app.address] = "App";
        for (let i = 0; i < accounts.length; ++i) {
            await dai.approve(daix.address, toWad(1000), {
                from: accounts[i]
            });
        }

        console.log("Giving the app one dollar so it doesn't break?");
        await checkBalance(app.address);

        await daix.upgrade((1 * 1e18).toString(), {
            from: bob
        });
        await daix.transfer(app.address, (1 * 1e18).toString(), { from: bob });
        await checkBalance(app.address);
    });

    async function checkBalance(who = alice) {
        console.log("Balance of ", users[who] || who);
        console.log("DAIx: ", (await daix.balanceOf(who)).toString());
    }

    async function checkBalances(accounts) {
        for (let i = 0; i < accounts.length; ++i) {
            await checkBalance(accounts[i]);
        }
    }

    async function sendDai(from, to, amount) {
        console.log(users[from] + "\t->\t", users[to] + "\tDAI/s:\t", amount);
        return await sf.host.callAgreement(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods
                .createFlow(
                    daix.address,
                    to.toString(),
                    amount.toString(),
                    "0x"
                )
                .encodeABI(),
            "0x", // user data
            {
                from: from.toString()
            }
        );
    }

    async function updateDai(from, to, amount) {
        console.log(users[from] + "\t->\t", users[to] + "\tDAI/s:\t", amount);
        return await sf.host.callAgreement(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods
                .updateFlow(
                    daix.address,
                    to.toString(),
                    amount.toString(),
                    "0x"
                )
                .encodeABI(),
            "0x", // user data
            {
                from: from.toString()
            }
        );
    }

    async function closeStream(from, to) {
        return await sf.host.callAgreement(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods
                .deleteFlow(daix.address, from, to, "0x")
                .encodeABI(),
            "0x", // user data
            {
                from
            }
        );
    }

    async function upgrade(accounts) {
        for (let i = 0; i < accounts.length; ++i) {
            await web3tx(
                daix.upgrade,
                `${users[accounts[i]]} upgrades many DAIx`
            )(toWad(100), { from: accounts[i] });
            await checkBalance(accounts[i]);
        }
    }

    async function logBids() {
        const winner = await app.winner.call();
        var bid,
            string = "\nspot:\tuser:\t\tbid:\t\tnetflow\t\tnext:\t\tprev:\n",
            //previous = "",
            bidder = winner,
            count = 0;
        minStep = Number((await app.minStep.call()).toString()) / 100000;
        console.log("minStep: ", minStep);
        do {
            bid = await app.bidders.call(bidder);
            string += `#${++count}\t${
                users[bidder.toString()]
            }\t\t${bid.flowRate.toString()}\t\t${await sf.agreements.cfa.getNetFlow.call(
                daix.address,
                bidder
            )}\t\t${users[bid.next.toString()]}\t\t${
                users[bid.prev.toString()]
            }\n`;
            //previous = bidder;
            bidder = bid.next;
        } while (bidder.substring(4, 7) !== "000");
        console.log(string);
    }

    async function send(who, amount) {
        console.log(
            users[who] + " sends a stream of " + amount + "DAI to the app"
        );
        minStep = Number((await app.minStep.call()).toString()) / 100000;
        if (amount < minStep) amount += Math.ceil(minStep);
        try {
            await sendDai(who, app.address, amount);
        } catch (e) {
            console.log("error oh no: ", e);
        }
        await logBids();
    }

    async function update(who, amount) {
        console.log(
            users[who] + " updates their stream to " + amount + "DAI to the app"
        );
        minStep = Number((await app.minStep.call()).toString()) / 100000;
        if (amount < minStep) amount += Math.ceil(minStep);
        try {
            await updateDai(who, app.address, amount);
        } catch (e) {
            console.log("error oh no: ", e);
        }
        await logBids();
    }

    async function close(who) {
        console.log(users[who] + " closes stream");
        try {
            await closeStream(who, app.address);
        } catch (e) {
            console.log("Oh no! Error closing: ", e);
        }
        await logBids();
    }

    async function flowExists(who) {
        var value =
            (
                await sf.agreements.cfa.getFlow.call(
                    daix.address,
                    who,
                    app.address
                )
            )[1].toString() !== "0";
        // console.log("flow of: " + users[who] + " exists? " + value);
        return value;
    }

    context("functional", () => {
        it("Case #1 - Alice joins auction, then leaves", async () => {
            minStep = 0;
            await upgrade([alice]);
            await checkBalance();
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            console.log("winner: ", users[await app.winner.call()]);

            await send(alice, 15);

            console.log("winner: ", users[await app.winner.call()]);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalance();

            close(alice);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalance();
        });

        it("Case #2 - Alice joins auction, then Bob joins", async () => {
            minStep = 0;
            accounts = [alice, bob, chris, dave, emma, frank];
            await upgrade(accounts);
            await checkBalances(accounts);
            await checkBalance(app.address);
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            await send(alice, 15);

            console.log("go forward in time");

            await send(bob, 40);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

            await close(alice);

            await send(alice, 41);

            await close(alice);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

            await send(alice, 15);

            await close(alice);

            await send(alice, 45);

            await close(alice);

            await close(bob);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalances(accounts);
            await checkBalance(app.address);
        });

        it("Case #3 - Everyone puts the same bid, then leaves", async () => {
            minStep = 0;
            accounts = [alice, bob, chris, dave, emma, frank];
            await upgrade(accounts);
            await checkBalances(accounts);
            await checkBalance(app.address);
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            for (let account of accounts) {
                await send(account, 100);
                console.log("go forward in time");
                await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            }

            for (let account of accounts) {
                await close(account);
            }
        });

        it("Case #4 - Everyone joins, then leaves randomly", async () => {
            minStep = 0;
            accounts = [alice, bob, chris, dave, emma, frank];
            await upgrade(accounts);
            await checkBalances(accounts);
            await checkBalance(app.address);
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

            for (let account of accounts) {
                await send(account, Math.floor(Math.random() * 100 + 1));
                console.log("go forward in time");
                await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            }
            var randomAccounts = accounts;
            for (var i = accounts.length; i > 0; i--) {
                var rand = Math.floor(Math.random() * (i - 1));
                await close(randomAccounts[rand]);
                randomAccounts.splice(rand, 1);
            }

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalances(accounts);
            await checkBalance(app.address);
        });

        it("Case #6 - updating streams", async () => {
            accounts = [alice, bob, chris, dave, emma, frank];
            minStep = 0;
            await upgrade(accounts);
            await checkBalances(accounts);
            await checkBalance(app.address);
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

            await send(alice, 10);
            await update(alice, 15);
            await send(bob, 10);
            await update(bob, 30);
            await update(bob, 20);
            await update(bob, 17);
            await update(bob, 16);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalances(accounts);
            await checkBalance(app.address);
        });
    });

    context.skip("Auction non-functional", accounts => {
        it("Case #7 - Everyone joins, then they randomly upgrade their bid", async () => {
            accounts = [alice, bob, chris, dave, emma, frank];
            minStep = 0;
            await upgrade(accounts);
            await checkBalances(accounts);
            await checkBalance(app.address);
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

            for (let account of accounts) {
                await send(account, Math.floor(Math.random() * 100 + 1));
                console.log("go forward in time");
                await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            }

            for (var i = 0; i < 30; i++) {
                var rand = Math.floor(Math.random() * accounts.length);
                await update(
                    accounts[rand],
                    Math.floor(Math.random() * 100 + 1)
                );
                console.log("go forward in time");
                await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            }

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalances(accounts);
            await checkBalance(app.address);
        });

        it("Case #8 - Fuzzy testing", async () => {
            accounts = [alice, bob, chris, dave, emma, frank];
            minStep = 0;
            await upgrade(accounts);
            await checkBalances(accounts);
            await checkBalance(app.address);
            const isApp = await sf.host.isApp(app.address);
            console.log("Is app: ", isApp);
            const isJailed = await sf.host.isAppJailed(app.address);
            console.log("Jail: ", isJailed);

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

            for (var i = 0; i < 100; i++) {
                var account =
                    accounts[Math.floor(Math.random() * accounts.length)];
                if (await flowExists(account)) {
                    if (i % 3)
                        await update(
                            account,
                            Math.floor(Math.random() * 100 + 1)
                        );
                    else await close(account);
                } else await send(account, Math.floor(Math.random() * 100 + 1));

                console.log("go forward in time");
                await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            }

            console.log("go forward in time");
            await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
            await checkBalances(accounts);
            await checkBalance(app.address);
        });
    });
});
