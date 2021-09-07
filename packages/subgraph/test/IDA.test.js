const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const chai = require("chai");
const fetch = require("node-fetch");
const chaiAsPromised = require("chai-as-promised");
chai.use(chaiAsPromised);

const INIT_BALANCE = toWad(100);

contract("IInstantDistributionAgreementV1", (accounts) => {
    const [aliceAddress, bobAddress, carolAddress] = accounts;

    let sf;
    let dai;
    let daix;
    let alice;
    let bob;
    let carol;

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

        alice = sf.user({ address: aliceAddress, token: daix.address });
        bob = sf.user({ address: bobAddress, token: daix.address });
        carol = sf.user({ address: carolAddress, token: daix.address });
    });

    describe("IDAs", () => {
        it("IDA with 1 member, not subscribed (showing unclaimed tokens)", async () => {
            await alice.createPool({ poolId: 1 });
            await alice.giveShares({ poolId: 1, recipient: bob, shares: 100 });

            await alice.distributeToPool({ poolId: 1, amount: 1000 });
            // let sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 1,
            //     subscriber: bobAddress,
            // });
            // let ind = await sf.ida.getIndex({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 1,
            // });
            fetch("http://localhost:8000/subgraphs/name/superfluid-test", {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify({
                    query: `
                    query Index{
                        indexes(where:{indexId:1}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                            }
                        }
                    }
                    `,
                }),
            })
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(
                        json.data.indexes[0]["activeSubscribers"].length,
                        1
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][0][
                            "totalPendingApproval"
                        ],
                        "1000"
                    );
                });
            // console.log(ind);
            // assert.equal(sub["pendingDistribution"], "1000");
            // assert.equal(sub["approved"], false);
        });
        it("IDA with 1 member, is subscribed", async () => {
            await alice.createPool({ poolId: 2 });
            await alice.giveShares({ poolId: 2, recipient: bob, shares: 100 });
            await sf.ida.approveSubscription({
                superToken: daix.address,
                indexId: 2,
                publisher: alice.address, // the publisher
                subscriber: bob.address, // who is receiving the units and sending this tx
            });
            // let sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 2,
            //     subscriber: bobAddress,
            // });
            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index{
                        indexes(where:{indexId:2}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                                approved
                            }
                        }
                    }
                    `,
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    // assert.equal(
                    //     json.data.indexes[0]["activeSubscribers"].length,
                    //     1
                    // );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][0]["approved"],
                        true
                    );
                    // console.log("done");
                });
            // assert.equal(sub["approved"], true);
        });
        it("IDA with multiple members, none subscribed (showing unclaimed tokens)", async () => {
            await alice.createPool({ poolId: 3 });
            await alice.giveShares({ poolId: 3, recipient: bob, shares: 50 });
            await alice.giveShares({ poolId: 3, recipient: carol, shares: 50 });
            await alice.distributeToPool({ poolId: 3, amount: 1000 });
            // let subs = await sf.ida.listSubcribers({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 3,
            // });
            // let ind = await sf.ida.getIndex({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 3,
            // });
            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index{
                        indexes(where:{indexId:3}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                                approved
                            }
                        }
                    }
                    `,
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(
                        json.data.indexes[0]["activeSubscribers"].length,
                        2
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][0][
                            "totalPendingApproval"
                        ],
                        "500"
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][1][
                            "totalPendingApproval"
                        ],
                        "500"
                    );
                });
            // console.log(ind);
            // assert.equal(ind["totalUnitsPending"], "100");
            // let sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 3,
            //     subscriber: bobAddress,
            // });
            // assert.equal(sub["approved"], false);
            // sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 3,
            //     subscriber: carolAddress,
            // });
            // assert.equal(sub["approved"], false);
            // assert.equal(subs.length, 2);
        });
        it("IDA with multiple members, partial subscribed  (showing unclaimed tokens)", async () => {
            await alice.createPool({ poolId: 4 });
            await alice.giveShares({ poolId: 4, recipient: bob, shares: 50 });
            await alice.giveShares({ poolId: 4, recipient: carol, shares: 50 });
            await sf.ida.approveSubscription({
                superToken: daix.address,
                indexId: 4,
                publisher: alice.address, // the publisher
                subscriber: bob.address, // who is receiving the units and sending this tx
            });
            await alice.distributeToPool({ poolId: 4, amount: 1000 });

            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index{
                        indexes(where:{indexId:4}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                                approved
                            }
                        }
                    }
                    `,
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(
                        json.data.indexes[0]["activeSubscribers"].length,
                        2
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][0]["approved"],
                        true
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][1][
                            "totalPendingApproval"
                        ],
                        "500"
                    );
                });

            // let sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 4,
            //     subscriber: bobAddress,
            // });
            // assert.equal(sub["approved"], true);
            // sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 4,
            //     subscriber: carolAddress,
            // });
            // assert.equal(sub["approved"], false);

            // let ind = await sf.ida.getIndex({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 4,
            // });
            // // console.log(ind);
            // assert.equal(ind["totalUnitsPending"], "50");
            // assert.equal(ind["totalUnitsApproved"], "50");
        });

        it("IDA with multiple members, all subscribed", async () => {
            await alice.createPool({ poolId: 5 });
            await alice.giveShares({ poolId: 5, recipient: bob, shares: 50 });
            await alice.giveShares({ poolId: 5, recipient: carol, shares: 50 });
            await sf.ida.approveSubscription({
                superToken: daix.address,
                indexId: 5,
                publisher: alice.address, // the publisher
                subscriber: bob.address, // who is receiving the units and sending this tx
            });
            await sf.ida.approveSubscription({
                superToken: daix.address,
                indexId: 5,
                publisher: alice.address, // the publisher
                subscriber: carol.address, // who is receiving the units and sending this tx
            });
            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index{
                        indexes(where:{indexId:5}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                                approved
                            }
                        }
                    }
                    `,
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(
                        json.data.indexes[0]["activeSubscribers"].length,
                        2
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][0]["approved"],
                        true
                    );
                    assert.equal(
                        json.data.indexes[0]["subscribers"][1]["approved"],
                        true
                    );
                });
            // let sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 5,
            //     subscriber: bobAddress,
            // });
            // assert.equal(sub["approved"], true);
            // sub = await sf.ida.getSubscription({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 5,
            //     subscriber: carolAddress,
            // });
            // assert.equal(sub["approved"], true);
        });

        it("IDA with multiple members, partial subscribe and a claim", async () => {
            await alice.createPool({ poolId: 6 });
            await alice.giveShares({ poolId: 6, recipient: bob, shares: 50 });
            await alice.giveShares({ poolId: 6, recipient: carol, shares: 50 });
            await sf.ida.approveSubscription({
                superToken: daix.address,
                indexId: 6,
                publisher: alice.address, // the publisher
                subscriber: bob.address, // who is receiving the units and sending this tx
            });
            await alice.distributeToPool({ poolId: 6, amount: 1000 });
            // let ind = await sf.ida.getIndex({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 6,
            // });
            // console.log(ind);
            // assert.equal(ind["totalUnitsPending"], "50");
            // assert.equal(ind["totalUnitsApproved"], "50");

            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index{
                        indexes(where:{indexId:6}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                                approved
                            }
                        }
                    }
                    `,
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(
                        json.data.indexes[0]["activeSubscribers"].length,
                        2
                    );
                    assert.equal(
                        json.data.indexes[0]["totalUnitsPending"],
                        "50"
                    );
                    assert.equal(
                        json.data.indexes[0]["totalUnitsApproved"],
                        "50"
                    );
                });
            // await sf.ida
            //     .getSubscription({
            //         superToken: daix.address,
            //         publisher: alice.address,
            //         indexId: 6,
            //         subscriber: carol.address,
            //     })
            //     .then((response) => {
            //         console.log("Subscription before claim: ");
            //         console.log(response);
            //     });
            // await sf.ida
            //     .getIndex({
            //         superToken: daix.address,
            //         publisher: alice.address,
            //         indexId: 6,
            //     })
            //     .then((response) => {
            //         console.log("Index before claim");
            //         console.log(response);
            //     });
            await sf.ida.claim({
                superToken: daix.address,
                publisher: alice.address,
                indexId: 6,
                subscriber: carol.address,
                sender: carol.address, // because ANYONE can send this tx
            });

            // await sf.ida
            //     .getSubscription({
            //         superToken: daix.address,
            //         publisher: alice.address,
            //         indexId: 6,
            //         subscriber: carol.address,
            //     })
            //     .then((response) => {
            //         console.log("Subscription after claim: ");
            //         console.log(response);
            //     });

            // await sf.ida
            //     .getIndex({
            //         superToken: daix.address,
            //         publisher: alice.address,
            //         indexId: 6,
            //     })
            //     .then((response) => {
            //         console.log("Index after claim");
            //         console.log(response);
            //     });

            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index{
                        indexes(where:{indexId:6}){
                            indexId
                            publisher
                            activeSubscribers
                            totalUnitsPending
                            totalDistribution
                            totalUnitsApproved
                            subscribers{
                                id
                                totalPendingApproval
                                approved
                                totalReceived
                            }
                        }
                    }
                    `,
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(
                        json.data.indexes[0]["activeSubscribers"].length,
                        2
                    );
                    // console.log(json.data.indexes[0]["subscribers"][0]);
                    // console.log(json.data.indexes[0]["subscribers"][1]);

                    assert.equal(
                        json.data.indexes[0]["totalUnitsApproved"],
                        "50"
                    );
                });
            // ind = await sf.ida.getIndex({
            //     superToken: daix.address,
            //     publisher: aliceAddress,
            //     indexId: 6,
            // });
            // console.log(ind);
            // assert.equal(ind["totalUnitsPending"], "0");
            // assert.equal(ind["totalUnitsApproved"], "100");
        });

        it("Members who are receiving multiple IDAs", async () => {
            await alice.createPool({ poolId: 7 });
            await alice.giveShares({ poolId: 7, recipient: bob, shares: 50 });
            await alice.giveShares({ poolId: 7, recipient: carol, shares: 50 });

            await alice.createPool({ poolId: 8 });
            await alice.giveShares({ poolId: 8, recipient: bob, shares: 50 });
            await alice.giveShares({ poolId: 8, recipient: carol, shares: 50 });
            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index($subscriber:Bytes){
                        subscribers(where:{subscriber:$subscriber}){
                            id
                            indexId
                        }
                    }
                    `,
                        variables: {
                            subscriber: bob.address,
                        },
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(json.data.subscribers.length > 1, true);
                });
            await fetch(
                "http://localhost:8000/subgraphs/name/superfluid-test",
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        query: `
                    query Index($subscriber:Bytes){
                        subscribers(where:{subscriber:$subscriber}){
                            id
                            indexId
                        }
                    }
                    `,
                        variables: {
                            subscriber: carol.address,
                        },
                    }),
                }
            )
                .then((response) => response.json())
                .then((json) => {
                    // console.log(json.data);
                    // console.log(json["body"]);
                    assert.equal(json.data.subscribers.length > 1, true);
                });
        });

        // //Negative tests
        it("Error messages for invalid arguments", async () => {});
    });
});
