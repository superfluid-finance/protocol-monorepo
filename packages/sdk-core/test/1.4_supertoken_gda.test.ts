import { expect } from "chai";
import { TestEnvironment, makeSuite } from "./TestEnvironment";
import SuperfluidPool from "../src/SuperfluidPool";
import { Signer, ethers } from "ethers";
import { WrapperSuperToken, toBN } from "../src";

const createPoolAndReturnPoolClass = async (
    superToken: WrapperSuperToken,
    signer: Signer,
    admin: string
) => {
    const data = await superToken.createPool({
        admin,
        signer,
    });
    return new SuperfluidPool(data.poolAddress);
};

makeSuite(
    "SuperToken-GDA and SuperfluidPool Tests",
    (testEnv: TestEnvironment) => {
        describe("Revert cases", () => {
            it("Should throw an error on GDA view functions when wrong params passed", async () => {
                try {
                    await testEnv.wrapperSuperToken.getGDANetFlow({
                        account: "",
                        providerOrSigner: testEnv.alice,
                    });
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting the GDA net flow."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.getFlowRate({
                        from: "",
                        pool: "",
                        providerOrSigner: testEnv.alice,
                    });
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting the GDA flow rate."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.estimateFlowDistributionActualFlowRate(
                        {
                            from: "",
                            pool: "",
                            requestedFlowRate: "",
                            providerOrSigner: testEnv.alice,
                        }
                    );
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error estimating the GDA flow distribution's actual flow rate."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.estimateDistributionActualAmount(
                        {
                            from: "",
                            pool: "",
                            requestedAmount: "",
                            providerOrSigner: testEnv.alice,
                        }
                    );
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error estimating the GDA distribution's actual amount."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.getPoolAdjustmentFlowRate({
                        pool: "",
                        providerOrSigner: testEnv.alice,
                    });
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting the GDA pool adjustment flow rate."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.isPool({
                        account: "",
                        providerOrSigner: testEnv.alice,
                    });
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error checking if the account is a pool."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.isMemberConnected({
                        pool: "",
                        member: "",
                        providerOrSigner: testEnv.alice,
                    });
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error checking if the member is connected to the pool."
                    );
                }

                try {
                    await testEnv.wrapperSuperToken.getPoolAdjustmentFlowInfo({
                        pool: "",
                        providerOrSigner: testEnv.alice,
                    });
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting the GDA pool adjustment flow information."
                    );
                }
            });

            it("Should throw an error on SuperfluidPool view functions when wrong params passed", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );

                try {
                    await pool.getPoolAdmin("" as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting the pool admin."
                    );
                }

                try {
                    await pool.getSuperToken("" as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting the pool's SuperToken."
                    );
                }

                try {
                    await pool.getTotalUnits("" as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting total units."
                    );
                }

                try {
                    await pool.getTotalConnectedUnits("" as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting total connected units."
                    );
                }

                try {
                    await pool.getUnits({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting units."
                    );
                }

                try {
                    await pool.getTotalConnectedFlowRate("" as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting total connected flow rate."
                    );
                }

                try {
                    await pool.getTotalDisconnectedFlowRate("" as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting total disconnected flow rate."
                    );
                }

                try {
                    await pool.getDisconnectedBalance({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting disconnected balance."
                    );
                }

                try {
                    await pool.getMemberFlowRate({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting member flow rate."
                    );
                }

                try {
                    await pool.getClaimable({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting claimable amount."
                    );
                }

                try {
                    await pool.getClaimableNow({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting claimable amount."
                    );
                }

                try {
                    await pool.totalSupply({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting total supply."
                    );
                }

                try {
                    await pool.balanceOf({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting balance."
                    );
                }

                try {
                    await pool.allowance({} as any);
                } catch (err) {
                    expect(err.type).to.equal("SUPERFLUID_POOL_READ");
                    expect(err.message).to.have.string(
                        "There was an error getting allowance."
                    );
                }
            });
        });

        describe("Happy Path Tests", () => {
            it("Should be able to create pool", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );

                expect(
                    await testEnv.wrapperSuperToken.isPool({
                        account: pool.contract.address,
                        providerOrSigner: testEnv.alice,
                    })
                ).to.be.true;

                expect(await pool.getPoolAdmin(testEnv.alice)).to.be.equal(
                    testEnv.alice.address
                );
                expect(await pool.getSuperToken(testEnv.alice)).to.be.equal(
                    testEnv.wrapperSuperToken.address
                );
            });

            it("Should be able to assign units to pool", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });

                // assert total balance/total supply
                expect(await pool.totalSupply(testEnv.bob)).to.equal(newUnits);
                expect(await pool.getTotalUnits(testEnv.bob)).to.equal(
                    newUnits
                );

                // assert bob's balance/units
                expect(
                    await pool.balanceOf({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(newUnits);
                expect(
                    await pool.getUnits({
                        member: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(newUnits);
            });

            it("Should be able to approve units", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const approvedUnits = "420";
                await pool.approve({
                    spender: testEnv.charlie.address,
                    amount: approvedUnits,
                    signer: testEnv.bob,
                });
                expect(
                    await pool.allowance({
                        owner: testEnv.bob.address,
                        spender: testEnv.charlie.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(approvedUnits);
            });

            it("Should be able to increase allowance", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const approvedUnits = "420";
                await pool.approve({
                    spender: testEnv.charlie.address,
                    amount: approvedUnits,
                    signer: testEnv.bob,
                });
                const increasedUnits = "69";
                await pool.increaseAllowance({
                    spender: testEnv.charlie.address,
                    amount: increasedUnits,
                    signer: testEnv.bob,
                });
                expect(
                    await pool.allowance({
                        owner: testEnv.bob.address,
                        spender: testEnv.charlie.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(toBN(approvedUnits).add(toBN(increasedUnits)));
            });

            it("Should be able to decrease allowance", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const approvedUnits = "420";
                await pool.approve({
                    spender: testEnv.charlie.address,
                    amount: approvedUnits,
                    signer: testEnv.bob,
                });
                const decreasedUnits = "69";
                await pool.decreaseAllowance({
                    spender: testEnv.charlie.address,
                    amount: decreasedUnits,
                    signer: testEnv.bob,
                });
                expect(
                    await pool.allowance({
                        owner: testEnv.bob.address,
                        spender: testEnv.charlie.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(toBN(approvedUnits).sub(toBN(decreasedUnits)));
            });

            it("Should be able to transfer units", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const transferUnits = "420";
                await pool.transfer({
                    to: testEnv.charlie.address,
                    amount: transferUnits,
                    signer: testEnv.bob,
                });
                expect(
                    await pool.balanceOf({
                        account: testEnv.charlie.address,
                        providerOrSigner: testEnv.charlie,
                    })
                ).to.equal(transferUnits);
                expect(
                    await pool.balanceOf({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(toBN(newUnits).sub(toBN(transferUnits)));
            });

            it("Should be able to transferFrom units", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const approvedUnits = "420";
                await pool.approve({
                    spender: testEnv.charlie.address,
                    amount: approvedUnits,
                    signer: testEnv.bob,
                });
                const transferUnits = "69";
                await pool.transferFrom({
                    from: testEnv.bob.address,
                    to: testEnv.charlie.address,
                    amount: transferUnits,
                    signer: testEnv.charlie,
                });
                expect(
                    await pool.balanceOf({
                        account: testEnv.charlie.address,
                        providerOrSigner: testEnv.charlie,
                    })
                ).to.equal(transferUnits);
                expect(
                    await pool.balanceOf({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(toBN(newUnits).sub(toBN(transferUnits)));
            });

            it("Should be able to update units (increase)", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "69";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const increasedUnits = "420";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits: increasedUnits,
                    signer: testEnv.alice,
                });
                expect(
                    await pool.balanceOf({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(increasedUnits);
            });

            it("Should be able to update units (decrease)", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "420";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const decreasedUnits = "69";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits: decreasedUnits,
                    signer: testEnv.alice,
                });
                expect(
                    await pool.balanceOf({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(decreasedUnits);
            });

            it("Should be able to update units (remove all)", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "420";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits: "0",
                    signer: testEnv.alice,
                });
                expect(
                    await pool.balanceOf({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal("0");
            });

            it("Should be able to connect and disconnect from pool", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "420";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const connectPoolOperation =
                    await testEnv.wrapperSuperToken.connectPool({
                        pool: pool.contract.address,
                        shouldUseCallAgreement: true,
                    });
                await connectPoolOperation.exec(testEnv.bob);
                expect(
                    await testEnv.wrapperSuperToken.isMemberConnected({
                        pool: pool.contract.address,
                        member: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(true);
                expect(
                    await pool.getTotalConnectedUnits(testEnv.alice)
                ).to.equal(newUnits);
                const disconnectPoolOperation =
                    await testEnv.wrapperSuperToken.disconnectPool({
                        pool: pool.contract.address,
                        shouldUseCallAgreement: true,
                    });
                await disconnectPoolOperation.exec(testEnv.bob);
                expect(
                    await testEnv.wrapperSuperToken.isMemberConnected({
                        pool: pool.contract.address,
                        member: testEnv.bob.address,
                        providerOrSigner: testEnv.bob,
                    })
                ).to.equal(false);
                expect(
                    await pool.getTotalConnectedUnits(testEnv.alice)
                ).to.equal("0");
            });

            it("Should be able to distribute tokens", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const aliceBalanceBefore =
                    await testEnv.wrapperSuperToken.balanceOf({
                        account: testEnv.alice.address,
                        providerOrSigner: testEnv.alice,
                    });
                const newUnits = "10";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const amountToDistribute = "1000";
                const actualAmountDistributed =
                    await testEnv.wrapperSuperToken.estimateDistributionActualAmount(
                        {
                            from: testEnv.alice.address,
                            requestedAmount: amountToDistribute,
                            pool: pool.contract.address,
                            providerOrSigner: testEnv.alice,
                        }
                    );
                const operation =
                    await testEnv.wrapperSuperToken.distributeWithGDA({
                        from: testEnv.alice.address,
                        requestedAmount: amountToDistribute,
                        pool: pool.contract.address,
                        shouldUseCallAgreement: true,
                    });
                await operation.exec(testEnv.alice);

                const aliceBalanceAfter =
                    await testEnv.wrapperSuperToken.balanceOf({
                        account: testEnv.alice.address,
                        providerOrSigner: testEnv.alice,
                    });
                expect(aliceBalanceAfter).to.equal(
                    toBN(aliceBalanceBefore).sub(toBN(actualAmountDistributed))
                );
            });

            it("Should be able to distribute flow tokens", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10";
                await pool.updateMember({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const requestedFlowRate = "1000";
                const actualDistributionFlowRate =
                    await testEnv.wrapperSuperToken.estimateFlowDistributionActualFlowRate(
                        {
                            from: testEnv.alice.address,
                            requestedFlowRate: requestedFlowRate,
                            pool: pool.contract.address,
                            providerOrSigner: testEnv.alice,
                        }
                    );
                const operation =
                    await testEnv.wrapperSuperToken.distributeFlow({
                        from: testEnv.alice.address,
                        requestedFlowRate: requestedFlowRate,
                        pool: pool.contract.address,
                        shouldUseCallAgreement: true,
                    });
                await operation.exec(testEnv.alice);

                expect(await testEnv.wrapperSuperToken.getGDANetFlow({
                    account: testEnv.alice.address,
                    providerOrSigner: testEnv.alice,
                })).to.equal(toBN(actualDistributionFlowRate.actualFlowRate).mul(toBN("-1")));

                const connectPoolOperation =
                    await testEnv.wrapperSuperToken.connectPool({
                        pool: pool.contract.address,
                        shouldUseCallAgreement: true,
                    });
                await connectPoolOperation.exec(testEnv.bob);

                expect(await testEnv.wrapperSuperToken.getGDANetFlow({
                    account: testEnv.bob.address,
                    providerOrSigner: testEnv.alice,
                })).to.equal(toBN(actualDistributionFlowRate.actualFlowRate));

                expect(
                    await testEnv.wrapperSuperToken.getPoolAdjustmentFlowRate({
                        pool: pool.contract.address,
                        providerOrSigner: testEnv.alice,
                    })
                ).to.equal("0");

                const poolAdjustmentFlowInfo =
                    await testEnv.wrapperSuperToken.getPoolAdjustmentFlowInfo({
                        pool: pool.contract.address,
                        providerOrSigner: testEnv.alice,
                    });
                expect(poolAdjustmentFlowInfo.flowRate).to.equal("0");
                expect(poolAdjustmentFlowInfo.recipient).to.equal(
                    testEnv.alice.address
                );
                const encoder = new ethers.utils.AbiCoder();
                const network = await testEnv.alice.provider?.getNetwork();
                if (!network) throw new Error("no network");
                const encodedData = encoder.encode(
                    ["uint256", "string", "address", "address"],
                    [
                        network.chainId,
                        "poolAdjustmentFlow",
                        pool.contract.address,
                        testEnv.alice.address,
                    ]
                );
                const flowHash = ethers.utils.keccak256(encodedData);
                expect(poolAdjustmentFlowInfo.flowHash).to.equal(flowHash);
            });

            it("Should be able to claimAllForMember as the member", async () => {});

            it("Should be able to claimAllForMember for a member", async () => {});

            it("Should be able to claimAll", async () => {});
        });
    }
);
