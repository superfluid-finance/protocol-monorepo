import { expect } from "chai";
import {
    TestEnvironment,
    makeSuite,
    validateOperationShouldUseCallAgreement,
} from "./TestEnvironment";
import SuperfluidPool from "../src/SuperfluidPool";
import { ethers } from "ethers";
import { WrapperSuperToken, toBN } from "../src";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";

interface ShouldDistributeTokensParams {
    shouldUseCallAgreement: boolean;
    newUnits: string;
    amountToDistribute: string;
    admin: SignerWithAddress;
    distributor: SignerWithAddress;
    member: SignerWithAddress;
}

interface ShouldConnectPoolParams {
    shouldUseCallAgreement: boolean;
    superToken: WrapperSuperToken;
    pool: SuperfluidPool;
    member: SignerWithAddress;
    doConnect: boolean;
}

interface ShouldUpdateMemberParams {
    pool: SuperfluidPool;
    newUnits: string;
    member: SignerWithAddress;
    admin: SignerWithAddress;
}

const createPoolAndReturnPoolClass = async (
    superToken: WrapperSuperToken,
    signer: SignerWithAddress,
    admin: string
) => {
    const data = await superToken.createPool({
        admin,
        signer,
    });

    expect(
        await superToken.isPool({
            account: data.poolAddress,
            providerOrSigner: signer,
        })
    ).to.be.true;
    const pool = new SuperfluidPool(data.poolAddress);

    expect(await pool.getPoolAdmin(signer)).to.be.equal(signer.address);
    expect(await pool.getSuperToken(signer)).to.be.equal(superToken.address);

    return pool;
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

            const shouldUpdateMember = async (
                params: ShouldUpdateMemberParams
            ) => {
                const providerSigner = params.member;
                const memberUnitsBefore = await params.pool.getUnits({
                    member: params.member.address,
                    providerOrSigner: providerSigner,
                });
                const balanceOfBefore = await params.pool.balanceOf({
                    account: params.member.address,
                    providerOrSigner: providerSigner,
                });
                const totalSupplyBefore = await params.pool.totalSupply(
                    providerSigner
                );
                const totalUnitsBefore = await params.pool.getTotalUnits(
                    providerSigner
                );
                const unitsDelta = toBN(params.newUnits).sub(memberUnitsBefore);
                await params.pool.updateMember({
                    member: params.member.address,
                    newUnits: params.newUnits,
                    signer: params.admin,
                });

                // assert total balance/total supply
                expect(await params.pool.totalSupply(providerSigner)).to.equal(
                    toBN(totalSupplyBefore).add(unitsDelta)
                );
                expect(await params.pool.getTotalUnits(providerSigner)).to.equal(
                    toBN(totalUnitsBefore).add(unitsDelta)
                );

                // assert member's balance/units
                expect(
                    await params.pool.balanceOf({
                        account: params.member.address,
                        providerOrSigner: providerSigner,
                    })
                ).to.equal(toBN(balanceOfBefore).add(unitsDelta));
                expect(
                    await params.pool.getUnits({
                        member: params.member.address,
                        providerOrSigner: providerSigner,
                    })
                ).to.equal(toBN(memberUnitsBefore).add(unitsDelta));
            };

            it("Should be able to update units for member", async () => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await shouldUpdateMember({
                    pool,
                    admin: testEnv.alice,
                    member: testEnv.bob,
                    newUnits
                });
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

            const shouldConnectPool = async (
                params: ShouldConnectPoolParams
            ) => {
                const connectPoolOperation = params.doConnect
                    ? await params.superToken.connectPool({
                          pool: params.pool.contract.address,
                          shouldUseCallAgreement: params.shouldUseCallAgreement,
                      })
                    : await params.superToken.disconnectPool({
                          pool: params.pool.contract.address,
                          shouldUseCallAgreement: params.shouldUseCallAgreement,
                      });
                await connectPoolOperation.exec(params.member);

                expect(
                    await params.superToken.isMemberConnected({
                        pool: params.pool.contract.address,
                        member: params.member.address,
                        providerOrSigner: params.member,
                    })
                ).to.equal(params.doConnect);

                if (params.doConnect) {
                    expect(
                        await params.pool.getTotalConnectedUnits(params.member)
                    ).to.equal(
                        await params.pool.getUnits({
                            member: params.member.address,
                            providerOrSigner: params.member,
                        })
                    );
                } else {
                    expect(
                        await params.pool.getTotalConnectedUnits(params.member)
                    ).to.equal("0");
                }
            };

            context(
                "Should be able to connect and disconnect from pool",
                async () => {
                    it("With Call Agreement", async () => {
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
                        await shouldConnectPool({
                            shouldUseCallAgreement: true,
                            superToken: testEnv.wrapperSuperToken,
                            pool,
                            member: testEnv.bob,
                            doConnect: true,
                        });
                        await shouldConnectPool({
                            shouldUseCallAgreement: true,
                            superToken: testEnv.wrapperSuperToken,
                            pool,
                            member: testEnv.bob,
                            doConnect: false,
                        });
                    });
                }
            );

            const shouldDistributeTokens = async (
                params: ShouldDistributeTokensParams
            ) => {
                const pool = await createPoolAndReturnPoolClass(
                    testEnv.wrapperSuperToken,
                    params.admin,
                    params.admin.address
                );
                const distributorBalanceBefore =
                    await testEnv.wrapperSuperToken.balanceOf({
                        account: params.distributor.address,
                        providerOrSigner: params.distributor,
                    });
                const memberBalanceBefore =
                    await testEnv.wrapperSuperToken.balanceOf({
                        account: params.member.address,
                        providerOrSigner: params.member,
                    });
                await pool.updateMember({
                    member: params.member.address,
                    newUnits: params.newUnits,
                    signer: params.admin,
                });
                const actualAmountDistributed =
                    await testEnv.wrapperSuperToken.estimateDistributionActualAmount(
                        {
                            from: params.distributor.address,
                            requestedAmount: params.amountToDistribute,
                            pool: pool.contract.address,
                            providerOrSigner: params.distributor,
                        }
                    );
                const operation =
                    await testEnv.wrapperSuperToken.distributeWithGDA({
                        from: params.distributor.address,
                        requestedAmount: params.amountToDistribute,
                        pool: pool.contract.address,
                        shouldUseCallAgreement: params.shouldUseCallAgreement,
                    });
                await operation.exec(params.distributor);
                validateOperationShouldUseCallAgreement(
                    testEnv,
                    operation,
                    params.shouldUseCallAgreement,
                    testEnv.sdkFramework.gdaV1.forwarder.address
                );

                const distributorBalanceAfter =
                    await testEnv.wrapperSuperToken.balanceOf({
                        account: params.distributor.address,
                        providerOrSigner: params.distributor,
                    });
                const memberBalanceAfter =
                    await testEnv.wrapperSuperToken.balanceOf({
                        account: params.member.address,
                        providerOrSigner: params.member,
                    });
                expect(distributorBalanceAfter).to.equal(
                    toBN(distributorBalanceBefore).sub(
                        toBN(actualAmountDistributed)
                    )
                );

                const isMemberConnected =
                    await testEnv.wrapperSuperToken.isMemberConnected({
                        pool: pool.contract.address,
                        member: params.member.address,
                        providerOrSigner: params.member,
                    });
                if (isMemberConnected) {
                    expect(memberBalanceAfter).to.equal(
                        toBN(memberBalanceBefore).add(
                            toBN(actualAmountDistributed)
                        )
                    );
                } else {
                    expect(memberBalanceAfter).to.equal(memberBalanceBefore);
                }

                return pool;
            };

            context("Should be able to distribute tokens", async () => {
                it("With Call Agreement", async () => {
                    await shouldDistributeTokens({
                        newUnits: "10",
                        amountToDistribute: "1000",
                        admin: testEnv.alice,
                        distributor: testEnv.alice,
                        member: testEnv.bob,
                        shouldUseCallAgreement: true,
                    });
                });

                it("With forwarder", async () => {
                    await shouldDistributeTokens({
                        newUnits: "10",
                        amountToDistribute: "1000",
                        admin: testEnv.alice,
                        distributor: testEnv.alice,
                        member: testEnv.bob,
                        shouldUseCallAgreement: false,
                    });
                });
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

                expect(
                    await testEnv.wrapperSuperToken.getGDANetFlow({
                        account: testEnv.alice.address,
                        providerOrSigner: testEnv.alice,
                    })
                ).to.equal(
                    toBN(actualDistributionFlowRate.actualFlowRate).mul(
                        toBN("-1")
                    )
                );

                const connectPoolOperation =
                    await testEnv.wrapperSuperToken.connectPool({
                        pool: pool.contract.address,
                        shouldUseCallAgreement: true,
                    });
                await connectPoolOperation.exec(testEnv.bob);

                expect(
                    await testEnv.wrapperSuperToken.getGDANetFlow({
                        account: testEnv.bob.address,
                        providerOrSigner: testEnv.alice,
                    })
                ).to.equal(toBN(actualDistributionFlowRate.actualFlowRate));

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

            // it("Should be able to claimAllForMember as the member", async () => {
            //     const pool = await shouldDistributeTokens({
            //         ...DEFAULT_DISTRIBUTE_PARAMS,
            //     });
            //     await pool.claimAllForMember({
            //         member: testEnv.bob.address,
            //         signer: testEnv.bob,
            //     });
            // });

            // it("Should be able to claimAllForMember for a member", async () => {
            //     const pool = await shouldDistributeTokens({
            //         ...DEFAULT_DISTRIBUTE_PARAMS,
            //     });
            //     await pool.claimAllForMember({
            //         member: testEnv.bob.address,
            //         signer: testEnv.alice,
            //     });
            // });

            // it("Should be able to claimAll", async () => {
            //     const pool = await shouldDistributeTokens({
            //         ...DEFAULT_DISTRIBUTE_PARAMS,
            //     });
            //     await pool.claimAll(testEnv.bob);
            // });
        });
    }
);
