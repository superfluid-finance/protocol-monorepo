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

interface ShouldConnectPoolParams {
    testEnv: TestEnvironment;
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

interface ShouldClaimAllForMemberParams {
    pool: SuperfluidPool;
    member: SignerWithAddress;
    claimer: SignerWithAddress;
    superToken: WrapperSuperToken;
    claimAll?: boolean;
}

interface ShouldInstantDistributeParams {
    testEnv: TestEnvironment;
    shouldUseCallAgreement: boolean;
    newUnits: string;
    amountToDistribute: string;
    admin: SignerWithAddress;
    distributor: SignerWithAddress;
    member: SignerWithAddress;
}

interface ShouldFlowDistributeParams {
    testEnv: TestEnvironment;
    shouldUseCallAgreement: boolean;
    newUnits: string;
    requestedFlowRate: string;
    admin: SignerWithAddress;
    distributor: SignerWithAddress;
    member: SignerWithAddress;
    superToken: WrapperSuperToken;
}

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

            it("Should throw when trying to createPool with bad params", async () => {
                try {
                    await shouldCreatePool(
                        testEnv.wrapperSuperToken,
                        testEnv.alice,
                        ""
                    );
                } catch (err) {
                    expect(err.type).to.equal("GDAV1_WRITE");
                    expect(err.message).to.have.string(
                        "There was an error creating the GDA pool."
                    );
                }
            });

            it("Should throw an error on SuperfluidPool view functions when wrong params passed", async () => {
                const pool = await shouldCreatePool(
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
                await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
            });

            it("Should be able to update units for member", async () => {
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await shouldUpdateMember({
                    pool,
                    admin: testEnv.alice,
                    member: testEnv.bob,
                    newUnits,
                });
            });

            it("Should be able to approve units", async () => {
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "10000";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "69";
                await pool.updateMemberUnits({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const increasedUnits = "420";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "420";
                await pool.updateMemberUnits({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                const decreasedUnits = "69";
                await pool.updateMemberUnits({
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
                const pool = await shouldCreatePool(
                    testEnv.wrapperSuperToken,
                    testEnv.alice,
                    testEnv.alice.address
                );
                const newUnits = "420";
                await pool.updateMemberUnits({
                    member: testEnv.bob.address,
                    newUnits,
                    signer: testEnv.alice,
                });
                await pool.updateMemberUnits({
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

            context(
                "Should be able to connect and disconnect from pool",
                async () => {
                    let pool: SuperfluidPool;
                    beforeEach(async () => {
                        pool = await shouldCreatePool(
                            testEnv.wrapperSuperToken,
                            testEnv.alice,
                            testEnv.alice.address
                        );
                        const newUnits = "420";
                        await pool.updateMemberUnits({
                            member: testEnv.bob.address,
                            newUnits,
                            signer: testEnv.alice,
                        });
                    });

                    it("With Call Agreement", async () => {
                        await shouldConnectPool({
                            testEnv,
                            shouldUseCallAgreement: true,
                            superToken: testEnv.wrapperSuperToken,
                            pool,
                            member: testEnv.bob,
                            doConnect: true,
                        });
                        await shouldConnectPool({
                            testEnv,
                            shouldUseCallAgreement: true,
                            superToken: testEnv.wrapperSuperToken,
                            pool,
                            member: testEnv.bob,
                            doConnect: false,
                        });
                    });

                    it("With Forwarder", async () => {
                        await shouldConnectPool({
                            testEnv,
                            shouldUseCallAgreement: false,
                            superToken: testEnv.wrapperSuperToken,
                            pool,
                            member: testEnv.bob,
                            doConnect: true,
                        });
                        await shouldConnectPool({
                            testEnv,
                            shouldUseCallAgreement: false,
                            superToken: testEnv.wrapperSuperToken,
                            pool,
                            member: testEnv.bob,
                            doConnect: false,
                        });
                    });
                }
            );

            context("Should be able to distribute tokens", async () => {
                it("With Call Agreement", async () => {
                    await shouldInstantDistributeTokensToOneMember({
                        testEnv,
                        newUnits: "10",
                        amountToDistribute: "1000",
                        admin: testEnv.alice,
                        distributor: testEnv.alice,
                        member: testEnv.bob,
                        shouldUseCallAgreement: true,
                    });
                });

                it("With Forwarder", async () => {
                    await shouldInstantDistributeTokensToOneMember({
                        testEnv,
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
                it("With Call Agreement", async () => {
                    await shouldDistributeFlow({
                        testEnv,
                        superToken: testEnv.wrapperSuperToken,
                        newUnits: "10",
                        requestedFlowRate: "1000",
                        admin: testEnv.alice,
                        distributor: testEnv.alice,
                        member: testEnv.bob,
                        shouldUseCallAgreement: true,
                    });
                });

                it("With Forwarder", async () => {
                    await shouldDistributeFlow({
                        testEnv,
                        superToken: testEnv.wrapperSuperToken,
                        newUnits: "10",
                        requestedFlowRate: "1000",
                        admin: testEnv.alice,
                        distributor: testEnv.alice,
                        member: testEnv.bob,
                        shouldUseCallAgreement: false,
                    });
                });
            });

            it("Should be able to claimAllForMember as the member", async () => {
                const pool = await shouldInstantDistributeTokensToOneMember({
                    testEnv,
                    shouldUseCallAgreement: true,
                    newUnits: "1000",
                    amountToDistribute: "100000",
                    admin: testEnv.alice,
                    distributor: testEnv.alice,
                    member: testEnv.bob,
                });

                await shouldClaimAllForMember({
                    pool,
                    member: testEnv.bob,
                    claimer: testEnv.bob,
                    superToken: testEnv.wrapperSuperToken,
                });
            });

            it("Should be able to claimAllForMember for someone else (alice claims for bob)", async () => {
                const pool = await shouldInstantDistributeTokensToOneMember({
                    testEnv,
                    shouldUseCallAgreement: true,
                    newUnits: "1000",
                    amountToDistribute: "100000",
                    admin: testEnv.alice,
                    distributor: testEnv.alice,
                    member: testEnv.bob,
                });
                await shouldClaimAllForMember({
                    pool,
                    member: testEnv.bob,
                    claimer: testEnv.alice,
                    superToken: testEnv.wrapperSuperToken,
                });
            });

            it("Should be able to claimAll", async () => {
                const pool = await shouldInstantDistributeTokensToOneMember({
                    testEnv,
                    shouldUseCallAgreement: true,
                    newUnits: "1000",
                    amountToDistribute: "100000",
                    admin: testEnv.alice,
                    distributor: testEnv.alice,
                    member: testEnv.bob,
                });
                await shouldClaimAllForMember({
                    pool,
                    member: testEnv.bob,
                    claimer: testEnv.bob,
                    superToken: testEnv.wrapperSuperToken,
                    claimAll: true,
                });
            });
        });
    }
);

const shouldCreatePool = async (
    superToken: WrapperSuperToken,
    signer: SignerWithAddress,
    admin: string
) => {
    const data = await superToken.createPool({
        admin,
        config: {
            transferabilityForUnitsOwner: true,
            distributionFromAnyAddress: true,
        },
        signer,
    });

    expect(
        await superToken.isPool({
            account: data.pool.contract.address,
            providerOrSigner: signer,
        })
    ).to.be.true;
    const pool = new SuperfluidPool(data.pool.contract.address);

    expect(await pool.getPoolAdmin(signer)).to.be.equal(signer.address);
    expect(await pool.getSuperToken(signer)).to.be.equal(superToken.address);

    return pool;
};

const shouldUpdateMember = async (params: ShouldUpdateMemberParams) => {
    const providerSigner = params.member;
    const memberUnitsBefore = await params.pool.getUnits({
        member: params.member.address,
        providerOrSigner: providerSigner,
    });
    const balanceOfBefore = await params.pool.balanceOf({
        account: params.member.address,
        providerOrSigner: providerSigner,
    });
    const totalSupplyBefore = await params.pool.totalSupply(providerSigner);
    const totalUnitsBefore = await params.pool.getTotalUnits(providerSigner);
    const unitsDelta = toBN(params.newUnits).sub(memberUnitsBefore);
    await params.pool.updateMemberUnits({
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

const shouldConnectPool = async (params: ShouldConnectPoolParams) => {
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

    await validateOperationShouldUseCallAgreement(
        params.testEnv,
        connectPoolOperation,
        params.shouldUseCallAgreement,
        params.testEnv.sdkFramework.gdaV1.forwarder.address
    );

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

const shouldInstantDistributeTokensToOneMember = async (
    params: ShouldInstantDistributeParams
) => {
    const pool = await shouldCreatePool(
        params.testEnv.wrapperSuperToken,
        params.admin,
        params.admin.address
    );
    const distributorBalanceBefore =
        await params.testEnv.wrapperSuperToken.balanceOf({
            account: params.distributor.address,
            providerOrSigner: params.distributor,
        });
    const memberBalanceBefore =
        await params.testEnv.wrapperSuperToken.balanceOf({
            account: params.member.address,
            providerOrSigner: params.member,
        });
    await pool.updateMemberUnits({
        member: params.member.address,
        newUnits: params.newUnits,
        signer: params.admin,
    });
    const actualAmountDistributed =
        await params.testEnv.wrapperSuperToken.estimateDistributionActualAmount(
            {
                from: params.distributor.address,
                requestedAmount: params.amountToDistribute,
                pool: pool.contract.address,
                providerOrSigner: params.distributor,
            }
        );
    const operation = await params.testEnv.wrapperSuperToken.distributeWithGDA({
        from: params.distributor.address,
        requestedAmount: params.amountToDistribute,
        pool: pool.contract.address,
        shouldUseCallAgreement: params.shouldUseCallAgreement,
    });
    await operation.exec(params.distributor);
    await validateOperationShouldUseCallAgreement(
        params.testEnv,
        operation,
        params.shouldUseCallAgreement,
        params.testEnv.sdkFramework.gdaV1.forwarder.address
    );

    const distributorBalanceAfter =
        await params.testEnv.wrapperSuperToken.balanceOf({
            account: params.distributor.address,
            providerOrSigner: params.distributor,
        });
    const memberBalanceAfter = await params.testEnv.wrapperSuperToken.balanceOf(
        {
            account: params.member.address,
            providerOrSigner: params.member,
        }
    );
    expect(distributorBalanceAfter).to.equal(
        toBN(distributorBalanceBefore).sub(toBN(actualAmountDistributed))
    );

    const isMemberConnected =
        await params.testEnv.wrapperSuperToken.isMemberConnected({
            pool: pool.contract.address,
            member: params.member.address,
            providerOrSigner: params.member,
        });
    if (isMemberConnected) {
        expect(memberBalanceAfter).to.equal(
            toBN(memberBalanceBefore).add(toBN(actualAmountDistributed))
        );
    } else {
        expect(memberBalanceAfter).to.equal(memberBalanceBefore);
    }

    return pool;
};

const shouldDistributeFlow = async (params: ShouldFlowDistributeParams) => {
    const pool = await shouldCreatePool(
        params.superToken,
        params.admin,
        params.admin.address
    );
    const newUnits = "10";
    await pool.updateMemberUnits({
        member: params.member.address,
        newUnits,
        signer: params.admin,
    });
    const providerOrSigner = params.admin;
    const requestedFlowRate = "1000";
    const actualDistributionFlowRate =
        await params.superToken.estimateFlowDistributionActualFlowRate({
            from: params.distributor.address,
            requestedFlowRate: requestedFlowRate,
            pool: pool.contract.address,
            providerOrSigner,
        });
    const operation = await params.superToken.distributeFlow({
        from: params.distributor.address,
        requestedFlowRate: requestedFlowRate,
        pool: pool.contract.address,
        shouldUseCallAgreement: true,
    });
    await operation.exec(params.distributor);

    await validateOperationShouldUseCallAgreement(
        params.testEnv,
        operation,
        params.shouldUseCallAgreement,
        params.testEnv.sdkFramework.gdaV1.forwarder.address
    );

    expect(
        await params.superToken.getGDANetFlow({
            account: params.distributor.address,
            providerOrSigner,
        })
    ).to.equal(toBN(actualDistributionFlowRate.actualFlowRate).mul(toBN("-1")));

    const connectPoolOperation = await params.superToken.connectPool({
        pool: pool.contract.address,
        shouldUseCallAgreement: true,
    });
    await connectPoolOperation.exec(params.member);

    expect(
        await params.superToken.getGDANetFlow({
            account: params.member.address,
            providerOrSigner,
        })
    ).to.equal(toBN(actualDistributionFlowRate.actualFlowRate));

    expect(
        await params.superToken.getPoolAdjustmentFlowRate({
            pool: pool.contract.address,
            providerOrSigner,
        })
    ).to.equal("0");

    const poolAdjustmentFlowInfo =
        await params.superToken.getPoolAdjustmentFlowInfo({
            pool: pool.contract.address,
            providerOrSigner,
        });
    expect(poolAdjustmentFlowInfo.flowRate).to.equal("0");
    expect(poolAdjustmentFlowInfo.recipient).to.equal(params.admin.address);
    const encoder = new ethers.utils.AbiCoder();
    const network = await providerOrSigner.provider?.getNetwork();
    if (!network) throw new Error("no network");

    const encodedData = encoder.encode(
        ["uint256", "string", "address", "address"],
        [
            network.chainId,
            "poolAdjustmentFlow",
            pool.contract.address,
            params.admin.address,
        ]
    );
    const flowHash = ethers.utils.keccak256(encodedData);
    expect(poolAdjustmentFlowInfo.flowHash).to.equal(flowHash);
};

const shouldClaimAllForMember = async (
    params: ShouldClaimAllForMemberParams
) => {
    const memberBalanceBefore = await params.superToken.balanceOf({
        account: params.member.address,
        providerOrSigner: params.member,
    });
    const claimableBalanceData = await params.pool.getClaimableNow({
        member: params.member.address,
        providerOrSigner: params.member,
    });

    if (params.claimAll) {
        await params.pool.claimAll(params.member);
    } else {
        await params.pool.claimAllForMember({
            member: params.member.address,
            signer: params.claimer,
        });
    }

    const memberBalanceAfter = await params.superToken.balanceOf({
        account: params.member.address,
        providerOrSigner: params.member,
    });
    expect(toBN(memberBalanceAfter).sub(toBN(memberBalanceBefore))).to.equal(
        claimableBalanceData.claimableBalance
    );
};
