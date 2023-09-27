import { expect } from "chai";

import { makeSuite, TestEnvironment } from "./TestEnvironment";
import { getPerSecondFlowRateByMonth } from "../src";

const createFlow = async (testEnv: TestEnvironment) => {
    const flowRate = getPerSecondFlowRateByMonth("1000");
    await testEnv.wrapperSuperToken
        .createFlow({
            sender: testEnv.alice.address,
            receiver: testEnv.bob.address,
            flowRate,
        })
        .exec(testEnv.alice);

    return await testEnv.wrapperSuperToken.constantOutflowNFTProxy.getTokenId({
        superToken: testEnv.wrapperSuperToken.address,
        sender: testEnv.alice.address,
        receiver: testEnv.bob.address,
        providerOrSigner: testEnv.alice,
    });
};

makeSuite("SuperToken-NFT Tests", (testEnv: TestEnvironment) => {
    describe("Revert cases", () => {
        it("Should revert when trying to transferFrom", async () => {
            const tokenId = await createFlow(testEnv);

            await expect(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy
                    .transferFrom({
                        from: testEnv.alice.address,
                        to: testEnv.bob.address,
                        tokenId,
                    })
                    .exec(testEnv.alice)
            ).to.be.revertedWithCustomError(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy.contract,
                "CFA_NFT_TRANSFER_IS_NOT_ALLOWED"
            );
        });

        it("Should revert when trying to safeTransferFrom", async () => {
            const tokenId = await createFlow(testEnv);

            await expect(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy
                    .safeTransferFrom({
                        from: testEnv.alice.address,
                        to: testEnv.bob.address,
                        tokenId,
                    })
                    .exec(testEnv.alice)
            ).to.be.revertedWithCustomError(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy.contract,
                "CFA_NFT_TRANSFER_IS_NOT_ALLOWED"
            );
        });

        it("Should revert when trying to safeTransferFromWithData", async () => {
            const tokenId = await createFlow(testEnv);

            await expect(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy
                    .safeTransferFromWithData({
                        from: testEnv.alice.address,
                        to: testEnv.bob.address,
                        tokenId,
                        data: "0x",
                    })
                    .exec(testEnv.alice)
            ).to.be.revertedWithCustomError(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy.contract,
                "CFA_NFT_TRANSFER_IS_NOT_ALLOWED"
            );
        });

        it("Should revert if ownerOf token does not exist", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.ownerOf(
                    {
                        tokenId: "69",
                        providerOrSigner: testEnv.alice,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain("CFA_NFT_INVALID_TOKEN_ID");
            }
        });

        it("Should revert if approve to owner", async () => {
            const tokenId = await createFlow(testEnv);

            await expect(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy
                    .approve({
                        approved: testEnv.alice.address,
                        tokenId,
                    })
                    .exec(testEnv.alice)
            ).to.be.revertedWithCustomError(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy.contract,
                "CFA_NFT_APPROVE_TO_CURRENT_OWNER"
            );
        });

        it("Should revert if approve on behalf of someone else", async () => {
            const tokenId = await createFlow(testEnv);

            await expect(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy
                    .approve({
                        approved: testEnv.bob.address,
                        tokenId,
                    })
                    .exec(testEnv.bob)
            ).to.be.revertedWithCustomError(
                testEnv.wrapperSuperToken.constantOutflowNFTProxy.contract,
                "CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL"
            );
        });

        it("Should catch error in balanceOf", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.balanceOf(
                    {
                        owner: "0x",
                        providerOrSigner: testEnv.alice,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting balanceOf"
                );
            }
        });

        it("Should catch error in getApproved", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.getApproved(
                    {
                        tokenId: "0x",
                        providerOrSigner: testEnv.alice,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting getApproved"
                );
            }
        });

        it("Should catch error in isApprovedForAll", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.isApprovedForAll(
                    {
                        owner: "0x",
                        operator: "0x",
                        providerOrSigner: testEnv.alice,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting isApprovedForAll"
                );
            }
        });

        it("Should catch error in name", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.name({
                    providerOrSigner: testEnv.alice,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting name"
                );
            }
        });

        it("Should catch error in symbol", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.symbol({
                    providerOrSigner: testEnv.alice,
                });
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting symbol"
                );
            }
        });

        it("Should catch error in tokenURI", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.tokenURI(
                    {
                        tokenId: "0x",
                        providerOrSigner: testEnv.alice,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting tokenURI"
                );
            }
        });

        it("Should catch error in getTokenId", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.getTokenId(
                    {
                        superToken: testEnv.wrapperSuperToken.address,
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        providerOrSigner: "testEnv.alice" as any,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting token id"
                );
            }
        });

        it("Should catch error in flowDataByTokenId", async () => {
            try {
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.flowDataByTokenId(
                    {
                        tokenId: "0x",
                        providerOrSigner: testEnv.alice,
                    }
                );
            } catch (err: any) {
                expect(err.message).to.contain(
                    "There was an error getting flow data by token id"
                );
            }
        });
    });

    describe("Happy Path Tests", () => {
        it("Should be able to get flowDataByTokenId", async () => {
            const tokenId = await createFlow(testEnv);

            const flowData =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.flowDataByTokenId(
                    {
                        tokenId,
                        providerOrSigner: testEnv.alice,
                    }
                );
            expect(flowData.flowSender).to.equal(testEnv.alice.address);
            expect(flowData.flowReceiver).to.equal(testEnv.bob.address);
        });

        it("Should be able to approve", async () => {
            const tokenId = await createFlow(testEnv);

            await testEnv.wrapperSuperToken.constantOutflowNFTProxy
                .approve({
                    approved: testEnv.bob.address,
                    tokenId,
                })
                .exec(testEnv.alice);

            const approved =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.getApproved(
                    {
                        tokenId,
                        providerOrSigner: testEnv.alice,
                    }
                );
            expect(approved).to.equal(testEnv.bob.address);
        });

        it("Should be able to setApprovalForAll", async () => {
            await testEnv.wrapperSuperToken.constantOutflowNFTProxy
                .setApprovalForAll({
                    operator: testEnv.bob.address,
                    approved: true,
                })
                .exec(testEnv.alice);

            const approved =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.isApprovedForAll(
                    {
                        owner: testEnv.alice.address,
                        operator: testEnv.bob.address,
                        providerOrSigner: testEnv.alice,
                    }
                );
            expect(approved).to.equal(true);
        });

        it("Should be able to get ownerOf", async () => {
            const tokenId = await createFlow(testEnv);

            const owner =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.ownerOf(
                    {
                        tokenId,
                        providerOrSigner: testEnv.alice,
                    }
                );
            expect(owner).to.equal(testEnv.alice.address);
        });

        it("Should be able to get balanceOf (always returns 1)", async () => {
            const balance =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.balanceOf(
                    {
                        owner: testEnv.alice.address,
                        providerOrSigner: testEnv.alice,
                    }
                );
            expect(balance.toString()).to.equal("1");
        });

        it("Should be able to get name", async () => {
            const name =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.name({
                    providerOrSigner: testEnv.alice,
                });
            expect(name).to.equal("Constant Outflow NFT");
        });

        it("Should be able to get tokenURI", async () => {
            const tokenId = await createFlow(testEnv);

            const tokenURI =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.tokenURI(
                    {
                        tokenId,
                        providerOrSigner: testEnv.alice,
                    }
                );
            expect(tokenURI).to.not.be.empty;
        });

        it("Should be able to get symbol", async () => {
            const symbol =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.symbol({
                    providerOrSigner: testEnv.alice,
                });
            expect(symbol.toString()).to.equal("COF");
        });
    });
});
