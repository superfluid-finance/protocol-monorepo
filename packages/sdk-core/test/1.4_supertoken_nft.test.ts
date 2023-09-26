import { expect } from "chai";

import { makeSuite, TestEnvironment } from "./TestEnvironment";
import { getPerSecondFlowRateByMonth } from "../src";

makeSuite("SuperToken-NFT Tests", (testEnv: TestEnvironment) => {
    describe("Revert cases", () => {
        let tokenId: string;
        beforeEach(async () => {
            const flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);

            tokenId =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.getTokenId(
                    {
                        superToken: testEnv.wrapperSuperToken.address,
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        providerOrSigner: testEnv.alice,
                    }
                );
        });

        it("Should revert when trying to transferFrom", async () => {
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
    });

    describe("Happy Path Tests", () => {
        let tokenId: string;
        beforeEach(async () => {
            const flowRate = getPerSecondFlowRateByMonth("1000");
            await testEnv.wrapperSuperToken
                .createFlow({
                    sender: testEnv.alice.address,
                    receiver: testEnv.bob.address,
                    flowRate,
                })
                .exec(testEnv.alice);

            tokenId =
                await testEnv.wrapperSuperToken.constantOutflowNFTProxy.getTokenId(
                    {
                        superToken: testEnv.wrapperSuperToken.address,
                        sender: testEnv.alice.address,
                        receiver: testEnv.bob.address,
                        providerOrSigner: testEnv.alice,
                    }
                );
        });

        it("Should be able to get flowDataByTokenId", async () => {
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
    });
});
