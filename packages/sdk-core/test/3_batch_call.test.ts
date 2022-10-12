import { expect } from "chai";
import { getPerSecondFlowRateByMonth } from "../src";
import { ethers } from "ethers";
import { createCallAppActionOperation } from "./2_operation.test";
import { TestEnvironment, makeSuite } from "./TestEnvironment";

makeSuite("Batch Call Tests", (testEnv: TestEnvironment) => {
    it("Should throw an error when empty", async () => {
        try {
            await testEnv.sdkFramework.batchCall([{} as any]).exec(testEnv.bob);
        } catch (err: any) {
            expect(err.type).to.equal("UNSUPPORTED_OPERATION");
            expect(err.message).to.contain(
                "The operation at index 0 is unsupported"
            );
        }
    });

    it("Should throw an error when data not provided", async () => {
        try {
            await testEnv.sdkFramework
                .batchCall([{ type: "ERC20_APPROVE" } as any])
                .exec(testEnv.bob);
        } catch (err: any) {
            expect(err.message).to.contain("undefined");
        }
    });

    it("Should throw an error when type not provided", async () => {
        const transferOp = testEnv.wrapperSuperToken.transfer({
            receiver: testEnv.bob.address,
            amount: ethers.utils.parseUnits("1000").toString(),
        });
        try {
            await testEnv.sdkFramework
                .batchCall([
                    {
                        populateTransactionPromise:
                            transferOp.populateTransactionPromise,
                    } as any,
                ])
                .exec(testEnv.bob);
        } catch (err: any) {
            expect(err.type).to.equal("UNSUPPORTED_OPERATION");
            expect(err.message).to.contain(
                "The operation at index 0 is unsupported"
            );
        }
    });

    it("Should throw an error on unsupported operations", async () => {
        const transferOp = testEnv.wrapperSuperToken.transfer({
            receiver: testEnv.bob.address,
            amount: ethers.utils.parseUnits("1000").toString(),
        });
        const batchCall = testEnv.sdkFramework.batchCall([transferOp]);

        try {
            const promises = batchCall.getOperationStructArrayPromises;
            await Promise.all(promises);
        } catch (err: any) {
            expect(err.message).to.not.be.undefined;
            expect(err.cause).to.be.undefined;
        }
    });

    it("Should throw an error when amount not provided ERC20 approve", async () => {
        const approveOp = testEnv.wrapperSuperToken.approve({
            receiver: testEnv.bob.address,
        } as any);
        try {
            await testEnv.sdkFramework.batchCall([approveOp]).exec(testEnv.bob);
        } catch (err: any) {
            expect(err.message).to.not.be.undefined;
        }
    });

    it("Should throw an error when sender not provided ERC20 transferFrom", async () => {
        const amount = ethers.utils.parseUnits("1000").toString();
        const transferFromOp = testEnv.wrapperSuperToken.transferFrom({
            receiver: testEnv.bob.address,
            amount,
        } as any);
        try {
            await testEnv.sdkFramework
                .batchCall([transferFromOp])
                .exec(testEnv.bob);
        } catch (err: any) {
            expect(err.message).to.not.be.undefined;
        }
    });

    it("Should throw an error if batch call fails", async () => {
        const createFlowOp = testEnv.wrapperSuperToken.createFlow({
            sender: testEnv.bob.address,
            receiver: testEnv.alice.address,
            flowRate: getPerSecondFlowRateByMonth("10000"),
        });
        try {
            await testEnv.sdkFramework
                .batchCall([createFlowOp])
                .exec(testEnv.alice);
        } catch (err: any) {
            expect(err.message).to.not.be.undefined;
        }
    });

    it("Should throw an error if amount not provided for upgrade", async () => {
        const upgradeOp = testEnv.wrapperSuperToken.upgrade({} as any);
        try {
            await testEnv.sdkFramework
                .batchCall([upgradeOp])
                .exec(testEnv.alice);
        } catch (err: any) {
            expect(err.message).to.not.be.undefined;
        }
    });

    it("Should be able to create and execute a batch call (approve + transferFrom)", async () => {
        const amount = ethers.utils.parseUnits("1000").toString();
        const approveOp = testEnv.wrapperSuperToken.approve({
            receiver: testEnv.bob.address,
            amount,
        });
        const transferFromOp = testEnv.wrapperSuperToken.transferFrom({
            sender: testEnv.alice.address,
            receiver: testEnv.bob.address,
            amount,
        });
        const batchCall = testEnv.sdkFramework.batchCall([
            approveOp,
            transferFromOp,
        ]);
        await expect(batchCall.exec(testEnv.alice))
            .to.emit(
                testEnv.wrapperSuperToken.contract.connect(testEnv.alice),
                "Transfer"
            )
            .withArgs(testEnv.alice.address, testEnv.bob.address, amount);
    });

    it("Should be able to batch create flows and batch update flows and batch delete flows", async () => {
        let flowRate = getPerSecondFlowRateByMonth("10000");
        const createFlow1 = testEnv.wrapperSuperToken.createFlow({
            sender: testEnv.charlie.address,
            receiver: testEnv.bob.address,
            flowRate,
        });
        const createFlow2 = testEnv.wrapperSuperToken.createFlow({
            sender: testEnv.charlie.address,
            receiver: testEnv.alice.address,
            flowRate,
        });
        await testEnv.sdkFramework
            .batchCall([createFlow1, createFlow2])
            .exec(testEnv.charlie);

        flowRate = getPerSecondFlowRateByMonth("12500");

        const updateFlow1 = testEnv.wrapperSuperToken.updateFlow({
            sender: testEnv.charlie.address,
            receiver: testEnv.bob.address,
            flowRate,
        });
        const updateFlow2 = testEnv.wrapperSuperToken.updateFlow({
            sender: testEnv.charlie.address,
            receiver: testEnv.alice.address,
            flowRate,
        });
        await testEnv.sdkFramework
            .batchCall([updateFlow1, updateFlow2])
            .exec(testEnv.charlie);
        const deleteFlow1 = testEnv.wrapperSuperToken.deleteFlow({
            sender: testEnv.charlie.address,
            receiver: testEnv.bob.address,
        });
        const deleteFlow2 = testEnv.wrapperSuperToken.deleteFlow({
            sender: testEnv.charlie.address,
            receiver: testEnv.alice.address,
        });
        await testEnv.sdkFramework
            .batchCall([deleteFlow1, deleteFlow2])
            .exec(testEnv.charlie);
    });

    it("Should be able to create a call app action operation and execute from batch call", async () => {
        const NEW_VAL = 69;
        const { superAppTester, operation } =
            await createCallAppActionOperation(
                testEnv.alice,
                testEnv.sdkFramework,
                NEW_VAL
            );
        await testEnv.sdkFramework.batchCall([operation]).exec(testEnv.alice);
        expect(await superAppTester.val()).to.equal("69");
    });
});
