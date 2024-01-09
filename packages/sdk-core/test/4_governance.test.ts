import { expect } from "chai";
import { TestEnvironment, makeSuite } from "./TestEnvironment";

makeSuite("Governance Tests", (testEnv: TestEnvironment) => {
    it("Should get default governance parameters", async () => {
        const defaultParams =
            await testEnv.sdkFramework.governance.getGovernanceParameters({
                providerOrSigner: testEnv.alice,
            });
        expect(defaultParams.liquidationPeriod).to.equal(
            testEnv.constants.LIQUIDATION_PERIOD
        );
        expect(defaultParams.patricianPeriod).to.equal(
            testEnv.constants.PATRICIAN_PERIOD
        );
        const defaultRewardAddress =
            await testEnv.sdkFramework.governance.getRewardAddress({
                providerOrSigner: testEnv.alice,
            });
        expect(defaultParams.rewardAddress).to.equal(defaultRewardAddress);
        expect(defaultParams.minimumDeposit).to.equal("0");
    });

    it("Should get token specific governance parameters", async () => {
        // NOTE: the token specific params are unset
        const tokenSpecificParams =
            await testEnv.wrapperSuperToken.getGovernanceParameters(
                testEnv.alice
            );
        expect(tokenSpecificParams.liquidationPeriod).to.equal(
            testEnv.constants.LIQUIDATION_PERIOD
        );
        expect(tokenSpecificParams.patricianPeriod).to.equal(
            testEnv.constants.PATRICIAN_PERIOD
        );
        const defaultRewardAddress =
            await testEnv.sdkFramework.governance.getRewardAddress({
                providerOrSigner: testEnv.alice,
            });
        expect(tokenSpecificParams.rewardAddress).to.equal(
            defaultRewardAddress
        );
        expect(tokenSpecificParams.minimumDeposit).to.equal("0");
    });
});
