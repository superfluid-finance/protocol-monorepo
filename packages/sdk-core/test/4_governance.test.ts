import { expect } from "chai";
import { TestEnvironment, _makeSuite } from "./TestEnvironment";

_makeSuite("Governance Tests", (testEnv: TestEnvironment) => {
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
        expect(defaultParams.rewardAddress).to.equal(
            testEnv.constants.DEFAULT_REWARD_ADDRESS
        );
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
        expect(tokenSpecificParams.rewardAddress).to.equal(
            testEnv.constants.DEFAULT_REWARD_ADDRESS
        );
        expect(tokenSpecificParams.minimumDeposit).to.equal("0");
    });
});
