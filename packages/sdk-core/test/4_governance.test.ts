import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { SuperToken as SuperTokenType } from "../src/typechain";
import { setup } from "../scripts/setup";
import { WrapperSuperToken } from "../src";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import hre from "hardhat";

const DEFAULT_PARAMS = {
    LIQUIDATION_PERIOD: "3600",
    PATRICIAN_PERIOD: "720",
    REWARD_ADDRESS: "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266",
    SUPER_TOKEN_MINIMUM_DEPOSIT: "0",
};

describe("Governance Tests", () => {
    let evmSnapshotId: string;
    let framework: Framework;
    let deployer: SignerWithAddress;
    let superToken: SuperTokenType;
    let daix: WrapperSuperToken;

    before(async () => {
        const { frameworkClass, Deployer, SuperToken } = await setup({
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
        });
        framework = frameworkClass;
        deployer = Deployer;
        superToken = SuperToken;
        daix = await framework.loadWrapperSuperToken(superToken.address);
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    beforeEach(async () => {
        await hre.network.provider.send("evm_revert", [evmSnapshotId]);
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    it("Should get default governance parameters", async () => {
        const defaultParams =
            await framework.governance.getGovernanceParameters({
                providerOrSigner: deployer,
            });
        expect(defaultParams.liquidationPeriod).to.equal(
            DEFAULT_PARAMS.LIQUIDATION_PERIOD
        );
        expect(defaultParams.patricianPeriod).to.equal(
            DEFAULT_PARAMS.PATRICIAN_PERIOD
        );
        expect(defaultParams.rewardAddress).to.equal(
            DEFAULT_PARAMS.REWARD_ADDRESS
        );
        expect(defaultParams.minimumDeposit).to.equal(
            DEFAULT_PARAMS.SUPER_TOKEN_MINIMUM_DEPOSIT
        );
    });

    it("Should get token specific governance parameters", async () => {
        // NOTE: the token specific params are unset
        const tokenSpecificParams = await daix.getGovernanceParameters(
            deployer
        );
        expect(tokenSpecificParams.liquidationPeriod).to.equal(
            DEFAULT_PARAMS.LIQUIDATION_PERIOD
        );
        expect(tokenSpecificParams.patricianPeriod).to.equal(
            DEFAULT_PARAMS.PATRICIAN_PERIOD
        );
        expect(tokenSpecificParams.rewardAddress).to.equal(
            DEFAULT_PARAMS.REWARD_ADDRESS
        );
        expect(tokenSpecificParams.minimumDeposit).to.equal(
            DEFAULT_PARAMS.SUPER_TOKEN_MINIMUM_DEPOSIT
        );
    });
});
