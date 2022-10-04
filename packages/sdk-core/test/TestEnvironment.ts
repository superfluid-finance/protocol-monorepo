import hre, { ethers } from "hardhat";
import { abi as TestTokenABI } from "../src/abi/TestToken.json";
import {
    Framework,
    NativeAssetSuperToken,
    PureSuperToken,
    toBN,
    WrapperSuperToken,
} from "../src";
import {
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    SuperfluidFrameworkDeployer,
    TestToken,
} from "../src/typechain";
import { deployContractsAndToken } from "../../subgraph/scripts/deployContractsAndToken";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { JsonRpcProvider } from "@ethersproject/providers";

export const TEST_ENVIRONMENT_CONSTANTS = {
    DEFAULT_REWARD_ADDRESS: "0x0000000000000000000000000000000000000045", // address(69)
    HARDHAT_PRIVATE_KEY:
        "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80",
    INITIAL_TOKEN_BALANCE: ethers.utils.parseUnits("10000000000"),
    LIQUIDATION_PERIOD: toBN(14400),
    PATRICIAN_PERIOD: toBN(1800),
    MAX_FLOW_RATE: toBN(2).pow(toBN(95)).sub(toBN(1)),
};

export interface TestEnvironment {
    provider: JsonRpcProvider;
    sdkFramework: Framework;
    superfluidFrameworkDeployer: SuperfluidFrameworkDeployer;
    frameworkAddresses: SuperfluidFrameworkDeployer.FrameworkStructOutput;
    constants: typeof TEST_ENVIRONMENT_CONSTANTS;
    users: SignerWithAddress[];
    alice: SignerWithAddress;
    bob: SignerWithAddress;
    charlie: SignerWithAddress;
    cfaV1: IConstantFlowAgreementV1;
    idaV1: IInstantDistributionAgreementV1;
    wrapperSuperToken: WrapperSuperToken;
    nativeAssetSuperToken: NativeAssetSuperToken;
    pureSuperToken: PureSuperToken;
    token: TestToken;
    snapshot: string;
}

const testEnv: TestEnvironment = {
    provider: hre.ethers.provider,
    sdkFramework: {} as Framework,
    superfluidFrameworkDeployer: {} as SuperfluidFrameworkDeployer,
    frameworkAddresses: {} as SuperfluidFrameworkDeployer.FrameworkStructOutput,
    constants: TEST_ENVIRONMENT_CONSTANTS,
    alice: {} as SignerWithAddress,
    bob: {} as SignerWithAddress,
    charlie: {} as SignerWithAddress,
    users: [],
    cfaV1: {} as IConstantFlowAgreementV1,
    idaV1: {} as IInstantDistributionAgreementV1,
    token: {} as TestToken,
    wrapperSuperToken: {} as WrapperSuperToken,
    nativeAssetSuperToken: {} as NativeAssetSuperToken,
    pureSuperToken: {} as PureSuperToken,
    snapshot: {} as string,
};

export const initializeTestEnvironment = async () => {
    const signers = await ethers.getSigners();

    console.log("Deploy Superfluid Test Framework...");
    testEnv.superfluidFrameworkDeployer = await deployContractsAndToken();

    console.log("Initialize Signers...");
    [testEnv.alice, testEnv.bob, testEnv.charlie] = signers;
    testEnv.users = signers;

    console.log("Initialize Framework...");
    testEnv.frameworkAddresses =
        await testEnv.superfluidFrameworkDeployer.getFramework();
    const provider = hre.ethers.provider;
    const chainId = (await provider.getNetwork()).chainId;

    testEnv.sdkFramework = await Framework.create({
        chainId,
        resolverAddress: testEnv.frameworkAddresses.resolver,
        provider,
        protocolReleaseVersion: "test",
    });

    console.log("Set Agreement Contracts...");
    testEnv.cfaV1 = testEnv.sdkFramework.cfaV1.contract.connect(testEnv.alice);
    testEnv.idaV1 = testEnv.sdkFramework.idaV1.contract.connect(testEnv.alice);

    console.log("Load SuperToken and TestToken...");
    testEnv.wrapperSuperToken =
        await testEnv.sdkFramework.loadWrapperSuperToken("fDAIx");
    testEnv.nativeAssetSuperToken =
        await testEnv.sdkFramework.loadNativeAssetSuperToken("ETHx");
    testEnv.pureSuperToken = await testEnv.sdkFramework.loadPureSuperToken(
        "MRx"
    );
    testEnv.token = new ethers.Contract(
        testEnv.wrapperSuperToken.underlyingToken.address,
        TestTokenABI,
        testEnv.alice
    ) as TestToken;

    for (let i = 0; i < testEnv.users.length; i++) {
        const user = testEnv.users[i];
        await testEnv.token
            .connect(user)
            .mint(user.address, testEnv.constants.INITIAL_TOKEN_BALANCE);

        await testEnv.token
            .connect(user)
            .approve(
                testEnv.wrapperSuperToken.address,
                testEnv.constants.INITIAL_TOKEN_BALANCE
            );

        await testEnv.wrapperSuperToken
            .upgrade({
                amount: testEnv.constants.INITIAL_TOKEN_BALANCE.toString(),
            })
            .exec(user);
    }
};

export const _makeSuite = (
    name: string,
    tests: (testEnvironment: TestEnvironment) => void
) => {
    describe(name, () => {
        before(async () => {
            testEnv.snapshot = await hre.network.provider.send("evm_snapshot");
        });

        tests(testEnv);

        beforeEach(async () => {
            await hre.network.provider.send("evm_revert", [testEnv.snapshot]);
            testEnv.snapshot = await hre.network.provider.send("evm_snapshot");
        });
    });
};
