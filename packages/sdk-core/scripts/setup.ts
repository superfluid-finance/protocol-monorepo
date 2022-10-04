import { ethers } from "hardhat";
import { abi as TestTokenABI } from "../src/abi/TestToken.json";
import {
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    TestToken,
} from "../src/typechain";
import { deployContractsAndToken } from "../../subgraph/scripts/deployContractsAndToken";
import { Framework, WrapperSuperToken } from "../src";

export const HARDHAT_PRIVATE_KEY =
    "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80";
interface ISetupProps {
    readonly amount?: string;
    readonly subgraphEndpoint?: string;
}

// NOTE: It is essential to pass in a Deployer into the contracts for initialization
// This is because when we are testing the emit, the passed in contract expects a
// provider and will throw an error if this doesn't exist.
export const setup = async (props: ISetupProps) => {
    const deployer = await deployContractsAndToken();
    const framework = await deployer.getFramework();

    const [Deployer, Alpha, Bravo, Charlie] = await ethers.getSigners();
    if (!Deployer || !Alpha || !Bravo || !Charlie) {
        throw new Error("Empty signer not allowed!");
    }
    const signers = [Deployer, Alpha, Bravo, Charlie];

    const provider = Deployer.provider;
    if (!provider) {
        throw new Error("No provider");
    }
    const chainId = (await provider.getNetwork()).chainId;

    const frameworkClass = await Framework.create({
        chainId,
        resolverAddress: framework.resolver,
        provider,
        customSubgraphQueriesEndpoint: props.subgraphEndpoint,
        protocolReleaseVersion: "test",
    });
    const CFAV1 = frameworkClass.contracts.cfaV1.connect(
        Deployer
    ) as IConstantFlowAgreementV1;
    const IDAV1 = frameworkClass.contracts.idaV1.connect(
        Deployer
    ) as IInstantDistributionAgreementV1;
    const superTokenClass = (await frameworkClass.loadSuperToken(
        "fDAIx"
    )) as WrapperSuperToken;
    let SuperToken = superTokenClass.contract;
    const Token = new ethers.Contract(
        superTokenClass.underlyingToken.address,
        TestTokenABI,
        Deployer
    ) as unknown as TestToken;
    if (props.amount) {
        const initialAmount = ethers.utils.parseUnits(props.amount);
        for (let i = 0; i < signers.length; i++) {
            const signer = signers[i]!;
            await Token.connect(signer).mint(signer.address, initialAmount, {
                from: signer.address,
            });
            await Token.connect(signer).approve(
                SuperToken.address,
                initialAmount,
                {
                    from: signer.address,
                }
            );
            await SuperToken.connect(signer).upgrade(initialAmount, {
                from: signer.address,
            });
        }
    }
    SuperToken = superTokenClass.contract.connect(Deployer);

    return {
        CFAV1,
        IDAV1,
        frameworkClass,
        Deployer,
        Alpha,
        Bravo,
        Charlie,
        SuperToken,
        Token,
        SignerCount: signers.length,
        ResolverAddress: framework.resolver,
    };
};
