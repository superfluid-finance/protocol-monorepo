import { ethers } from "hardhat";
import { abi as TestTokenABI } from "../src/abi/TestToken.json";
import { abi as IResolverABI } from "../src/abi/IResolver.json";
import { abi as SuperTokenABI } from "../src/abi/SuperToken.json";
import { abi as IConstantFlowAgreementV1ABI } from "../src/abi/IConstantFlowAgreementV1.json";
import { abi as IInstantDistributionAgreementV1ABI } from "../src/abi/IInstantDistributionAgreementV1.json";
import {
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    IResolver,
    SuperToken,
    TestToken,
} from "../src/typechain";
import { Framework } from "../src";

// NOTE: This assumes you are testing with the generic hardhat mnemonic as the deployer:
// test test test test test test test test test test test junk
export const RESOLVER_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

interface ISetupProps {
    readonly amount?: string;
    readonly subgraphEndpoint?: string;
}

export const setup = async (props: ISetupProps) => {
    const [Deployer, Alpha] = await ethers.getSigners();
    if (!Deployer || !Alpha) {
        throw new Error("No deployer");
    }
    const signers = [Deployer, Alpha];

    const provider = Deployer.provider;
    if (!provider) {
        throw new Error("No provider");
    }
    const resolver = new ethers.Contract(
        RESOLVER_ADDRESS,
        IResolverABI,
        Deployer
    ) as IResolver;
    const superTokenAddress = await resolver.get("supertokens.test.fDAIx");
    const SuperToken = new ethers.Contract(
        superTokenAddress,
        SuperTokenABI
    ) as SuperToken;
    const underlyingToken = await SuperToken.connect(
        Deployer
    ).getUnderlyingToken();
    const Token = new ethers.Contract(
        underlyingToken,
        TestTokenABI
    ) as TestToken;
    const frameworkClass = await Framework.create({
        networkName: "custom",
        resolverAddress: RESOLVER_ADDRESS,
        provider,
        customSubgraphQueriesEndpoint: props.subgraphEndpoint,
        protocolReleaseVersion: "test",
    });
    const CFAV1 = new ethers.Contract(
        frameworkClass.settings.config.cfaV1Address,
        IConstantFlowAgreementV1ABI,
        Deployer
    ) as IConstantFlowAgreementV1;
    const IDAV1 = new ethers.Contract(
        frameworkClass.settings.config.idaV1Address,
        IInstantDistributionAgreementV1ABI,
        Deployer
    ) as IInstantDistributionAgreementV1;
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

    return {
        CFAV1,
        IDAV1,
        frameworkClass,
        Deployer,
        Alpha,
        SuperToken,
        Token,
    };
};
