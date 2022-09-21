import { expect } from "chai";
import { ethers } from "ethers";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { SuperToken } from "../src/typechain";
import { networkNameToChainIdMap } from "../src/constants";
import { HARDHAT_PRIVATE_KEY, setup } from "../scripts/setup";
import hre from "hardhat";

export const ROPSTEN_SUBGRAPH_ENDPOINT =
    "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten";

describe("Framework Tests", async () => {
    let evmSnapshotId: string;
    let deployer: SignerWithAddress;
    let alpha: SignerWithAddress;
    let superToken: SuperToken;
    let framework: Framework;
    let INFURA_API_URL = "https://polygon-rpc.com/";
    let customProvider = new ethers.providers.JsonRpcProvider(
        INFURA_API_URL,
        "matic"
    );

    before(async () => {
        const { frameworkClass, Deployer, SuperToken, Alpha } = await setup({
            amount: "10000000000",
            subgraphEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
        });
        framework = frameworkClass;
        deployer = Deployer;
        alpha = Alpha;
        superToken = SuperToken;
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    beforeEach(async () => {
        await hre.network.provider.send("evm_revert", [evmSnapshotId]);
        evmSnapshotId = await hre.network.provider.send("evm_snapshot");
    });

    describe("Validate Framework Constructor Options Tests", async () => {
        it("Should throw an error if no networkName or chainId", async () => {
            try {
                await Framework.create({
                    provider: deployer.provider!,
                    chainId: null as any,
                });
            } catch (err: any) {
                expect(err.name).to.equal("SFError");
                expect(err.message).to.equal(
                    "Framework Initialization Error: You must input chainId."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should throw an error if your provider network and selected chainId/networkName don't match", async () => {
            const chainId = (await deployer.provider!.getNetwork()).chainId;
            try {
                await Framework.create({
                    // force cast because we know this exists
                    chainId: networkNameToChainIdMap.get("eth-goerli")!,
                    provider: deployer.provider!,
                });
            } catch (err: any) {
                expect(err.name).to.equal("SFError");
                expect(err.message).to.equal(
                    "Network Mismatch Error: Your provider network chainId is: " +
                        chainId +
                        " whereas your desired chainId is: " +
                        networkNameToChainIdMap.get("eth-goerli")!
                );
            }
        });

        it("Should throw an error if no provider, injected web3 or injected hardhat ethers provider", async () => {
            try {
                // NOTE: as any to get this to throw an error when test no provider initialization (as if this was JS)
                await Framework.create({
                    // force cast because we know this exists
                    chainId: networkNameToChainIdMap.get("polygon-mainnet")!,
                } as any);
            } catch (err: any) {
                expect(err.name).to.equal("SFError");
                expect(err.message).to.equal(
                    "Framework Initialization Error: You must pass in a provider, an injected web3.js or ethers.js instance when initializing the framework."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should throw an error if resolver address is null on unsupported network", async () => {
            try {
                const chainId = (await deployer.provider!.getNetwork()).chainId;
                await Framework.create({
                    chainId,
                    provider: deployer.provider!,
                    customSubgraphQueriesEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
                    protocolReleaseVersion: "test",
                });
            } catch (err: any) {
                expect(err.name).to.equal("SFError");
                expect(err.message).to.equal(
                    "Framework Initialization Error: You must input your own resolver address if you use an unsupported network."
                );
                expect(err.cause).to.be.undefined;
            }
        });
    });

    describe("Framework.create Tests", () => {
        it("Should throw an error if loadFramework fails", async () => {
            try {
                const chainId = (await deployer.provider!.getNetwork()).chainId;
                await Framework.create({
                    chainId,
                    provider: deployer.provider!,
                    customSubgraphQueriesEndpoint: ROPSTEN_SUBGRAPH_ENDPOINT,
                    resolverAddress:
                        "0x0000000000000000000000000000000000000000",
                    protocolReleaseVersion: "test",
                });
            } catch (err: any) {
                expect(err.name).to.equal("SFError");
                expect(err.message).to.contain(
                    "Framework Initialization Error: There was an error initializing the framework"
                );
                expect(err.cause).to.be.instanceOf(Error);
            }
        });

        it("Should be able to create a framework with chain id only", async () => {
            await Framework.create({
                chainId: networkNameToChainIdMap.get("polygon-mainnet")!,
                provider: customProvider,
            });
        });

        it("Should be able to create a framework with web3.js via ethers.providers.Web3Provider", async () => {
            const provider = new ethers.providers.Web3Provider(
                (global as any).web3.currentProvider
            );
            const chainId = (await deployer.provider!.getNetwork()).chainId;
            await Framework.create({
                chainId,
                provider,
                resolverAddress: process.env.RESOLVER_ADDRESS,
                protocolReleaseVersion: "test",
            });
        });

        it("Should be able to create a framework with injected web3", async () => {
            const chainId = (await deployer.provider!.getNetwork()).chainId;
            await Framework.create({
                chainId,
                provider: (global as any).web3,
                resolverAddress: process.env.RESOLVER_ADDRESS,
                protocolReleaseVersion: "test",
            });
        });

        it("Should be able to create a framework with injected hardhat ethers", async () => {
            const chainId = hre.network.config.chainId!;
            await Framework.create({
                chainId,
                provider: hre.ethers,
                resolverAddress: process.env.RESOLVER_ADDRESS,
                protocolReleaseVersion: "test",
            });
        });
    });

    describe("Framework Function Tests", () => {
        it("Should catch error when creating a signer if minimum isn't passed in.", () => {
            try {
                framework.createSigner({});
            } catch (err: any) {
                expect(err.message).to.equal(
                    "Create Signer Error: You must pass in a private key, provider or signer."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should catch error when creating a signer with PK, but no provider.", () => {
            try {
                framework.createSigner({
                    privateKey: HARDHAT_PRIVATE_KEY,
                });
            } catch (err: any) {
                expect(err.message).to.equal(
                    "Create Signer Error: You must pass in a provider with your private key."
                );
                expect(err.cause).to.be.undefined;
            }
        });

        it("Should be able to create a signer successfully with all different inputs.", () => {
            // create signer with private key
            framework.createSigner({
                privateKey: HARDHAT_PRIVATE_KEY,
                provider: deployer.provider!,
            });

            // create signer directly
            framework.createSigner({
                signer: deployer,
            });
        });

        it("Should be able to create an empty batch call with framework.", () => {
            framework.batchCall([]);
        });

        it("Should be able to create an instance of a supertoken (with address) with framework.", async () => {
            const daix = await framework.loadSuperToken(superToken.address);
            expect(daix.settings.address).to.equal(superToken.address);
        });

        it("Should be able to create an instance of a supertoken (with token symbol) with framework.", async () => {
            const tokenName = await superToken.symbol();
            const daix = await framework.loadSuperToken(tokenName);
            expect(daix.settings.address).to.equal(superToken.address);
        });

        it("Should be able to use contract object", async () => {
            const flowData = await framework.contracts.cfaV1
                .connect(deployer)
                .getFlow(superToken.address, deployer.address, alpha.address);
            expect(flowData.timestamp).to.eq("0");
            expect(flowData.flowRate).to.eq("0");
            expect(flowData.deposit).to.eq("0");
            expect(flowData.owedDeposit).to.eq("0");
        });
    });
});
