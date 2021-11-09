import { expect } from "chai";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Framework } from "../src/index";
import { SuperToken } from "../src/typechain";
import { RESOLVER_ADDRESS, setup } from "./setup";

describe.only("CFA V1 Tests", () => {
    let deployer: SignerWithAddress;
    let superToken: SuperToken;
    let framework: Framework;

    before(async () => {
        const { frameworkClass, Deployer, SuperToken } = await setup({
            amount: "100000",
            fakeSubgraphEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
        });
        framework = frameworkClass;
        deployer = Deployer;
        superToken = SuperToken;
        console.log("superToken.address", superToken.address);
    });

    it("Should throw an error if no networkName or chainId.", async () => {
        try {
            await Framework.create({ provider: deployer.provider! });
        } catch (err: any) {
            expect(err.message).to.equal(
                "Framework Initialization Error - You must input chainId or networkName."
            );
        }
    });

    it("Should throw an error if network and chainId don't match.", async () => {
        try {
            // NOTE: as any to get this to compile to test no provider initialization (as if this was JS)
            await Framework.create({
                networkName: "matic",
                chainId: 4,
                provider: deployer.provider!
            });
        } catch (err: any) {
            expect(err.message).to.equal(
                "Framework Initialization Error - The network name and chainId you have selected don't match."
            );
        }
    });

    it("Should throw an error if no provider.", async () => {
        try {
            // NOTE: as any to get this to compile to test no provider initialization (as if this was JS)
            await Framework.create({
                networkName: "matic"
            } as any);
        } catch (err: any) {
            expect(err.message).to.equal(
                "Framework Initialization Error - You must pass in a provider when initializing the framework."
            );
        }
    });

    it("Should throw an error if loadFramework fails.", async () => {
        try {
            await Framework.create({
                networkName: "custom",
                provider: deployer.provider!,
                customSubgraphQueriesEndpoint:
                    "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
                resolverAddress: "0x0000000000000000000000000000000000000000",
                protocolReleaseVersion: "test",
            });
        } catch (err: any) {
            expect(err.message).to.contain(
                "Framework Initialization Error - There was an error initializing the framework"
            );
        }
    });

    it("Should throw an error if subgraph endpoint is null and WEB3_ONLY isn't selected.", async () => {
        try {
            await Framework.create({
                networkName: "custom",
                provider: deployer.provider!,
                resolverAddress: RESOLVER_ADDRESS,
                protocolReleaseVersion: "test",
            });
        } catch (err: any) {
            expect(err.message).to.equal(
                "Framework Initialization Error - You cannot have a null subgaphQueriesEndpoint if you haven't selected 'WEB3_ONLY' as your dataMode."
            );
        }
    });

    it("Should catch error when creating a signer if minimum isn't passed in.", () => {
        try {
            framework.createSigner({});
        } catch (err: any) {
            expect(err.message).to.equal(
                "Create Signer Error - You must pass in a private key, provider or signer."
            );
        }
    });

    it("Should catch error when creating a signer with PK, but no provider.", () => {
        try {
            framework.createSigner({
                privateKey: process.env.TEST_ACCOUNT_PRIVATE_KEY,
            });
        } catch (err: any) {
            expect(err.message).to.equal(
                "Create Signer Error - You must pass in a provider with your private key."
            );
        }
    });

    it("Should be able to create a signer successfully with all different inputs.", () => {});

    it("Should be able to create an empty batch call with framework.", () => {});

    it("Should be able to create an instance of a supertoken with framework.", () => {});
});
