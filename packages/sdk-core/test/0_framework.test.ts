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
        const { frameworkClass, Deployer, SuperToken } = await setup();
        framework = frameworkClass;
        deployer = Deployer;
        superToken = SuperToken;
        console.log(superToken.address);
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
            console.log("err.message", err.message);
            expect(err.message).to.contain(
                "Framework Initialization Error - There was an error initializing the framework:"
            );
        }
    });

    it("Should catch error when creating a signer if minimum isn't passed in.", () => {});
    it("Should be able to create a signer successfully with all different inputs.", () => {});
    it("Should be able to create an empty batch call with framework.", () => {});
    it("Should be able to create an instance of a supertoken with framework.", () => {});
});
