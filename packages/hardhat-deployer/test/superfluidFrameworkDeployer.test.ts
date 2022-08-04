// tslint:disable-next-line no-implicit-dependencies
import { assert } from "chai";
import * as ethers from "ethers";
import Resolver from "@superfluid-finance/ethereum-contracts/build/contracts/Resolver.json";
import Superfluid from "@superfluid-finance/ethereum-contracts/build/contracts/Superfluid.json";
import { Framework } from "@superfluid-finance/sdk-core";

import { SuperfluidFrameworkDeployer } from "../src/SuperfluidFrameworkDeployer";
import { ERC1820_ADDRESS } from "../src/ERC1820Constants";
import { useEnvironment } from "./helpers";

// Environment Variables
// (Anvil generated key)
const privKey =
    "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80";
const httpUrl = "http://localhost:8545";

const cfaAgreementType = "0xa9214cc96615e0085d3bb077758db69497dc2dce3b2b1e97bc93c3d18d83efd3";
const idaAgreementType = "0x8aedc3b5d4bf031e11a7e2940f7251c005698405d58e02e1c247fed3b1b3a674";

describe("Integration Tests", async function () {
    describe("Hardhat Superfluid Runtime Extension", function () {
        useEnvironment("hardhat-project");

        it("Should inject SuperfluidFrameworkDeployer into HRE", function () {
            assert.instanceOf(this.hre.superfluidFrameworkDeployer, SuperfluidFrameworkDeployer);
        });

        it("Should create SDK-Core Framework", async function () {
            const { superfluidFrameworkDeployer } = this.hre

            this.deployer = new ethers.Wallet(
                privKey,
                new ethers.providers.JsonRpcProvider(httpUrl)
            );

            const resolverAddress = await superfluidFrameworkDeployer.deploy(this.deployer);

            await Framework.create({
                provider: this.deployer.provider,
                resolverAddress,
                protocolReleaseVersion: "test",
                chainId: 31337
            });
        })
    });
});

describe("Unit Tests", async function () {
    describe("SuperfluidFrameworkDeployer", async function () {
        before(function () {
            // simply set up a wallet
            this.deployer = new ethers.Wallet(
                privKey,
                new ethers.providers.JsonRpcProvider(httpUrl)
            );
            this.superfluidFrameworkDeployer = new SuperfluidFrameworkDeployer();
        });

        it("Should begin with all zero addresses", function () {
            const { address, resolverAddress } = this.superfluidFrameworkDeployer;

            assert.equal(address, ethers.constants.AddressZero);
            assert.equal(resolverAddress, ethers.constants.AddressZero);
        });

        it("Should throw when deployer does not contain provider", async function () {
            try {
                const deployer = { provider: null };
                await this.superfluidFrameworkDeployer.deploy(deployer as any);
            } catch (error) {
                assert.equal(
                    (error as any).message,
                    "No provider is defined in the `Signer` object."
                );
            }
        });

        it("Should throw on wrapping if framework is not deployed", async function () {
            try {
                await this.superfluidFrameworkDeployer
                    .deployWrapperSuperToken("Test Token", "TEST", this.deployer);
            } catch (error) {
                assert.equal(
                    (error as any).message,
                    "Super Token Factory is Zero Address.\n\`superfluidFrameworkDeployer.deploy(deployerSigner)\` must be called first."
                );
            }
        });

        it("Should deploy all Superfluid contracts", async function () {
            const resolverAddress = await this.superfluidFrameworkDeployer.deploy(this.deployer);

            assert.equal(resolverAddress, this.superfluidFrameworkDeployer.resolverAddress);
            assert.notEqual(resolverAddress, ethers.constants.AddressZero);

            const resolver = new ethers.Contract(resolverAddress, Resolver.abi, this.deployer);

            assert.notEqual(await this.deployer.provider.getCode(ERC1820_ADDRESS), "0x");
            assert.notEqual(
                await resolver.get("TestGovernance.test"),
                ethers.constants.AddressZero
            );

            const hostAddress = await resolver.get("Superfluid.test");

            assert.notEqual(hostAddress, ethers.constants.AddressZero);

            const host = new ethers.Contract(hostAddress, Superfluid.abi, this.deployer);

            assert.notEqual(
                await host.getAgreementClass(cfaAgreementType),
                ethers.constants.AddressZero
            );
            assert.notEqual(
                await host.getAgreementClass(idaAgreementType),
                ethers.constants.AddressZero
            );
            assert.notEqual(await host.getSuperTokenFactory(), ethers.constants.AddressZero);
            assert.notEqual(await host.getGovernance(), ethers.constants.AddressZero);
        });

        it("Should deploy a mock wrapper super token", async function () {
            await this.superfluidFrameworkDeployer.deploy(this.deployer);

            const { underlyingToken, superToken } =
                await this.superfluidFrameworkDeployer.deployWrapperSuperToken(
                    "Test Token",
                    "TEST",
                    this.deployer
                );

            assert.notEqual(underlyingToken.address, ethers.constants.AddressZero);
            assert.notEqual(superToken.address, ethers.constants.AddressZero);
            assert.equal(await superToken.getUnderlyingToken(), underlyingToken.address);
        });
    });
});
