import chai, { expect } from "chai";
import { ethers, network } from "hardhat";
import { chaiEthers } from "chai-ethers";
import TestTokenABI from "@superfluid-finance/ethereum-contracts/build/contracts/TestToken.json";
import {
    Framework,
    SuperToken,
    WrapperSuperToken,
} from "@superfluid-finance/sdk-core";
import { OpsMock } from "../typechain-types/src/mocks/OpsMock";
import { SimpleACLCloseResolver } from "../typechain-types/src/SimpleACLCloseResolver";
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { Provider } from "@ethersproject/providers";
import { TestToken } from "@superfluid-finance/sdk-core/dist/module/typechain";
import { deployFrameworkAndTokens } from "../scripts/deployFrameworkAndTokens";

// use chaiEthers for things like expectRevertedWith/expectRevert
// and .to.emit(ContractObject, "EventName").withArgs(evArg1, evArg2, ...)
chai.use(chaiEthers);

// NOTE: This assumes you are testing with the generic hardhat mnemonic as the deployer:
// test test test test test test test test test test test junk
export const RESOLVER_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

describe("SimpleACLCloseResolver", () => {
    let SuperfluidFramework: Framework;
    let SuperToken: SuperToken;

    let OpsMock: OpsMock;
    let Token: TestToken;
    let SimpleACLCloseResolver: SimpleACLCloseResolver;

    let Sender: SignerWithAddress;
    let Receiver: SignerWithAddress;
    let Misc: SignerWithAddress;

    let EndTime: string;
    let EVMSnapshotId: string;

    const _getCurrentBlockTime = async () => {
        const currentBlockNumber = await ethers.provider.getBlockNumber();
        const currentBlock = await ethers.provider.getBlock(currentBlockNumber);
        return currentBlock.timestamp;
    };

    const _takeSnapshotAndSetId = async () => {
        const snapshotId = await network.provider.send("evm_snapshot");
        EVMSnapshotId = snapshotId;
    };
    const _revertToSnapshot = async (snapshotId: string) => {
        await network.provider.send("evm_revert", [snapshotId]);
    };
    const _useLastEVMSnapshot = async () => {
        await _revertToSnapshot(EVMSnapshotId);
        await _takeSnapshotAndSetId();
    };

    before(async () => {
        await deployFrameworkAndTokens();
        const signers = await ethers.getSigners();
        Sender = signers[0];
        Receiver = signers[1];
        Misc = signers[2];
        const provider = Sender.provider!;
        const chainId = (await provider.getNetwork()).chainId;

        const frameworkClass = await Framework.create({
            chainId,
            resolverAddress: RESOLVER_ADDRESS,
            provider: provider as unknown as Provider,
            dataMode: "WEB3_ONLY",
            protocolReleaseVersion: "test",
        });

        const superToken = (await frameworkClass.loadSuperToken(
            "fDAIx",
        )) as WrapperSuperToken;
        const token = new ethers.Contract(
            superToken.underlyingToken.address,
            TestTokenABI.abi,
            Sender,
        ) as unknown as TestToken;

        const initialAmount = ethers.utils.parseUnits("10000");
        console.log("Minting and Upgrading Tokens...");
        for (let i = 0; i < signers.length; i++) {
            const signer = signers[i]!;
            await token.connect(signer).mint(signer.address, initialAmount, {
                from: signer.address,
            });
            await token
                .connect(signer)
                .approve(superToken.address, initialAmount, {
                    from: signer.address,
                });

            const upgradeOp = superToken.upgrade({
                amount: initialAmount.toString(),
            });
            await upgradeOp.exec(signer);
        }
        const opsMockFactory = await ethers.getContractFactory("OpsMock");
        const simpleACLCloseResolverFactory = await ethers.getContractFactory(
            "SimpleACLCloseResolver",
        );
        const currentBlockTime = await _getCurrentBlockTime();
        // end time is 5 minutes in the future
        EndTime = (currentBlockTime + 5 * 60).toString();
        const simpleACLCloseResolver =
            await simpleACLCloseResolverFactory.deploy(
                EndTime,
                frameworkClass.settings.config.cfaV1Address,
                superToken.address,
                Sender.address,
                Receiver.address,
            );
        const opsMock = await opsMockFactory.deploy(
            (simpleACLCloseResolver as any).address,
            frameworkClass.settings.config.hostAddress,
        );
        SuperfluidFramework = frameworkClass;
        OpsMock = opsMock;
        Token = token;
        SuperToken = superToken;
        SimpleACLCloseResolver = simpleACLCloseResolver;

        // take a snapshot after initial setup
        await _takeSnapshotAndSetId();
    });

    beforeEach(async () => {
        await _useLastEVMSnapshot();
    });

    describe("Revert Cases", () => {
        describe("Ops Tests", () => {
            it("Should revert when attempting to execute early without approval", async () => {
                const createFlowOp = SuperfluidFramework.cfaV1.createFlow({
                    flowRate: "1",
                    superToken: SuperToken.address,
                    receiver: Receiver.address,
                });
                await createFlowOp.exec(Sender);
                await expect(OpsMock.connect(Misc).exec()).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith(
                //     "CannotExecute()",
                // );
            });

            it("Should revert when attempting to execute at time without approval", async () => {
                const createFlowOp = SuperfluidFramework.cfaV1.createFlow({
                    flowRate: "1",
                    superToken: SuperToken.address,
                    receiver: Receiver.address,
                });
                await createFlowOp.exec(Sender);
                // increase time to end time
                await network.provider.send("evm_setNextBlockTimestamp", [
                    Number(EndTime),
                ]);
                await expect(OpsMock.connect(Misc).exec()).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // (
                //     "FailedExecution()",
                // );
            });

            it("Should revert when attempting to execute before end time with approval", async () => {
                const createFlowOp = SuperfluidFramework.cfaV1.createFlow({
                    flowRate: "1",
                    superToken: SuperToken.address,
                    receiver: Receiver.address,
                });
                await createFlowOp.exec(Sender);
                const authorizeOp =
                    SuperfluidFramework.cfaV1.updateFlowOperatorPermissions({
                        superToken: SuperToken.address,
                        flowOperator: OpsMock.address,
                        permissions: 4,
                        flowRateAllowance: "0",
                    });
                await authorizeOp.exec(Sender);

                await expect(OpsMock.connect(Misc).exec()).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith(
                //     "CannotExecute()",
                // );
            });

            it("Should revert when attempting to close non-existent flow", async () => {
                const authorizeOp =
                    SuperfluidFramework.cfaV1.updateFlowOperatorPermissions({
                        superToken: SuperToken.address,
                        flowOperator: OpsMock.address,
                        permissions: 4,
                        flowRateAllowance: "0",
                    });
                await authorizeOp.exec(Sender);
                await network.provider.send("evm_setNextBlockTimestamp", [
                    Number(EndTime),
                ]);

                await expect(OpsMock.connect(Misc).exec()).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith(
                //     "CannotExecute()",
                // );
            });
        });
        describe("Resolver Tests", () => {
            it("Should revert if update flow receiver to invalid receiver", async () => {
                await expect(
                    SimpleACLCloseResolver.connect(Sender).updateFlowReceiver(
                        ethers.constants.AddressZero,
                    ),
                ).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith("InvalidFlowReceiver()");

                await expect(
                    SimpleACLCloseResolver.connect(Sender).updateFlowReceiver(
                        Sender.address,
                    ),
                ).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith("InvalidFlowReceiver()");
            });

            it("Should revert if update flow sender to invalid sender", async () => {
                await expect(
                    SimpleACLCloseResolver.connect(Sender).updateFlowSender(
                        ethers.constants.AddressZero,
                    ),
                ).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith("InvalidFlowSender()");

                await expect(
                    SimpleACLCloseResolver.connect(Sender).updateFlowSender(
                        Receiver.address,
                    ),
                ).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith("InvalidFlowSender()");
            });

            it("Should revert if update end time to earlier", async () => {
                await expect(
                    SimpleACLCloseResolver.connect(Sender).updateEndTime(0),
                ).to.be.reverted;
                // custom errors aren't being caught in CI build for examples
                // .revertedWith("InvalidEndTime()");
            });

            it("Should revert if not owner update flow sender", async () => {
                await expect(
                    SimpleACLCloseResolver.connect(Receiver).updateFlowSender(
                        Misc.address,
                    ),
                ).to.be.revertedWith("Ownable: caller is not the owner");
            });

            it("Should revert if not owner update flow receiver", async () => {
                await expect(
                    SimpleACLCloseResolver.connect(Receiver).updateFlowReceiver(
                        Misc.address,
                    ),
                ).to.be.revertedWith("Ownable: caller is not the owner");
            });

            it("Should revert if not owner update end time", async () => {
                await expect(
                    SimpleACLCloseResolver.connect(Receiver).updateEndTime(
                        EndTime,
                    ),
                ).to.be.revertedWith("Ownable: caller is not the owner");
            });
        });
    });

    describe("Passing Cases", () => {
        describe("Ops Tests", () => {
            it("Should be able to close after end time", async () => {
                const createFlowOp = SuperfluidFramework.cfaV1.createFlow({
                    flowRate: "1",
                    superToken: SuperToken.address,
                    receiver: Receiver.address,
                });
                await createFlowOp.exec(Sender);

                const authorizeOp =
                    SuperfluidFramework.cfaV1.updateFlowOperatorPermissions({
                        superToken: SuperToken.address,
                        flowOperator: OpsMock.address,
                        permissions: 4,
                        flowRateAllowance: "0",
                    });
                await authorizeOp.exec(Sender);

                await network.provider.send("evm_setNextBlockTimestamp", [
                    Number(EndTime),
                ]);

                await OpsMock.connect(Misc).exec();
            });
        });

        describe("Resolver Tests", () => {
            it("Should be able to update end time", async () => {
                await SimpleACLCloseResolver.connect(Sender).updateEndTime(
                    Number(EndTime) + 1,
                );
            });
            it("Should be able to update flow receiver", async () => {
                await SimpleACLCloseResolver.connect(Sender).updateFlowReceiver(
                    Misc.address,
                );
            });
            it("Should be able to update flow sender", async () => {
                await SimpleACLCloseResolver.connect(Sender).updateFlowSender(
                    Misc.address,
                );
            });
        });
    });
});
