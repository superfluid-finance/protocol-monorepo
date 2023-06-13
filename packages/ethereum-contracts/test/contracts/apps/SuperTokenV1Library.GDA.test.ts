import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
import {ContractReceipt} from "ethers";
import {ethers, expect} from "hardhat";

import {
    SuperfluidPool,
    SuperTokenLibraryGDAMock,
    SuperTokenMock,
} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";
import {toBN} from "../utils/helpers";

describe("SuperTokenV1Library.GDA", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();
    // const {FLOW_RATE1} = t.configs;

    const getPoolAddressFromReceipt = (receipt: ContractReceipt) => {
        const POOL_CREATED_TOPIC = ethers.utils.solidityKeccak256(
            ["string"],
            ["PoolCreated(address,address,address)"]
        );
        const event = receipt.events?.find((x) =>
            x.topics.includes(POOL_CREATED_TOPIC)
        );
        return ethers.utils.hexStripZeros(
            event ? event.data : ethers.constants.AddressZero
        );
    };

    let alice: string, bob: string;
    let superTokenLibraryGDAMock: SuperTokenLibraryGDAMock;
    let superToken: SuperTokenMock;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        ({alice, bob} = t.aliases);
        superToken = t.tokens.SuperToken;
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
        const superTokenV1LibraryFactory = await ethers.getContractFactory(
            "SuperTokenV1Library"
        );
        const superTokenV1Library = await superTokenV1LibraryFactory.deploy();
        const mockFactory = await ethers.getContractFactory(
            "SuperTokenLibraryGDAMock",
            {
                libraries: {
                    SuperTokenV1Library: superTokenV1Library.address,
                },
            }
        );
        superTokenLibraryGDAMock = await mockFactory.deploy();

        t.beforeEachTestCaseBenchmark(this);
    });

    this.afterEach(() => {
        t.afterEachTestCaseBenchmark();
    });

    it("#1.1 Should be able to create pool", async () => {
        const createPoolTxn = await superTokenLibraryGDAMock.createPoolTest(
            superToken.address,
            alice
        );
        const receipt = await createPoolTxn.wait();
        const poolAddress = getPoolAddressFromReceipt(receipt);
        const poolContract = await ethers.getContractAt(
            "SuperfluidPool",
            poolAddress
        );
        expect(await poolContract.admin()).to.equal(alice);
        expect(await poolContract.superToken()).to.equal(superToken.address);
    });
    context("With a pool", () => {
        let pool: SuperfluidPool;
        let admin: SignerWithAddress;

        beforeEach(async () => {
            admin = await ethers.getSigner(alice);
            const createPoolTxn = await superTokenLibraryGDAMock.createPoolTest(
                superToken.address,
                admin.address
            );
            const receipt = await createPoolTxn.wait();
            const poolAddress = getPoolAddressFromReceipt(receipt);
            pool = await ethers.getContractAt("SuperfluidPool", poolAddress);

            // transfer tokens to the mock gda contract
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            await superToken
                .connect(admin)
                .transfer(
                    superTokenLibraryGDAMock.address,
                    t.configs.INIT_BALANCE
                );
        });

        it("#1.2 Should be able to distribute to pool", async () => {
            expect(await pool.getUnits(bob)).to.equal("0");
            await pool.connect(admin).updateMember(bob, "10");
            expect(await pool.getUnits(bob)).to.equal("10");
            const bobBalanceBefore = await superToken.balanceOf(bob);
            const requestedDistributionAmount = "100";
            const estimatedDistributionActualAmount =
                await superTokenLibraryGDAMock.estimateDistributionActualAmountTest(
                    superToken.address,
                    superTokenLibraryGDAMock.address,
                    pool.address,
                    requestedDistributionAmount
                );
            await superTokenLibraryGDAMock.distributeToPoolTest(
                superToken.address,
                superTokenLibraryGDAMock.address,
                pool.address,
                requestedDistributionAmount
            );
            const bobBalanceAfter = await superToken.balanceOf(bob);
            expect(
                bobBalanceAfter,
                bobBalanceBefore
                    .add(toBN(requestedDistributionAmount))
                    .toString()
            );
            expect(
                estimatedDistributionActualAmount,
                requestedDistributionAmount
            );
        });

        it("#1.3 Should be able to distribute flow to pool", async () => {
            expect(await pool.getUnits(bob)).to.equal("0");
            await pool.connect(admin).updateMember(bob, "10");
            expect(await pool.getUnits(bob)).to.equal("10");
            const requestedFlowRate = "99";
            const estimatedFlowDistributionActualFlowRate =
                await superTokenLibraryGDAMock.estimateFlowDistributionActualFlowRateTest(
                    superToken.address,
                    superTokenLibraryGDAMock.address,
                    pool.address,
                    requestedFlowRate
                );
            await superTokenLibraryGDAMock.distributeFlowTest(
                superToken.address,
                superTokenLibraryGDAMock.address,
                pool.address,
                requestedFlowRate
            );
            const flowDistributionFlowRate =
                await superTokenLibraryGDAMock.getFlowDistributionFlowRateTest(
                    superToken.address,
                    superTokenLibraryGDAMock.address,
                    pool.address
                );
            expect(
                flowDistributionFlowRate.toString(),
                estimatedFlowDistributionActualFlowRate.actualFlowRate.toString()
            );
        });

        it("#1.4 Should be able to connect to a pool", async () => {
            await superTokenLibraryGDAMock.connectPoolTest(
                superToken.address,
                pool.address
            );
            expect(
                await superTokenLibraryGDAMock.isMemberConnectedTest(
                    superToken.address,
                    pool.address,
                    superTokenLibraryGDAMock.address
                )
            ).to.equal(true);
        });

        it("#1.4 Should be able to disconnect from a pool", async () => {
            await superTokenLibraryGDAMock.connectPoolTest(
                superToken.address,
                pool.address
            );
            expect(
                await superTokenLibraryGDAMock.isMemberConnectedTest(
                    superToken.address,
                    pool.address,
                    superTokenLibraryGDAMock.address
                )
            ).to.equal(true);
            await superTokenLibraryGDAMock.disconnectPoolTest(
                superToken.address,
                pool.address
            );
            expect(
                await superTokenLibraryGDAMock.isMemberConnectedTest(
                    superToken.address,
                    pool.address,
                    superTokenLibraryGDAMock.address
                )
            ).to.equal(false);
        });
    });
});
