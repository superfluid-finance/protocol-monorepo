import {ContractReceipt} from "ethers";
import {ethers} from "hardhat";

import {
    SuperTokenLibraryGDAMock,
    SuperTokenMock,
} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";

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
            event?.topics[2] || ethers.constants.AddressZero
        );
    };

    let alice: string;
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
        const mockFactory = await ethers.getContractFactory(
            "SuperTokenLibraryGDAMock"
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
        console.log({poolAddress});
        const poolContract = await ethers.getContractAt(
            "SuperfluidPool",
            poolAddress
        );
        const code = await ethers.provider.getCode(poolContract.address);
        console.log({code});
    });
});
