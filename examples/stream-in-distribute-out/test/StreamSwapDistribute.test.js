const { ethers } = require("hardhat");
const { assert } = require("chai");
const { Framework } = require("@superfluid-finance/sdk-core");
const deploySuperfluid = require("./util/deploy-sf.js");
const createSuperToken = require("./util/create-supertoken.js");

const ten = ethers.utils.parseEther("10").toString();
const flowRate = ethers.utils.parseEther("0.000001").toString();
const updatedFlowRate = ethers.utils.parseEther("0.000002").toString();
const indexId = "0";
const overrides = { gasLimit: 3000000 }; // Using this to manually limit gas to avoid giga-errors.

// Deploying signer
let admin,
    // Alice signer
    alice,
    // Bob signer
    bob,
    // Superfluid sdk-core framework instance
    sf,
    // Underlying ERC20 of `inToken`
    inUnderlyingToken,
    // Underlying ERC20 of `outToken`
    outUnderlyingToken,
    // Super token to stream in
    inToken,
    // Super token to distribute out
    outToken,
    // Stream swap distribute super app
    streamSwapDistributeApp,
    // Uniswap router mock
    routerMock;

before(async function () {
    [admin, alice, bob] = await ethers.getSigners();

    const resolverAddress = await deploySuperfluid(admin);

    sf = await Framework.create({
        provider: admin.provider,
        resolverAddress,
        dataMode: "WEB3_ONLY",
        protocolReleaseVersion: "test",
        networkName: "custom",
    });
});

beforeEach(async function () {
    // deploy tokens
    const ERC20MockFactory = await ethers.getContractFactory(
        "ERC20Mock",
        admin
    );

    inUnderlyingToken = await ERC20MockFactory.deploy("In Token", "ITn");
    outUnderlyingToken = await ERC20MockFactory.deploy("Out Token", "OTn");

    inToken = await createSuperToken(
        inUnderlyingToken.address,
        "Super In Token",
        "ITnx",
        sf,
        admin
    );
    outToken = await createSuperToken(
        outUnderlyingToken.address,
        "Super Out Token",
        "OTnx",
        sf,
        admin
    );

    // mint to alice and bob
    await inUnderlyingToken.connect(alice).mint(alice.address, ten);
    await inUnderlyingToken.connect(alice).approve(inToken.address, ten);
    await inToken.connect(alice).upgrade(ten);

    await inUnderlyingToken.connect(bob).mint(bob.address, ten);
    await inUnderlyingToken.connect(bob).approve(inToken.address, ten);
    await inToken.connect(bob).upgrade(ten);

    const routerFactory = await ethers.getContractFactory(
        "UniswapRouterMock",
        admin
    );

    routerMock = await routerFactory.deploy();

    const appFactory = await ethers.getContractFactory(
        "StreamSwapDistribute",
        admin
    );

    streamSwapDistributeApp = await appFactory.deploy(
        sf.settings.config.hostAddress,
        sf.settings.config.cfaV1Address,
        sf.settings.config.idaV1Address,
        inToken.address,
        outToken.address,
        routerMock.address
    );
});

describe("Streaming Operations", async function () {
    it("Can create flow to super app", async function () {
        await sf.cfaV1
            .createFlow({
                superToken: inToken.address,
                flowRate,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        assert.equal(
            (
                await sf.cfaV1.getFlow({
                    superToken: inToken.address,
                    sender: alice.address,
                    receiver: streamSwapDistributeApp.address,
                    providerOrSigner: admin,
                })
            ).flowRate,
            flowRate
        );

        assert.equal(
            (
                await sf.idaV1.getSubscription({
                    indexId,
                    superToken: outToken.address,
                    publisher: streamSwapDistributeApp.address,
                    subscriber: alice.address,
                    providerOrSigner: admin.provider,
                })
            ).units,
            flowRate
        );
    });

    it("Can update flow to super app", async function () {
        await sf.cfaV1
            .createFlow({
                superToken: inToken.address,
                flowRate,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        await sf.cfaV1
            .updateFlow({
                superToken: inToken.address,
                flowRate: updatedFlowRate,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        assert.equal(
            (
                await sf.cfaV1.getFlow({
                    superToken: inToken.address,
                    sender: alice.address,
                    receiver: streamSwapDistributeApp.address,
                    providerOrSigner: admin,
                })
            ).flowRate,
            updatedFlowRate
        );

        assert.equal(
            (
                await sf.idaV1.getSubscription({
                    indexId,
                    superToken: outToken.address,
                    publisher: streamSwapDistributeApp.address,
                    subscriber: alice.address,
                    providerOrSigner: admin.provider,
                })
            ).units,
            updatedFlowRate
        );
    });

    it("Can delete flow to super app", async function () {
        await sf.cfaV1
            .createFlow({
                superToken: inToken.address,
                flowRate,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        await sf.cfaV1
            .deleteFlow({
                superToken: inToken.address,
                sender: alice.address,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        assert.equal(
            (
                await sf.cfaV1.getFlow({
                    superToken: inToken.address,
                    sender: alice.address,
                    receiver: streamSwapDistributeApp.address,
                    providerOrSigner: admin,
                })
            ).flowRate,
            "0"
        );

        assert.equal(
            (
                await sf.idaV1.getSubscription({
                    indexId,
                    superToken: outToken.address,
                    publisher: streamSwapDistributeApp.address,
                    subscriber: alice.address,
                    providerOrSigner: admin.provider,
                })
            ).units,
            "0"
        );
    });
});

describe("IDA Operations", async function () {
    it("Can approve subscription to super app", async function () {
        await sf.cfaV1
            .createFlow({
                superToken: inToken.address,
                flowRate,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        await sf.idaV1.approveSubscription({
            indexId,
            superToken: outToken.address,
            publisher: streamSwapDistributeApp.address,
            overrides
        })
        .exec(alice);
    });
});

describe("Action operations", async () => {
    // this also asserts the `createFlow` from the first streamer won't throw.
    it("Can execute action with zero units", async function () {
        await streamSwapDistributeApp.connect(alice).executeAction();
        assert(true);
    });

    it("Can execute action after flow created", async function () {
        await sf.cfaV1
            .createFlow({
                superToken: inToken.address,
                flowRate,
                receiver: streamSwapDistributeApp.address,
                overrides,
            })
            .exec(alice);

        await sf.idaV1.approveSubscription({
            indexId,
            superToken: outToken.address,
            publisher: streamSwapDistributeApp.address,
            overrides,
        }).exec(alice);

        await streamSwapDistributeApp.connect(alice).executeAction();

        assert.notEqual(
            (await outToken.balanceOf(alice.address)).toString(),
            "0"
        );
    });
});
