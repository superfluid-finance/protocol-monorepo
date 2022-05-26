const { ethers } = require("hardhat");
const { assert } = require("chai");
const { Framework } = require("@superfluid-finance/sdk-core");
const { deploySuperfluid } = require("./util/deploy-sf.js");
const ISuperTokenFactory = require("@superfluid-finance/ethereum-contracts/build/contracts/ISuperTokenFactory.json");
const ISuperToken = require("@superfluid-finance/ethereum-contracts/build/contracts/ISuperToken.json");

const one = ethers.utils.parseEther("1").toString();
const ten = ethers.utils.parseEther("10").toString();
const flowRate = ethers.utils.parseEther("0.000001").toString();
const indexId = "0";
const overrides = { gasLimit: 3000000 }; // Using this to manually limit gas to avoid giga-errors.

const getCreatedSuperToken = async (tx) =>
    (await tx.wait()).events.find((e) => e.event === "SuperTokenCreated")
        .args[0];

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

    const superTokenFactoryAddress = await sf.host.hostContract
        .connect(admin)
        .getSuperTokenFactory();

    const superTokenFactory = new ethers.Contract(
        superTokenFactoryAddress,
        ISuperTokenFactory.abi
    );

    // IF YOU'RE READING THIS, you're either reviewing this or you're looking to add this to your
    // project. If you're the latter, avoid this pattern, as this is a hack and I have a deadline :)
    // `await superTokenFactory.createERC20Wrapper(...args)` is best.
    const inSuperTokenCreated = await superTokenFactory
        .connect(admin)
        .functions["createERC20Wrapper(address,uint8,string,string)"](
            inUnderlyingToken.address,
            1,
            "Super In Token",
            "ITnx"
        );

    inTokenAddress = await getCreatedSuperToken(inSuperTokenCreated);

    const outSuperTokenCreated = await superTokenFactory
        .connect(admin)
        .functions["createERC20Wrapper(address,uint8,string,string)"](
            outUnderlyingToken.address,
            1,
            "Super Out Token",
            "OTnx"
        );

    outTokenAddress = await getCreatedSuperToken(outSuperTokenCreated);

    inToken = new ethers.Contract(inTokenAddress, ISuperToken.abi, admin);
    outToken = new ethers.Contract(outTokenAddress, ISuperToken.abi, admin);

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
    it("Execute action does not throw if no units", async function () {
        const beforeValue = (
            await sf.idaV1.getIndex({
                superToken: outToken.address,
                publisher: streamSwapDistributeApp.address,
                indexId,
                providerOrSigner: admin,
            })
        ).indexValue;

        await streamSwapDistributeApp.connect(alice).executeAction();

        const afterValue = (
            await sf.idaV1.getIndex({
                superToken: outToken.address,
                publisher: streamSwapDistributeApp.address,
                indexId,
                providerOrSigner: admin,
            })
        ).indexValue;

        assert.equal(beforeValue, afterValue);
    });

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

        const params = {
            indexId,
            superToken: outToken.address,
            publisher: streamSwapDistributeApp.address,
            subscriber: alice.address,
            providerOrSigner: admin.provider,
        }
        const sub = await sf.idaV1.getSubscription(params);
        console.log(sub)
    });
});
