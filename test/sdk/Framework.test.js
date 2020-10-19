const { assert } = require("chai");
const {
    expectRevert
} = require("@openzeppelin/test-helpers");
const TestEnvironment = require("../TestEnvironment");
const deployTestToken = require("../../scripts/deploy-test-token");
const deploySuperToken = require("../../scripts/deploy-super-token");
const SuperfluidSDK = require("../..");


contract("Framework class", () => {

    const t = new TestEnvironment([]);

    before(async () => {
        await t.reset();
        await deployTestToken(t.errorHandler, [":", "fDAI"]);
        await deployTestToken(t.errorHandler, [":", "fUSDC"]);
        await deploySuperToken(t.errorHandler, [":", "fDAI"]);
        await deploySuperToken(t.errorHandler, [":", "fUSDC"]);
    });

    describe("initialization", () => {
        function testLoadedContracts(sf) {
            const {
                IERC20,
                IResolver,
                TokenInfo,
                ISuperfluid,
                ISuperToken,
                IConstantFlowAgreementV1,
                IInstantDistributionAgreementV1,
            } = sf.contracts;

            assert.isDefined(IERC20.abi);
            assert.equal(IERC20.contractName, "IERC20");
            assert.isTrue(IERC20.abi.filter(i => i.name === "Transfer").length > 0);

            assert.isDefined(IResolver.abi);
            assert.equal(IResolver.contractName, "IResolver");
            assert.isTrue(IResolver.abi.filter(i => i.name === "get").length > 0);

            assert.isDefined(TokenInfo.abi);
            assert.equal(TokenInfo.contractName, "TokenInfo");
            assert.isTrue(TokenInfo.abi.filter(i => i.name === "symbol").length > 0);

            assert.isDefined(ISuperfluid.abi);
            assert.equal(ISuperfluid.contractName, "ISuperfluid");
            assert.isTrue(ISuperfluid.abi.filter(i => i.name === "getERC20Wrapper").length > 0);

            assert.isDefined(ISuperToken.abi);
            assert.equal(ISuperToken.contractName, "ISuperToken");
            assert.isTrue(ISuperToken.abi.filter(i => i.name === "upgrade").length > 0);

            assert.isDefined(IConstantFlowAgreementV1.abi);
            assert.equal(IConstantFlowAgreementV1.contractName, "IConstantFlowAgreementV1");
            assert.isTrue(IConstantFlowAgreementV1.abi.filter(i => i.name === "updateFlow").length > 0);

            assert.isDefined(IInstantDistributionAgreementV1.abi);
            assert.equal(IInstantDistributionAgreementV1.contractName, "IInstantDistributionAgreementV1");
            assert.isTrue(IInstantDistributionAgreementV1.abi.filter(i => i.name === "createIndex").length > 0);
        }

        it("without truffle framework", async () => {
            const sf = new SuperfluidSDK.Framework({ web3Provider: web3.currentProvider });
            await sf.initialize();
            testLoadedContracts(sf);
        });

        it("with truffle framework", async () => {
            const sf = new SuperfluidSDK.Framework({ isTruffle: true });
            await sf.initialize();
            testLoadedContracts(sf);
        });

        describe("and load tokens", () => {
            it("registered in resolver", async () => {
                const sf = new SuperfluidSDK.Framework({
                    isTruffle: true,
                    tokens: ["fUSDC", "fDAI"]
                });
                await sf.initialize();
                assert.equal(await sf.tokens.fUSDC.symbol(), "fUSDC");
                assert.equal(await sf.tokens.fDAI.symbol(), "fDAI");
                assert.equal(await sf.tokens.fUSDCx.symbol(), "fUSDCx");
                assert.equal(await sf.tokens.fDAIx.symbol(), "fDAIx");
            });

            it("failed due to unregistered in resolver", async () => {
                const sf = new SuperfluidSDK.Framework({
                    isTruffle: true,
                    tokens: ["fML"]
                });
                await expectRevert(sf.initialize(), "Token fML is not registered");
            });

            it("failed due to no super token wrapper", async () => {
                await deployTestToken(t.errorHandler, [":", "SASHIMI"]);
                const sf = new SuperfluidSDK.Framework({
                    isTruffle: true,
                    tokens: ["SASHIMI"]
                });
                await expectRevert(sf.initialize(), "Token SASHIMI doesn't have a super token wrapper");
            });
        });
    });

    describe("token registry", () => {
        let sf;

        beforeEach(async () => {
            sf = new SuperfluidSDK.Framework({
                isTruffle: true,
                tokens: ["fUSDC"]
            });
            await sf.initialize();
        });

        describe("getERC20Wrapper", () => {
            it("by token info", async () => {
                const fUSDCAddress = await sf.resolver.get("tokens.fUSDC");
                const wrapper = await sf.getERC20Wrapper(await sf.contracts.TokenInfo.at(fUSDCAddress));
                assert.isTrue(wrapper.created);
                assert.equal(wrapper.wrapperAddress, sf.tokens.fUSDCx.address);
            });
            it("by token address", async () => {
                const fUSDCAddress = await sf.resolver.get("tokens.fUSDC");
                const wrapper = await sf.getERC20Wrapper(fUSDCAddress);
                assert.isTrue(wrapper.created);
                assert.equal(wrapper.wrapperAddress, sf.tokens.fUSDCx.address);
            });
        });

        describe("createERC20Wrapper", () => {
            it("over new underlying token", async () => {
                await deployTestToken(t.errorHandler, [":", "MISO"]);
                const misoAddress = await sf.resolver.get("tokens.MISO");
                const misoToken = await sf.contracts.TokenInfo.at(misoAddress);
                await sf.createERC20Wrapper(misoToken);
                const wrapper = await sf.getERC20Wrapper(misoToken);
                assert.isTrue(wrapper.created);
            });

            it("failed on existing token", async () => {
                await expectRevert(
                    sf.createERC20Wrapper(sf.tokens.fUSDC),
                    "SF: createERC20Wrapper wrapper exist");
            });
        });

    });

});
