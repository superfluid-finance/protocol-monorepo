const { expectRevert } = require("@openzeppelin/test-helpers");
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("../src");

contract("Framework class", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 1), { isTruffle: true });
    const { admin } = t.aliases;

    before(async () => {
        await t.reset();
        await deployTestToken(t.errorHandler, [":", "fDAI"], {
            isTruffle: true
        });
        await deployTestToken(t.errorHandler, [":", "fUSDC"], {
            isTruffle: true
        });
        await deploySuperToken(t.errorHandler, [":", "fDAI"], {
            isTruffle: true
        });
        await deploySuperToken(t.errorHandler, [":", "fUSDC"], {
            isTruffle: true
        });
    });

    describe("initialization", () => {
        function testLoadedContracts(sf) {
            const {
                IERC20,
                IResolver,
                TokenInfo,
                ISuperfluid,
                ISuperToken,
                ISuperTokenFactory,
                IConstantFlowAgreementV1,
                IInstantDistributionAgreementV1
            } = sf.contracts;

            assert.isDefined(IERC20.abi);
            assert.equal(IERC20.contractName, "IERC20");
            assert.isTrue(
                IERC20.abi.filter(i => i.name === "Transfer").length > 0
            );

            assert.isDefined(IResolver.abi);
            assert.equal(IResolver.contractName, "IResolver");
            assert.isTrue(
                IResolver.abi.filter(i => i.name === "get").length > 0
            );

            assert.isDefined(TokenInfo.abi);
            assert.equal(TokenInfo.contractName, "TokenInfo");
            assert.isTrue(
                TokenInfo.abi.filter(i => i.name === "symbol").length > 0
            );

            assert.isDefined(ISuperfluid.abi);
            assert.equal(ISuperfluid.contractName, "ISuperfluid");
            assert.isTrue(
                ISuperfluid.abi.filter(i => i.name === "callAgreement").length >
                    0
            );

            assert.isDefined(ISuperToken.abi);
            assert.equal(ISuperToken.contractName, "ISuperToken");
            assert.isTrue(
                ISuperToken.abi.filter(i => i.name === "upgrade").length > 0
            );

            assert.isDefined(ISuperTokenFactory.abi);
            assert.equal(ISuperTokenFactory.contractName, "ISuperTokenFactory");
            assert.isTrue(
                ISuperTokenFactory.abi.filter(
                    i => i.name === "createERC20Wrapper"
                ).length > 0
            );

            assert.isDefined(IConstantFlowAgreementV1.abi);
            assert.equal(
                IConstantFlowAgreementV1.contractName,
                "IConstantFlowAgreementV1"
            );
            assert.isTrue(
                IConstantFlowAgreementV1.abi.filter(
                    i => i.name === "updateFlow"
                ).length > 0
            );

            assert.isDefined(IInstantDistributionAgreementV1.abi);
            assert.equal(
                IInstantDistributionAgreementV1.contractName,
                "IInstantDistributionAgreementV1"
            );
            assert.isTrue(
                IInstantDistributionAgreementV1.abi.filter(
                    i => i.name === "createIndex"
                ).length > 0
            );
        }

        it("without truffle framework", async () => {
            const sf = new SuperfluidSDK.Framework({
                web3Provider: web3.currentProvider
            });
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
                    web3Provider: web3.currentProvider,
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
                    web3Provider: web3.currentProvider,
                    tokens: ["fML"]
                });
                await expectRevert(
                    sf.initialize(),
                    "Token fML is not registered"
                );
            });

            it("failed due to no super token wrapper", async () => {
                await deployTestToken(t.errorHandler, [":", "SASHIMI"]);
                const sf = new SuperfluidSDK.Framework({
                    web3Provider: web3.currentProvider,
                    tokens: ["SASHIMI"]
                });
                await expectRevert(
                    sf.initialize(),
                    "Token SASHIMI doesn't have a super token wrapper"
                );
            });
        });
    });

    describe("createERC20Wrapper", () => {
        let sf;

        beforeEach(async () => {
            sf = new SuperfluidSDK.Framework({
                web3Provider: web3.currentProvider
            });
            await sf.initialize();
        });

        it("create new super token", async () => {
            await deployTestToken(t.errorHandler, [":", "MISO"]);
            const misoAddress = await sf.resolver.get("tokens.MISO");
            const misoToken = await sf.contracts.TokenInfo.at(misoAddress);
            const superMisoToken = await sf.createERC20Wrapper(misoToken, {
                from: admin
            });
            assert.equal(
                await superMisoToken.getUnderlyingToken.call(),
                misoAddress
            );
        });
    });
});
