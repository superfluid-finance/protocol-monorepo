const deployFramework = require("../scripts/deploy-framework");
const ISuperfluid = artifacts.require("ISuperfluid");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const TestToken = artifacts.require("TestToken");
const IConstantFlowAgreementV1 = artifacts.require("IConstantFlowAgreementV1");
const IInstantDistributionAgreementV1 = artifacts.require("IInstantDistributionAgreementV1");

const {
    web3tx,
    toWad,
    wad4human,
    toBN,
} = require("@decentral.ee/web3-helpers");


module.exports = class Tester {

    constructor(accounts) {
        process.env.USE_MOCKS = 1;
        this.aliases = {
            admin: accounts[0],
            alice: accounts[1],
            bob: accounts[2],
            carol: accounts[3],
            dan: accounts[4],
            eve: accounts[5],
            frank: accounts[6],
            grace: accounts[7],
            heidi: accounts[8],
            ivan: accounts[9],
        };
        // delete undefined accounts
        Object.keys(this.aliases).forEach(alias => {
            if (!this.aliases[alias]) delete this.aliases[alias];
        });
        this.toAliases = Object.keys(this.aliases).reduce((acc, alias) => {
            acc[this.aliases[alias]] = alias;
            return acc;
        }, {});

        this.constants = {
            MAX_UINT256: "115792089237316195423570985008687907853269984665640564039457584007913129639935",
            INIT_BALANCE: toWad(100),
            ZERO_ADDRESS: "0x0000000000000000000000000000000000000000",
            LIQUIDATION_PERIOD: 2,
            DUST_AMOUNT: toBN(10000),
            AUM_DUST_AMOUNT: toBN(10000),
        };
    }

    errorHandler(err) {
        if (err) throw err;
    }

    async reset() {
        console.log("Aliases", this.aliases);

        // deploy framework
        delete process.env.TEST_RESOLVER_ADDRESS;
        await deployFramework(this.errorHandler);

        this.contracts = {};
        // load host contract
        const superfluid = this.contracts.superfluid = await ISuperfluid.at(process.env.TEST_SUPERFLUID_ADDRESS);
        // load agreement contracts
        const cfav1Type = web3.utils.sha3("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
        const idav1Type = web3.utils.sha3("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");
        const cfaAddress = await superfluid.getAgreementClass.call(cfav1Type);
        const idaAddress = await superfluid.getAgreementClass.call(idav1Type);
        this.contracts.cfa = await IConstantFlowAgreementV1.at(cfaAddress);
        this.contracts.ida = await IInstantDistributionAgreementV1.at(idaAddress);
    }

    async createNewToken({ doUpgrade } = {}) {
        // test token contract
        this.contracts.testToken = await web3tx(TestToken.new, "TestToken.new")(
            "Test Token", "TEST");

        // create super token
        await web3tx(this.contracts.superfluid.createERC20Wrapper, "Creating wrapper for the new TestToken")(
            this.contracts.testToken.address,
            18,
            "Super Test Token",
            "TESTx");

        this.contracts.superToken = await SuperTokenMock.at(
            (await this.contracts.superfluid.getERC20Wrapper.call(
                this.contracts.testToken.address,
                "TESTx"
            )).wrapperAddress
        );

        // mint test tokens to test accounts
        await Promise.all(Object.keys(this.aliases).map(async alias => {
            const userAddress = this.aliases[alias];
            await web3tx(this.contracts.testToken.mint, `Mint token for ${alias}`)(
                userAddress,
                this.constants.INIT_BALANCE, {
                    from: userAddress
                }
            );
            await web3tx(this.contracts.testToken.approve, `TestToken.approve by ${alias} to SuperToken`)(
                this.contracts.superToken.address,
                this.constants.MAX_UINT256, {
                    from: userAddress
                }
            );
            if (doUpgrade) {
                await web3tx(this.contracts.superToken.upgrade, `Upgrade token for ${alias}`)(
                    this.constants.INIT_BALANCE, {
                        from: userAddress
                    }
                );
            }
        }));
    }

    async validateSystem() {
        console.log("======== System validation report Begin ========");

        const currentBlock = await web3.eth.getBlock("latest");

        let rtBalanceSum = toBN(0);
        await Promise.all(Object.keys(this.aliases).map(async alias => {
            const userAddress = this.aliases[alias];
            const tokenBalance = await this.contracts.testToken.balanceOf.call(userAddress);
            const balances = await this.contracts.superToken.realtimeBalanceOf.call(
                userAddress,
                currentBlock.timestamp);
            // Available Balance = Realtime Balance - Deposit + Min(Deposit, Owed Deposit)
            const realtimeBalance = balances.availableBalance
                .add(balances.deposit)
                .sub(web3.utils.BN.min(balances.owedDeposit, balances.deposit));

            console.log(`${alias} underlying token balance: ${wad4human(tokenBalance)}`);
            console.log(`${alias} super token available balance: ${wad4human(balances.availableBalance)}`);
            console.log(`${alias} super token deposit: ${wad4human(balances.deposit)}`);
            console.log(`${alias} super token real-time balance: ${wad4human(realtimeBalance)}`);

            rtBalanceSum = rtBalanceSum.add(realtimeBalance);
        }));

        const aum = await this.contracts.testToken.balanceOf.call(this.contracts.superToken.address);
        console.log(`AUM of super tokens: ${wad4human(aum)}`);

        const totalSupply = await this.contracts.superToken.totalSupply.call();
        console.log(`Total real-time blances of super tokens: ${wad4human(rtBalanceSum)}`);

        console.log(`Total supply of super tokens: ${wad4human(totalSupply)}`);
        console.log("======== System Validation Report End ========");

        assert.isTrue(aum.add(this.constants.AUM_DUST_AMOUNT).gte(rtBalanceSum),
            "AUM should be equal or more than the real-time balance sum");
        assert.isTrue(aum.sub(rtBalanceSum).lte(this.constants.AUM_DUST_AMOUNT),
            "AUM minus the real-time balance sum should only be a dust amount");
        assert.equal(wad4human(aum, 8), wad4human(rtBalanceSum, 8),
            "AUM should match the real-time balance sum to at least 8 decimals during testing");
        assert.equal(aum.toString(), totalSupply.toString(),
            "Total supply should be equal to the AUM");
    }

};
