const Proxy = artifacts.require("Proxy");
const SuperToken = artifacts.require("SuperToken");
const ISuperToken = artifacts.require("ISuperToken");
const TestToken = artifacts.require("TestToken");
const TestGovernance = artifacts.require("TestGovernance");
const ConstantFlowAgreementV1 = artifacts.require("ConstantFlowAgreementV1");
const InstantDistributionAgreementV1 = artifacts.require("InstantDistributionAgreementV1");
const Superfluid = artifacts.require("Superfluid");

const {
    web3tx,
    toWad,
    wad4human,
    toBN,
} = require("@decentral.ee/web3-helpers");

const AUM_DUST_AMOUNT = toBN(10000);

module.exports = class Tester {

    constructor(accounts) {
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
        };
    }

    printAliases() {
        console.log("Aliases", this.aliases);
    }

    async resetContracts() {
        this.contracts = {};

        // test token contract
        this.contracts.token = await web3tx(TestToken.new, "TestToken.new")(
            "Test Token", "TEST",
            {
                from: this.aliases.admin
            });

        // superfluid host contract
        const superfluidLogic = await web3tx(Superfluid.new, "Superfluid.new")(
            {
                from: this.aliases.admin
            });
        const superfluidProxy = await web3tx(Proxy.new, "Create superfluid proxy contract")(
            {
                from: this.aliases.admin
            }
        );
        await web3tx(superfluidProxy.initializeProxy, "superfluidProxy.initializeProxy")(
            superfluidLogic.address, {
                from: this.aliases.admin
            }
        );
        this.contracts.superfluid = await Superfluid.at(superfluidProxy.address);

        // governance contract
        this.contracts.governance = await web3tx(TestGovernance.new, "TestGovernance.new")(
            this.aliases.admin /* rewardAddress */,
            this.constants.LIQUIDATION_PERIOD /* liquidationReriod */,
            {
                from: this.aliases.admin
            });

        await web3tx(this.contracts.superfluid.initialize, "superfluid.initialize")();
        await web3tx(this.contracts.superfluid.setGovernance, "superfluid.setGovernance")(
            this.contracts.governance.address
        );

        // super token logic contract
        const superTokenLogic = await web3tx(SuperToken.new, "Create super token logic contract")();
        await web3tx(this.contracts.superfluid.setSuperTokenLogic, "superfluid.setSuperTokenLogic")(
            superTokenLogic.address
        );

        // create super token
        this.contracts.superfluid.createERC20Wrapper(
            this.contracts.token.address,
            18,
            "Super Test Token",
            "TESTx",
        );
        this.contracts.superToken = await ISuperToken.at(
            (await this.contracts.superfluid.getERC20Wrapper.call(
                this.contracts.token.address,
                "TESTx"
            )).wrapperAddress
        );

        // CFA contract
        this.contracts.cfa = await web3tx(ConstantFlowAgreementV1.new, "ConstantFlowAgreementV1.new")(
            {
                from: this.aliases.admin
            });

        // IDA contract
        this.contracts.ida = await web3tx(InstantDistributionAgreementV1.new, "InstantDistributionAgreementV1.new")(
            {
                from: this.aliases.admin
            });

        // Add agreements to whiteList
        if (!(await this.contracts.governance.isAgreementListed.call(this.contracts.cfa.address))) {
            await web3tx(this.contracts.governance.addAgreement, "Governance lists CFA")(
                this.contracts.cfa.address
            );
        }
        if (!(await this.contracts.governance.isAgreementListed.call(this.contracts.ida.address))) {
            await web3tx(this.contracts.governance.addAgreement, "Governance lists IDA")(
                this.contracts.ida.address
            );
        }

        // mint test tokens to test accounts
        await Promise.all(Object.keys(this.aliases).map(async alias => {
            const userAddress = this.aliases[alias];
            await web3tx(this.contracts.token.mint, `Mint token for ${alias}`)(
                userAddress,
                this.constants.INIT_BALANCE, {
                    from: userAddress
                }
            );
            await web3tx(this.contracts.token.approve, `TestToken.approve - from ${alias} to SuperToken`)(
                this.contracts.superToken.address,
                this.constants.MAX_UINT256, {
                    from: userAddress
                }
            );
        }));
    }

    async validateSystem() {
        console.log("======== System validation report Begin ========");

        const currentBlock = await web3.eth.getBlock("latest");

        let rtBalanceSum = toBN(0);
        await Promise.all(Object.keys(this.aliases).map(async alias => {
            const userAddress = this.aliases[alias];
            const tokenBalance = await this.contracts.token.balanceOf.call(userAddress);
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

        const aum = await this.contracts.token.balanceOf.call(this.contracts.superToken.address);
        console.log(`AUM of super tokens: ${wad4human(aum)}`);

        const totalSupply = await this.contracts.superToken.totalSupply.call();
        console.log(`Total real-time blances of super tokens: ${wad4human(rtBalanceSum)}`);

        console.log(`Total supply of super tokens: ${wad4human(totalSupply)}`);
        console.log("======== System Validation Report End ========");

        assert.isTrue(aum.add(AUM_DUST_AMOUNT).gte(rtBalanceSum),
            "AUM should be equal or more than the real-time balance sum");
        assert.isTrue(aum.sub(rtBalanceSum).lte(AUM_DUST_AMOUNT),
            "AUM minus the real-time balance sum should only be a dust amount");
        assert.equal(wad4human(aum, 8), wad4human(rtBalanceSum, 8),
            "AUM should match the real-time balance sum to at least 8 decimals during testing");
        assert.equal(aum.toString(), totalSupply.toString(),
            "Total supply should be equal to the AUM");
    }

};
