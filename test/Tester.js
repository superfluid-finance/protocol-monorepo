const Proxy = artifacts.require("Proxy");
const SuperToken = artifacts.require("SuperToken");
const TestToken = artifacts.require("TestToken");
const TestGovernance = artifacts.require("TestGovernance");
const FlowAgreement = artifacts.require("FlowAgreement");
const Superfluid = artifacts.require("Superfluid");

const {
    web3tx,
    toWad,
    wad4human,
    toBN
} = require("@decentral.ee/web3-helpers");

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
        };
    }

    printAliases() {
        console.log("Aliases", this.aliases);
    }

    async resetContracts() {
        this.contracts = {};

        this.contracts.token = await web3tx(TestToken.new, "TestToken.new")(
            "Test Token", "TEST",
            {
                from: this.aliases.admin
            });

        this.contracts.superfluid = await web3tx(Superfluid.new, "SuperToken.new")(
            {
                from: this.aliases.admin
            });

        this.contracts.governance = await web3tx(TestGovernance.new, "TestGovernance.new")(
            this.aliases.admin,
            1,
            3600,
            10000,
            10000,
            this.contracts.superfluid.address,
            {
                from: this.aliases.admin
            });

        const superTokenLogic = await web3tx(SuperToken.new, "Create super token logic contract")();
        const proxy = await web3tx(Proxy.new, "Create super token proxy contract")(
            {
                from: this.aliases.admin
            }
        );
        await web3tx(proxy.initializeProxy, "proxy.initializeProxy")(
            superTokenLogic.address, {
                from: this.aliases.admin
            }
        );
        this.contracts.superToken = await SuperToken.at(proxy.address);
        await web3tx(this.contracts.superToken.initialize, "superToken.initialize")(
            "SuperTestToken",
            "STT",
            18,
            this.contracts.token.address,
            this.contracts.governance.address, {
                from: this.aliases.admin
            }
        );

        this.contracts.flowAgreement = await web3tx(FlowAgreement.new, "FlowAgreement.new")(
            {
                from: this.aliases.admin
            });

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
            const superTokenBalance = await this.contracts.superToken.realtimeBalanceOf.call(
                userAddress,
                currentBlock.timestamp);

            console.log(`${alias} token balance: ${wad4human(tokenBalance)}`);
            console.log(`${alias} super token Balance: ${wad4human(superTokenBalance)}`);
            rtBalanceSum = rtBalanceSum.add(superTokenBalance);
        }));


        const aum = await this.contracts.token.balanceOf.call(this.contracts.superToken.address);
        console.log(`AUM of super tokens: ${wad4human(aum)}`);

        const totalSupply = await this.contracts.superToken.totalSupply.call();
        console.log(`Total real-time blances of super tokens: ${wad4human(rtBalanceSum)}`);

        console.log(`Total supply of super tokens: ${wad4human(totalSupply)}`);
        console.log("======== System Validation Report End ========");

        assert.isTrue(aum.gte(rtBalanceSum),
            "AUM should be equal or more than the real-time balance sum");
        assert.equal(wad4human(aum, 8), wad4human(rtBalanceSum, 8),
            "AUM should match the real-time balance sum to at least 8 decimals during testing");
        assert.equal(aum.toString(), totalSupply.toString(),
            "Total supply should be equal to the AUM");
    }

};
