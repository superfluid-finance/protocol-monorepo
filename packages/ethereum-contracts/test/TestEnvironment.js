const _ = require("lodash");
const deployFramework = require("../scripts/deploy-framework");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const IERC1820Registry = artifacts.require("IERC1820Registry");
const SuperfluidMock = artifacts.require("SuperfluidMock");
const ConstantFlowAgreementV1 = artifacts.require("ConstantFlowAgreementV1");
const InstantDistributionAgreementV1 = artifacts.require(
    "InstantDistributionAgreementV1"
);
const TestGovernance = artifacts.require("TestGovernance");
const TestToken = artifacts.require("TestToken");
const SuperTokenMock = artifacts.require("SuperTokenMock");

const { BN } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    toWad,
    wad4human,
    toBN
} = require("@decentral.ee/web3-helpers");

module.exports = class TestEnvironment {
    constructor(accounts, { isTruffle, useMocks } = {}) {
        this.useMocks = useMocks;
        this.isTruffle = isTruffle;
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
            ivan: accounts[9]
        };
        // delete undefined accounts
        Object.keys(this.aliases).forEach(alias => {
            if (!this.aliases[alias]) delete this.aliases[alias];
        });

        this.configs = {
            INIT_BALANCE: toWad(100),
            AUM_DUST_AMOUNT: toBN(0),
            LIQUIDATION_PERIOD: 3600
        };

        this.constants = Object.assign(
            {},
            require("@openzeppelin/test-helpers").constants
        );
    }

    errorHandler(err) {
        if (err) throw err;
    }

    /**************************************************************************
     * Test case setup functions
     *************************************************************************/

    /// reset the system
    async reset(deployOpts = {}) {
        console.log("Aliases", this.aliases);

        // deploy framework
        await deployFramework(this.errorHandler, {
            newTestResolver: true,
            useMocks: this.useMocks,
            isTruffle: this.isTruffle,
            ...deployOpts
        });

        // load the SDK
        this.sf = new SuperfluidSDK.Framework({
            isTruffle: this.isTruffle
        });
        await this.sf.initialize();

        // re-loading contracts with testing/mocking interfaces
        this.contracts = {};
        // load singletons
        this.contracts.erc1820 = await IERC1820Registry.at(
            "0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24"
        );
        // load host contract
        this.contracts.superfluid = await SuperfluidMock.at(
            this.sf.host.address
        );
        // load agreement contracts
        this.contracts.cfa = await ConstantFlowAgreementV1.at(
            this.sf.agreements.cfa.address
        );
        this.contracts.ida = await InstantDistributionAgreementV1.at(
            this.sf.agreements.ida.address
        );
        // load governance contract
        this.contracts.governance = await TestGovernance.at(
            await this.sf.host.getGovernance()
        );

        await this.resetForTestCase();
    }

    /// reset function for each test case
    async resetForTestCase() {
        // test data can be persisted here
        this.data = {};

        await web3tx(
            this.contracts.governance.setLiquidationPeriod,
            "reset liquidation period"
        )(this.configs.LIQUIDATION_PERIOD);
        await web3tx(
            this.contracts.governance.setRewardAddress,
            "reset reward address to admin"
        )(this.aliases.admin);
    }

    /// create a new test token
    async createNewToken({ doUpgrade } = {}) {
        // test token contract
        this.contracts.testToken = await web3tx(TestToken.new, "TestToken.new")(
            "Test Token",
            "TEST",
            18
        );

        this.contracts.superToken = await SuperTokenMock.at(
            (await this.sf.createERC20Wrapper(this.contracts.testToken)).address
        );

        // mint test tokens to test accounts
        await Promise.all(
            Object.keys(this.aliases).map(async alias => {
                const userAddress = this.aliases[alias];
                await web3tx(
                    this.contracts.testToken.approve,
                    `TestToken.approve by ${alias} to SuperToken`
                )(
                    this.contracts.superToken.address,
                    this.constants.MAX_UINT256,
                    {
                        from: userAddress
                    }
                );
                await web3tx(
                    this.contracts.testToken.mint,
                    `Mint token for ${alias}`
                )(userAddress, this.configs.INIT_BALANCE, {
                    from: userAddress
                });
                if (doUpgrade) {
                    await web3tx(
                        this.contracts.superToken.upgrade,
                        `Upgrade token for ${alias}`
                    )(this.configs.INIT_BALANCE, {
                        from: userAddress
                    });
                }
            })
        );
    }

    /**************************************************************************
     * Alias functions
     *************************************************************************/

    listAliases() {
        if (!("moreAliases" in this.data)) this.data.moreAliases = {};
        return Object.keys(this.aliases).concat(
            Object.keys(this.data.moreAliases)
        );
    }

    listAddresses() {
        if (!("moreAliases" in this.data)) this.data.moreAliases = {};
        return Object.values(this.aliases).concat(
            Object.values(this.data.moreAliases)
        );
    }

    addAlias(alias, address) {
        if (!("moreAliases" in this.data)) this.data.moreAliases = {};
        this.data.moreAliases = _.merge(this.data.moreAliases, {
            [alias]: address
        });
    }

    toAlias(address) {
        return this.listAliases().find(
            i => this.getAddress(i).toLowerCase() === address.toLowerCase()
        );
    }

    getAddress(alias) {
        if (!("moreAliases" in this.data)) this.data.moreAliases = {};
        return this.aliases[alias] || this.data.moreAliases[alias];
    }

    /**************************************************************************
     * Test data functions
     *************************************************************************/

    async upgradeBalance(alias, amount) {
        const account = this.getAddress(alias);
        await web3tx(this.contracts.testToken.mint, `Mint token for ${alias}`)(
            account,
            this.configs.INIT_BALANCE,
            {
                from: account
            }
        );
        await web3tx(
            this.contracts.superToken.upgrade,
            `Upgrade ${amount.toString()} for account ${alias}`
        )(amount, {
            from: account
        });
        this.updateAccountBalanceSnapshot(
            this.contracts.superToken.address,
            account,
            await this.contracts.superToken.realtimeBalanceOfNow(account)
        );
    }

    async transferBalance(from, to, amount) {
        const fromAccount = this.getAddress(from);
        const toAccount = this.getAddress(to);
        await this.contracts.superToken.transfer(toAccount, amount, {
            from: fromAccount
        });
        this.updateAccountBalanceSnapshot(
            this.contracts.superToken.address,
            toAccount,
            await this.contracts.superToken.realtimeBalanceOfNow(toAccount)
        );
        this.updateAccountBalanceSnapshot(
            this.contracts.superToken.address,
            fromAccount,
            await this.contracts.superToken.realtimeBalanceOfNow(fromAccount)
        );
    }

    /**************************************************************************
     * Test data functions
     *************************************************************************/

    updateAccountBalanceSnapshot(superToken, account, balanceSnapshot) {
        assert.isDefined(account);
        assert.isDefined(balanceSnapshot);
        assert.isDefined(balanceSnapshot.timestamp);
        _.merge(this.data, {
            tokens: {
                [superToken]: {
                    accounts: {
                        [account]: {
                            balanceSnapshot: {
                                availableBalance:
                                    balanceSnapshot.availableBalance,
                                deposit: balanceSnapshot.deposit,
                                owedDeposit: balanceSnapshot.owedDeposit,
                                timestamp: balanceSnapshot.timestamp
                            }
                        }
                    }
                }
            }
        });
    }

    getAccountBalanceSnapshot(superToken, account) {
        _.defaultsDeep(this.data, {
            tokens: {
                [superToken]: {
                    accounts: {
                        [account]: {
                            balanceSnapshot: {
                                availableBalance: 0,
                                deposit: 0,
                                owedDeposit: 0,
                                timestamp: 0
                            }
                        }
                    }
                }
            }
        });
        return _.clone(
            this.data.tokens[superToken].accounts[account].balanceSnapshot
        );
    }

    updateAccountExpectedBalanceDelta(
        superToken,
        account,
        expectedBalanceDelta
    ) {
        assert.isDefined(account);
        assert.isDefined(expectedBalanceDelta);
        _.merge(this.data, {
            tokens: {
                [superToken]: {
                    accounts: {
                        [account]: {
                            expectedBalanceDelta: expectedBalanceDelta.toString()
                        }
                    }
                }
            }
        });
    }

    getAccountExpectedBalanceDelta(superToken, account) {
        _.defaultsDeep(this.data, {
            tokens: {
                [superToken]: {
                    accounts: {
                        [account]: {
                            expectedBalanceDelta: "0"
                        }
                    }
                }
            }
        });
        return toBN(
            this.data.tokens[superToken].accounts[account].expectedBalanceDelta
        );
    }

    /**************************************************************************
     * Logging utilities
     *************************************************************************/

    realtimeBalance(balance) {
        return toBN(balance.availableBalance).add(
            BN.max(
                toBN(0),
                toBN(balance.deposit).sub(toBN(balance.owedDeposit))
            )
        );
    }

    printSingleBalance(title, balance) {
        console.log(
            `${title}:`,
            `${wad4human(balance)} (${balance.toString()})`
        );
    }

    printRealtimeBalance(title, balance) {
        console.log(
            `${title}: `,
            `${wad4human(
                balance.availableBalance
            )} (${balance.availableBalance.toString()})`,
            `${wad4human(balance.deposit)} (${balance.deposit.toString()})`,
            `${wad4human(
                balance.owedDeposit
            )} (${balance.owedDeposit.toString()})`,
            balance.timestamp.toString()
        );
    }

    /**************************************************************************
     * Invariance tests
     *************************************************************************/

    async validateExpectedBalances(syncExpectedBalancesFn) {
        //console.log("!!! 1", JSON.stringify(testenv.data, null, 4));
        const { superToken } = this.contracts;

        const txBlock = await web3.eth.getBlock("latest");
        const balances2 = {};

        // update balance snapshot
        await Promise.all(
            this.listAddresses().map(async address => {
                balances2[address] = await superToken.realtimeBalanceOf(
                    address,
                    txBlock.timestamp
                );
                balances2[address].timestamp = txBlock.timestamp;
            })
        );

        await syncExpectedBalancesFn();

        await Promise.all(
            this.listAddresses().map(async address => {
                const alias = this.toAlias(address);

                const balanceSnapshot1 = this.getAccountBalanceSnapshot(
                    superToken.address,
                    address
                );
                const realtimeBalanceDelta = this.realtimeBalance(
                    balances2[address]
                ).sub(this.realtimeBalance(balanceSnapshot1));
                console.log(
                    `${alias} real-time balance delta`,
                    realtimeBalanceDelta.toString()
                );

                const expectedBalanceDelta = this.getAccountExpectedBalanceDelta(
                    superToken.address,
                    address
                );

                assert.equal(
                    realtimeBalanceDelta.toString(),
                    expectedBalanceDelta.toString(),
                    `wrong real-time balance changes of ${alias}`
                );

                this.updateAccountBalanceSnapshot(
                    superToken.address,
                    address,
                    balances2[address]
                );

                this.updateAccountExpectedBalanceDelta(
                    superToken.address,
                    address,
                    0
                );
            })
        );
    }

    async validateSystemInvariance({ allowCriticalAccount } = {}) {
        console.log("======== validateSystemInvariance begins ========");

        const currentBlock = await web3.eth.getBlock("latest");

        let rtBalanceSum = toBN(0);
        await Promise.all(
            this.listAliases().map(async alias => {
                const userAddress = this.getAddress(alias);
                const tokenBalance = await this.contracts.testToken.balanceOf.call(
                    userAddress
                    /* TODO query old block currentBlock.timestamp*/
                );
                const superTokenBalance = await this.contracts.superToken.realtimeBalanceOf.call(
                    userAddress,
                    currentBlock.timestamp.toString()
                );
                superTokenBalance.timestamp = currentBlock.timestamp;
                // Available Balance = Realtime Balance - Deposit + Min(Deposit, Owed Deposit)
                const realtimeBalance = superTokenBalance.availableBalance
                    .add(superTokenBalance.deposit)
                    .sub(
                        web3.utils.BN.min(
                            superTokenBalance.owedDeposit,
                            superTokenBalance.deposit
                        )
                    );

                this.printSingleBalance(
                    `${alias} underlying token balance`,
                    tokenBalance
                );
                console.log(
                    `${alias} super token balance`,
                    wad4human(realtimeBalance)
                );
                this.printRealtimeBalance(
                    `${alias} super token balance (tuple)`,
                    superTokenBalance
                );

                if (!allowCriticalAccount) {
                    assert.isTrue(
                        superTokenBalance.availableBalance.gte(toBN(0)),
                        `${alias} account is critical`
                    );
                }

                rtBalanceSum = rtBalanceSum.add(realtimeBalance);
            })
        );

        this.printSingleBalance(
            "Total real-time blances of super tokens",
            rtBalanceSum
        );

        const aum = await this.contracts.testToken.balanceOf.call(
            this.contracts.superToken.address
        );
        this.printSingleBalance("AUM of super tokens", aum);

        const totalSupply = await this.contracts.superToken.totalSupply.call();
        this.printSingleBalance("Total supply of super tokens", totalSupply);

        assert.isTrue(
            aum.gte(rtBalanceSum),
            "AUM should be equal or more than real-time balance"
        );
        assert.isTrue(
            aum.sub(rtBalanceSum).lte(this.configs.AUM_DUST_AMOUNT),
            "AUM minus the real-time balance sum should only be a dust amount"
        );
        assert.equal(
            wad4human(aum, 8),
            wad4human(rtBalanceSum, 8),
            "AUM should match the real-time balance sum to at least 8 decimals during testing"
        );
        assert.equal(
            aum.toString(),
            totalSupply.toString(),
            "Total supply should be equal to the AUM"
        );

        console.log("======== validateSystemInvariance ends ========");
    }
};
