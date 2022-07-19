const { Framework } = require("@superfluid-finance/sdk-core");
const { assert } = require("chai");
const { ethers } = require("ethers");
// const { ethers, web3 } = require("hardhat");
const daiABI = require("./abis/fDAIABI");
const web3Provider = new ethers.providers.Web3Provider(web3.currentProvider);
// const signer = web3Provider.getSigner(accounts[0]);

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

const provider = web3Provider;


let sf;
let dai;
let daix;
let superSigner;
let TradeableCashflow;

const errorHandler = (err) => {
    if (err) throw err;
};


contract("TradeableCashflow", (accounts) => {

before(async function () {

    console.log(accounts);
    //deploy the framework
    await deployFramework(errorHandler, {
        web3,
        from: accounts[0],
    });

    //deploy a fake erc20 token
    let fDAIAddress = await deployTestToken(errorHandler, [":", "fDAI"], {
        web3,
        from: accounts[0],
    });
    //deploy a fake erc20 wrapper super token around the fDAI token
    let fDAIxAddress = await deploySuperToken(errorHandler, [":", "fDAI"], {
        web3,
        from: accounts[0],
    });
    
    //initialize the superfluid framework...put custom and web3 only bc we are using hardhat locally
    sf = await Framework.create({
        networkName: "custom",
        provider,
        dataMode: "WEB3_ONLY",
        resolverAddress: process.env.RESOLVER_ADDRESS, //this is how you get the resolver address
        protocolReleaseVersion: "test",
    });
    

    superSigner = await sf.createSigner({
        signer: provider.getSigner(accounts[0]),
        provider: provider
    });
    //use the framework to get the super toen
    daix = await sf.loadSuperToken("fDAIx");
    
    //get the contract object for the erc20 token
    let daiAddress = daix.underlyingToken.address;
    dai = new ethers.Contract(daiAddress, daiABI, provider);
    let App = artifacts.require("TradeableCashflow");

    // let App = await ethers.getContractFactory("TradeableCashflow", accounts[0]);    
    
    TradeableCashflow = await App.new(
        accounts[1],
        "TradeableCashflow",
        "TCF",
        sf.settings.config.hostAddress,
        daix.address
    );    
});

beforeEach(async function () {
    
    await dai.connect(provider.getSigner(accounts[0])).mint(
        accounts[0], ethers.utils.parseEther("1000")
    );

    await dai.connect(provider.getSigner(accounts[0])).approve(daix.address, ethers.utils.parseEther("1000"));

    const daixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("1000")
    });

    console.log(provider.getSigner(accounts[0]))
    
    await daixUpgradeOperation.exec(provider.getSigner(accounts[0]));

    const daiBal = await daix.balanceOf({account: accounts[0], providerOrSigner: superSigner});
    console.log('daix bal for acct 0: ', daiBal);
});

describe("sending flows", async function () {    
    
    it("Case #1 - Alice sends a flow", async () => {

        console.log(TradeableCashflow.address);

        const appInitialBalance = await daix.balanceOf({
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
        });
        
        const createFlowOperation = sf.cfaV1.createFlow({
            receiver: TradeableCashflow.address,
            superToken: daix.address,
            flowRate: "100000000",
        })    
                
        const txn = await createFlowOperation.exec(provider.getSigner(accounts[0]));

        const receipt = await txn.wait();

        const appFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
          });

        const ownerFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[1],
            providerOrSigner: superSigner
        })
        
        const appFinalBalance = await daix.balanceOf({
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
        });        

        assert.equal(
            ownerFlowRate, "100000000", "owner not receiving 100% of flowRate"
        );

        assert.equal(
            appFlowRate,
            0,
            "App flowRate not zero"
        );

        assert.equal(
            appInitialBalance.toString(),
            appFinalBalance.toString(),
            "balances aren't equal"
        );
    });

    it("Case #2 - Alice upates flows to the contract", async () => {

        const appInitialBalance = await daix.balanceOf({
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
        });

        const initialOwnerFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[1],
            providerOrSigner: superSigner
        })

        console.log('initial owner flow rate: ', initialOwnerFlowRate);      

        const appFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
          });

        const ownerFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[1],
            providerOrSigner: superSigner
        })

        const senderFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[0],
            providerOrSigner: superSigner
        })
        console.log('sender flow rate: ', senderFlowRate);
        console.log('tcf address: ', TradeableCashflow.address);
        console.log('app flow rate: ', appFlowRate);
        
        
        const updateFlowOperation = sf.cfaV1.updateFlow({
            receiver: TradeableCashflow.address,
            superToken: daix.address,
            flowRate: "200000000",
        })    
                
        const updateFlowTxn = await updateFlowOperation.exec(provider.getSigner(accounts[0]));

        const updateFlowReceipt = await updateFlowTxn.wait();        
        
        const appFinalBalance = await daix.balanceOf({
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
        });     
        
        const updatedOwnerFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[1],
            providerOrSigner: superSigner
        });

        assert.equal(
            updatedOwnerFlowRate, "200000000", "owner not receiving correct updated flowRate"
        );

        assert.equal(
            appFlowRate,
            0,
            "App flowRate not zero"
        );

        assert.equal(
            appInitialBalance.toString(),
            appFinalBalance.toString(),
            "balances aren't equal"
        );

    });

    it('Case 3: multiple users send flows into contract', async () => {
        const appInitialBalance = await daix.balanceOf({
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
        });

        const initialOwnerFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[1],
            providerOrSigner: superSigner
        })

        console.log('initial owner flow rate: ', initialOwnerFlowRate);    

        console.log(accounts[2]);
    
        const daixTransferOperation = daix.transfer({
            receiver: accounts[2], 
            amount: ethers.utils.parseEther("500")
        });
    
        await daixTransferOperation.exec(provider.getSigner(accounts[0]));   
        
        const account2Balance = await daix.balanceOf({account: accounts[2], providerOrSigner: superSigner});
        console.log('account 2 balance ',account2Balance);
        
        const createFlowOperation2 = sf.cfaV1.createFlow({
            receiver: TradeableCashflow.address,
            superToken: daix.address,
            flowRate: "100000000",
        })    
                
        const createFlowOperation2Txn = await createFlowOperation2.exec(provider.getSigner(accounts[2]));

        const createFlowOperation2Receipt = await createFlowOperation2Txn.wait();

        const appFlowRate = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
          });

          const appFinalBalance = await daix.balanceOf({
            account: TradeableCashflow.address,
            providerOrSigner: superSigner
        });     
        
        const updatedOwnerFlowRate2 = await sf.cfaV1.getNetFlow({
            superToken: daix.address,
            account: accounts[1],
            providerOrSigner: superSigner
        });

        assert.equal(
            updatedOwnerFlowRate2, "300000000", "owner not receiving correct updated flowRate"
        );

        assert.equal(
            appFlowRate,
            0,
            "App flowRate not zero"
        );

        assert.equal(
            appInitialBalance.toString(),
            appFinalBalance.toString(),
            "balances aren't equal"
        );
    })

//need deletion case
        
});

    describe("Changing owner", async function () {
        it("Case #5 - When the owner changes, the flow changes", async () => {
        
            const initialOwnerFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: accounts[1],
                providerOrSigner: superSigner
            });

            console.log("initial owner ", await TradeableCashflow.ownerOf(1));
            console.log("initial owner flowRate flowRate: ", initialOwnerFlowRate);
            
            const newOwnerFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: accounts[3],
                providerOrSigner: superSigner
            });

            console.log("new owner flowRate: ", newOwnerFlowRate);
            assert.equal(0, newOwnerFlowRate, "new owner shouldn't have flow yet");
            
            await TradeableCashflow.transferFrom(accounts[1], accounts[3], 1, {from: accounts[1]});

            console.log("new owner, ", await TradeableCashflow.ownerOf(1));

            const initialOwnerUpdatedFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: accounts[1],
                providerOrSigner: superSigner
            });

            console.log("initial owner updated flow rate", initialOwnerUpdatedFlowRate);

            assert.equal(initialOwnerUpdatedFlowRate, 0, "old owner should no longer be receiving flows");

            const newOwnerUpdatedFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: accounts[3],
                providerOrSigner: superSigner
            });

            console.log('new owner updated flowrate', newOwnerUpdatedFlowRate);

            assert.equal(newOwnerUpdatedFlowRate, initialOwnerFlowRate, "new receiver should be getting all of flow into app")
        });
    });
});