const { Framework } = require("@superfluid-finance/sdk-core");
const { assert } = require("chai");
const { expectRevert } = require("@openzeppelin/test-helpers");
const { ethers, web3, network } = require("hardhat");
const daiABI = require("./abis/fDAIABI");
const LoanArtifact = require("../artifacts/contracts/EmploymentLoan.sol/EmploymentLoan.json");
const LoanABI = LoanArtifact.abi;

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

const provider = web3;

let accounts;

let sf;
let dai;
let daix;
let col;
let colx;
let borrower;
let lender;
let employer;
let outsider;
let loanFactory;
let priceFeed;
let employmentLoan;

const errorHandler = (err) => {
    if (err) throw err;
};

before(async function () {    

    //get accounts from hardhat
    accounts = await ethers.getSigners();

    //deploy the framework
    await deployFramework(errorHandler, {
        web3,
        from: accounts[0].address,
    });

    //deploy a fake erc20 token for borrow token
    let fDAIAddress = await deployTestToken(errorHandler, [":", "fDAI"], {
        web3,
        from: accounts[0].address,
    });

    //deploy another fake erc20 token for collateral token
    let fCOLAddress = await deployTestToken(errorHandler, [":", "fCOL"], {
        web3,
        from: accounts[0].address,
    });

    //deploy a fake erc20 wrapper super token around the fDAI token
    let fDAIxAddress = await deploySuperToken(errorHandler, [":", "fDAI"], {
        web3,
        from: accounts[0].address,
    });

    let fCOLxAddress = await deploySuperToken(errorHandler, [":", "fCOL"], {
        web3,
        from: accounts[0].address,
    });

    //initialize the superfluid framework...put custom and web3 only bc we are using hardhat locally
    sf = await Framework.create({
        chainId: 31337,
        provider,
        resolverAddress: process.env.RESOLVER_ADDRESS, //this is how you get the resolver address
        protocolReleaseVersion: "test",
    });    

    borrower = await sf.createSigner({
        signer: accounts[0],
        provider: provider
    });    

    lender = await sf.createSigner({
        signer: accounts[1],
        provider: provider
    });

    employer = await sf.createSigner({
        signer: accounts[2],
        provider: provider
    })

    outsider = await sf.createSigner({
        signer: accounts[3],
        provider: provider
    });
    //use the framework to get the super toen
    daix = await sf.loadSuperToken("fDAIx");
    colx = await sf.loadSuperToken("fCOLx");
    
    //get the contract object for the erc20 token
    let daiAddress = daix.underlyingToken.address;
    dai = new ethers.Contract(daiAddress, daiABI, accounts[0]);

    let coladdress = colx.underlyingToken.address;
    col = new ethers.Contract(coladdress, daiABI, accounts[0]);
    
    let LoanFactory = await ethers.getContractFactory("LoanFactory", accounts[0]);   
    loanFactory = await LoanFactory.deploy();
    await loanFactory.deployed();
    
    let MockV3Aggregator = await ethers.getContractFactory("MockV3Aggregator", accounts[0]); 
    
    //fake price of collateral token to simulate oracle - 10 borrow tokens for 1 collateral tokens, collateralTokenAddress is used
    priceFeed = await MockV3Aggregator.deploy(10000000000);
    
    await priceFeed.deployed();

    let borrowAmount = ethers.utils.parseEther("1000");
    let interest = 10;
    let paybackMonths = 12;
    let collateralAmount =ethers.utils.parseEther("1000");

    await loanFactory.createNewLoan(
        borrowAmount, //borrowing 1000 fDAI tokens
        interest, // 10% annual interest
        paybackMonths, //in months
        collateralAmount, // total collateral amount required
        employer.address, //address of employer
        borrower.address, //address of borrower
        daix.address,
        colx.address,
        sf.settings.config.hostAddress,
        priceFeed.address,
        8
    );

    let loanAddress = await loanFactory.idToLoan(1);
    
    employmentLoan = new ethers.Contract(loanAddress, LoanABI, accounts[0]); 

});

beforeEach(async function () {
    console.log("Topping up account balances...");
        
    await dai.connect(employer).mint(
        employer.address, ethers.utils.parseEther("10000")
    );

    await dai.connect(lender).mint(
        lender.address, ethers.utils.parseEther("10000")
    );

    await col.connect(borrower).mint(
        borrower.address, ethers.utils.parseEther("1000")
    );    

    await dai.connect(employer).approve(daix.address, ethers.utils.parseEther("10000"));
    await dai.connect(lender).approve(daix.address, ethers.utils.parseEther("10000"));
    await col.connect(borrower).approve(colx.address, ethers.utils.parseEther("1000"));
    
    const employerDaixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("10000")
    });
    const lenderDaixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("10000")
    });
    const borrowerColxUpgradeOperation = colx.upgrade({
        amount: ethers.utils.parseEther("1000")
    });
    
    await employerDaixUpgradeOperation.exec(employer);
    await lenderDaixUpgradeOperation.exec(lender);
    await borrowerColxUpgradeOperation.exec(borrower);

});

describe("employment loan deployment", async function () {
    it("0 deploys correctly", async () => {        

        let borrowAmount = ethers.utils.parseEther("1000");
        let interest = 10;
        let paybackMonths = 12;

        console.log(
        `
        New Loan Generated...
        Loan Address: ${employmentLoan.address}
        Borrow Amount: ${await employmentLoan.borrowAmount()}
        Interest Rate: ${await employmentLoan.interestRate()}
        Payback Months: ${await employmentLoan.paybackMonths()}
        Collateral Token: ${await employmentLoan.collateralToken()}
        Collateral Amount: ${await employmentLoan.collateralAmount()}
        Borrow Token: ${await employmentLoan.borrowToken()}
        Borrow Amount: ${await employmentLoan.borrowAmount()}
        Employer: ${await employmentLoan.employer()}
        Borrower: ${await employmentLoan.borrower()}
        `
        );

        let actualBorrowAmount = await employmentLoan.borrowAmount()
        let actualInterest = await employmentLoan.interestRate();
        let actualPaybackMonths = await employmentLoan.paybackMonths()
        let acutalEmployerAddress = await employmentLoan.employer()
        let actualBorrower = await employmentLoan.borrower();
        let actualBorrowToken = await employmentLoan.borrowToken()
        let actualCollateralToken = await employmentLoan.collateralToken()
        let actualPriceFeed = await employmentLoan.priceFeed()


        assert.equal(
            borrowAmount, actualBorrowAmount.toString(), "borrow amount not equal to intended amount"
        );

        assert.equal(
            interest, actualInterest, "interest rate not equal to intended rate"
        )

        assert.equal(
            paybackMonths, actualPaybackMonths, "payback months not equal to intended months"
        )

        assert.equal(
            employer.address, acutalEmployerAddress, "wrong employer address"
        )

        assert.equal(
            borrower.address, actualBorrower, "wrong borrower address"
        )

        assert.equal(
            daix.address, actualBorrowToken, "wrong borrow token"
        )

        assert.equal(  
            colx.address, actualCollateralToken, "wrong collateral token"
        )

        assert.equal(
            priceFeed.address, actualPriceFeed, "wrong mock address"
        )
    });
});

    describe("Loan is initialized properly", async function() {
        it("1 - Depositing collateral works properly", async () => {
            //1) Calling sendCollateral should deposit collateral in contract at proper amount
            //2) Calling sendCollateral should revert if borrower has insufficient funds
            let collateral = await employmentLoan.collateralAmount();

            let colxApprovalOperation = colx.approve({
                receiver: employmentLoan.address,
                amount: collateral
            });
        
            await colxApprovalOperation.exec(borrower);

            await employmentLoan.connect(borrower).sendCollateral();
            
            let loanContractBalance = await colx.balanceOf({account: employmentLoan.address, providerOrSigner: borrower});

            assert.equal(
                collateral, loanContractBalance, "contract should have all collateral"
            )
        
        });

        it("2 First flow into contract works correctly", async () => {
            
            let loanContractBalance = await colx.balanceOf({account: employmentLoan.address, providerOrSigner: borrower});

            let loadEmployerBalance = await daix.balanceOf({account: employer.address, providerOrSigner: borrower});            

            let employerFlowOperation = sf.cfaV1.createFlow({
                superToken: daix.address,
                receiver: employmentLoan.address,
                flowRate: "3215019290123456" // ~100k per year in usd
            });

            await employerFlowOperation.exec(employer);

            let employerNetFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: employer.address,
                providerOrSigner: employer
            });

            let borrowerNetFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: borrower.address,
                providerOrSigner: employer
            });

            let contractNetFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: employmentLoan.address,
                providerOrSigner: employer
            });

            console.log("employer flow into contract",employerNetFlowRate);
            console.log("borrower flow from contract", borrowerNetFlowRate);
            console.log("contract net flow rate", contractNetFlowRate);

            assert.equal(
                employerNetFlowRate, -3215019290123456
            );

            assert.equal(
                borrowerNetFlowRate, 3215019290123456
            );

            assert.equal(
                contractNetFlowRate, 0
            )

        })

        it("3 - Flow Reduction works correctly", async () => {
            //testing reduction in flow

            const getEmployerContractFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: employer
            });

            console.log("current flow is.....", getEmployerContractFlow.flowRate);
            

            const reduceFlowOperation = sf.cfaV1.updateFlow({
                superToken: daix.address,
                receiver: employmentLoan.address,
                flowRate: "1000000"
            });

            await reduceFlowOperation.exec(employer);

            const newEmployerFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: employer
            });

            console.log("New Employer flow rate:", newEmployerFlowRate.flowRate);
            

            const newBorrowerFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            console.log("New Borrower flow rate: ", newBorrowerFlowRate.flowRate);

            const newContractFlowRate = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: employmentLoan.address,
                providerOrSigner: employer
            });

            console.log("New contract flow rate:", newContractFlowRate);
            

            assert.equal(
                newEmployerFlowRate.flowRate, "1000000", "wrong employer flow rate"
            );
            
            assert.equal(
                newBorrowerFlowRate.flowRate, "1000000", "wrong borrower flow rate"
            );

            assert.equal(
                newContractFlowRate, 0, "contract is not balanced"
            )

        })

        it("4 Lend Function works correctly", async () => {

            //should reduce flow rate, test to ensure failure, then test update flow rate
            //try calling lend - should revert
            const borrowAmount = await employmentLoan.borrowAmount();

            const lenderApprovalOperation = daix.approve({
                receiver: employmentLoan.address,
                amount: borrowAmount
            });

            await lenderApprovalOperation.exec(lender);    

            const employerUpdateFlowOperation = sf.cfaV1.updateFlow({
                superToken: daix.address,
                receiver: employmentLoan.address,
                flowRate: "3215019290123456"
            });

            await employerUpdateFlowOperation.exec(employer);

            let borrowerBalBefore = await daix.balanceOf({account: borrower.address, providerOrSigner: borrower});

            let lenderBalBefore = await daix.balanceOf({account: lender.address, providerOrSigner: lender});

            let borrowerFlowRateBefore = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            let lenderFlowRateBefore = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            console.log("borrow bal before", borrowerBalBefore);
            console.log("lender bal before", lenderBalBefore);
            console.log("borrow amount", borrowAmount.toString());

            await employmentLoan.connect(lender).lend();

            let lenderBalAfter = await daix.balanceOf({account: lender.address, providerOrSigner: lender});

            let borrowerBalAfter = await daix.balanceOf({account: borrower.address, providerOrSigner: borrower});

            let borrowerFlowRateAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            let lenderFlowRateAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            let employerFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: lender
            });

            let expectedLender = await employmentLoan.lender();
            let loanStartedTime = await employmentLoan.loanStartTime();
            
            let expectedFlowRate = await employmentLoan.getPaymentFlowRate();
            console.log("expected lender flow rate", expectedFlowRate.flowRate);
            

            assert.isAtLeast(
                Number(borrowerBalBefore + borrowAmount), Number(borrowerBalAfter), "borrower bal did not increase enough"
            )

            assert.isAtMost(
                lenderBalBefore - borrowAmount, Number(lenderBalAfter), "lender should have less money"
            )

            assert.equal(
                Number(borrowerFlowRateAfter.flowRate), Number(Number(employerFlow.flowRate) - Number(lenderFlowRateAfter.flowRate)), "borrower flow rate incorrect"
                //borrower flow rate should decrease by paymentFlowrate amount after lend is called
            )

            assert.equal(
                //lender flow rate should increase by proper amount when lend is called
                Number(lenderFlowRateAfter.flowRate), Number(borrowerFlowRateBefore.flowRate) - (Number(borrowerFlowRateBefore.flowRate) - expectedFlowRate), "lender flowRate incorrect"
            )

            assert.equal(
                Number(lender.address), Number(expectedLender), "lender is not correct"
            )

            assert.notEqual(
                loanStartedTime, 0, "loan has not been started properly"
            )
            
        });

        it("5 - flow is reduced", async () => {
            const lenderInitialFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            console.log(lenderInitialFlowRate.flowRate);

            const collateralFlowRate = await employmentLoan._getCollateralFlowRate();
            console.log("expected collateral flow rate is: ", collateralFlowRate);
            const [,price,,,] = await priceFeed.latestRoundData();
            console.log("price is:", price)
            
            const colAmount = await employmentLoan.collateralAmount();
            console.log("Collateral Amount", colAmount);
            const [,collateralPrice,,,] = await priceFeed.latestRoundData();
            console.log("Current price of collateral token", collateralPrice);
            
            const updateFlowOp = await sf.cfaV1.updateFlow({
                superToken: daix.address,
                receiver: employmentLoan.address,
                flowRate: "10000"
            });

            await updateFlowOp.exec(employer);

            const newEmployerFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: employer
            })

            console.log("updated contract flow rate", newEmployerFlowRate.flowRate);

            const newBorrowerNetflow = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: borrower.address,
                providerOrSigner: borrower
            });

            console.log("new borrower net flow:", newBorrowerNetflow);

            const collateralFlowToLender = await sf.cfaV1.getFlow({
                superToken: colx.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const borrowFlowToLender = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            console.log("borrow token amount sent to lender: ", borrowFlowToLender.flowRate);

            const borrowerNewFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            console.log("lender init flowRate: ", lenderInitialFlowRate.flowRate);
            console.log("Collateral flow to lender", collateralFlowToLender.flowRate);

            assert.equal(borrowFlowToLender.flowRate, 0, "should be zero")
            //new flow rate should be correct - take a decimal place away bc the price of the collateral is 10 for every 1 borrow token
            assert.equal(Number(collateralFlowToLender.flowRate), Math.floor(Number(lenderInitialFlowRate.flowRate) / 10), "collateral flow rate is incorrect");
            //remaining amount should go to borrower
            assert.equal(borrowerNewFlowRate.flowRate, "10000", "borrower new flow should be zero");
        
        });

        it("6 - should allow a loan to become solvent again after a flow is reduced", async () => {
            const collateralFlowToLenderBefore = await sf.cfaV1.getFlow({
                superToken: colx.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const borrowTokenFlowToLenderBefore = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            //should be 0

            const borrowTokenFlowToBorrowerBefore = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: lender
            });
            //should be ~100000

            let employerFlowOperation = sf.cfaV1.updateFlow({
                superToken: daix.address,
                receiver: employmentLoan.address,
                flowRate: "3215019290123456" // ~100k per year in usd
            });

            await employerFlowOperation.exec(employer);

            const employerFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: lender
            });

            const collateralFlowToLenderAfter = await sf.cfaV1.getFlow({
                superToken: colx.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            //should be 0

            const borrowTokenFlowToLenderAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            //should be total inflow to contract from employer - flow to lender

            const borrowTokenFlowToBorrowerAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: lender
            });

            //should be total inflow to contract from employer - flow to lender

            assert.equal(collateralFlowToLenderAfter.flowRate, 0, "collateral flow rate after should be 0");

            assert.equal(
                Number(borrowTokenFlowToBorrowerAfter.flowRate), Number(Number(employerFlowRate.flowRate) - Number(borrowTokenFlowToLenderAfter.flowRate)), "borrower flow rate incorrect"
                //borrower flow rate should decrease by paymentFlowrate amount after lend is called
            )

            assert.equal(
                //lender flow rate should increase by proper amount when lend is called
                Number(borrowTokenFlowToLenderAfter.flowRate), Number(employerFlowRate.flowRate) - (Number(borrowTokenFlowToBorrowerAfter.flowRate)), "lender flowRate incorrect"
            );
        });

        it("7 - flow is deleted", async() => {
            //delete flow 
            const lenderInitialFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            
            const colAmount = await employmentLoan.collateralAmount();
            console.log("Collateral Amount", colAmount);
            const [,collateralPrice,,,] = await priceFeed.latestRoundData();
            console.log("Current price of collateral token", collateralPrice);
            
            const deleteFlowOp = await sf.cfaV1.deleteFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
            });

            await deleteFlowOp.exec(employer);

            const newEmployerFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: employer
            })

            console.log("updated contract flow rate", newEmployerFlowRate.flowRate);

            const newBorrowerNetflow = await sf.cfaV1.getNetFlow({
                superToken: daix.address,
                account: borrower.address,
                providerOrSigner: borrower
            });

            console.log("new borrower net flow:", newBorrowerNetflow);

            const collateralFlowToLender = await sf.cfaV1.getFlow({
                superToken: colx.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const borrowFlowToLender = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            console.log("borrow token amount sent to lender: ", borrowFlowToLender.flowRate);
            

            const borrowerNewFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            console.log("lender init flowRate: ", lenderInitialFlowRate.flowRate);
            console.log("Collateral flow to lender", collateralFlowToLender.flowRate);

            assert.equal(newEmployerFlowRate.flowRate, 0, "employer to contract flow rate should be 0");

            assert.equal(borrowFlowToLender.flowRate, 0, "lender should no longer receive daix")
            //new flow rate should be correct - take a decimal place away bc the price of the collateral is 10 for every 1 borrow token
            assert.equal(Number(collateralFlowToLender.flowRate), Math.floor(Number(lenderInitialFlowRate.flowRate) / 10), "collateral flow rate is incorrect");
            //remaining amount should go to borrower
            assert.equal(borrowerNewFlowRate.flowRate, "0", "borrower new flow should be zero");

        });

        it("8 - should allow loan to become solvent again after deletion ", async() => {
            //re start flow
            const collateralFlowToLenderBefore = await sf.cfaV1.getFlow({
                superToken: colx.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const borrowTokenFlowToLenderBefore = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            //should be 0
            const borrowTokenFlowToBorrowerBefore = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: lender
            });
            //should be ~100000

            let employerFlowOperation = sf.cfaV1.createFlow({
                superToken: daix.address,
                receiver: employmentLoan.address,
                flowRate: "3215019290123456" // ~100k per year in usd
            });

            await employerFlowOperation.exec(employer);

            const employerFlowRate = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: lender
            });

            const collateralFlowToLenderAfter = await sf.cfaV1.getFlow({
                superToken: colx.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            //should be 0

            const borrowTokenFlowToLenderAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });
            //should be total inflow to contract from employer - flow to lender

            const borrowTokenFlowToBorrowerAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: lender
            });

            //should be total inflow to contract from employer - flow to lender

            assert.equal(collateralFlowToLenderAfter.flowRate, 0, "collateral flow rate after should be 0");

            assert.equal(
                Number(borrowTokenFlowToBorrowerAfter.flowRate), Number(Number(employerFlowRate.flowRate) - Number(borrowTokenFlowToLenderAfter.flowRate)), "borrower flow rate incorrect"
                //borrower flow rate should decrease by paymentFlowrate amount after lend is called
            )

            assert.equal(
                //lender flow rate should increase by proper amount when lend is called
                Number(borrowTokenFlowToLenderAfter.flowRate), Number(employerFlowRate.flowRate) - (Number(borrowTokenFlowToBorrowerAfter.flowRate)), "lender flowRate incorrect"
            ); 
            
        });

        //todo fix - looks like transfer and approve opp don't work here
        it("9 closing the loan early with payment from borrower", async () => {
            //borrower sends payment to pay off loan
            const amountLeft = await employmentLoan.connect(borrower).getTotalAmountRemaining();
            const lenderBalBefore = await daix.balanceOf({account: lender.address, providerOrSigner: lender});
                        
            //somewhat impractical, but we'll assume that the borrower is sent money from lender (they just need the money in general to pay off loan)
            const transferTokenOperation = daix.transfer({
                receiver: borrower.address,
                amount: amountLeft
            });

            await transferTokenOperation.exec(lender);

            const borrowerApprovalOperation = await daix.approve({
                receiver: employmentLoan.address,
                amount: amountLeft
            });

            await borrowerApprovalOperation.exec(borrower);

            await employmentLoan.connect(borrower).closeOpenLoan(amountLeft);

            const lenderFlowRateAfterCompletion = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const borrowerFlowRateAfterCompletion = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            const employerFlowRateAfterCompletion = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan.address,
                providerOrSigner: employer
            });

            assert.equal(lenderFlowRateAfterCompletion.flowRate, 0, "lender flow rate should now be zero");
            assert.equal(borrowerFlowRateAfterCompletion.flowRate, employerFlowRateAfterCompletion.flowRate, "employer should now send 100% of flow to employee");
            assert.isAtLeast(Number(Number(lenderBalBefore) + Number(amountLeft)), Number(lenderBalBefore), "lender should see increase in borrow token balance");
        });

        it("10 closing the loan early from lender", async () => {

            //other party sends payment to pay off loan
            let borrowAmount = ethers.utils.parseEther("1000");
            let interest = 10;
            let paybackMonths = 12;
            let collateralAmount =ethers.utils.parseEther("1000");
            
            await loanFactory.createNewLoan(
                borrowAmount, //borrowing 1000 fDAI tokens
                interest, // 10% annual interest
                paybackMonths, //in months
                collateralAmount, // total collateral amount required
                employer.address, //address of employer
                borrower.address, //address of borrower
                daix.address,
                colx.address,
                sf.settings.config.hostAddress,
                priceFeed.address,
                8
            );
            
            let loanAddress = await loanFactory.idToLoan(1);
            let loan2Address = await loanFactory.idToLoan(2);
            let employmentLoan2 = new ethers.Contract(loan2Address, LoanABI, accounts[0]); 

            //send collateral
            let collateral = await employmentLoan2.collateralAmount();

            let colxApprovalOperation = colx.approve({
                receiver: employmentLoan2.address,
                amount: collateral
            });
        
            await colxApprovalOperation.exec(borrower);            
            
            console.log("sending collateral to loan 2");
            
            await employmentLoan2.connect(borrower).sendCollateral();

            //create flow

            const createFlowOperation = sf.cfaV1.createFlow({
                superToken: daix.address,
                receiver: employmentLoan2.address,
                flowRate: "3215019290123456"
            });

            await createFlowOperation.exec(employer);

            //lend

            const lenderApprovalOperation = daix.approve({
                receiver: employmentLoan2.address,
                amount: borrowAmount.toString()
            });

            await lenderApprovalOperation.exec(lender);

            console.log("lender lends for loan 2");

            await employmentLoan2.connect(lender).lend();

            //make sure it worked
            const borrowerFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan2.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            const lenderFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan2.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const employerFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan2.address,
                providerOrSigner: employer
            });

            console.log("Borrower flow: ", borrowerFlow.flowRate);
            console.log("lender flow: ", lenderFlow.flowRate)

            let pass6Months = 86400 * (365 / 2);
            await network.provider.send("evm_increaseTime", [pass6Months])
            await network.provider.send("evm_mine");

            //close loan before it ends
            await employmentLoan2.connect(lender).closeOpenLoan(0);

            const borrowerFlowAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan2.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            const lenderFlowAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan2.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            console.log("borrower flow after: ", borrowerFlowAfter.flowRate);
            console.log("lender flow after: ", lenderFlowAfter.flowRate);     
            
            assert.isBelow(Number(borrowerFlow.flowRate), Number(employerFlow.flowRate), "borrower flow rate should be less than total amount sent into loan by employer prior to the closing of the loan");
            assert.isAbove(Number(lenderFlow.flowRate), 0, "lender should have positive flow rate prior to loan ending");
            assert.equal(borrowerFlowAfter.flowRate, employerFlow.flowRate, "borrower flow after loan ends should be equal to full value of employer flow");
            assert.equal(lenderFlowAfter.flowRate, 0, "lender flow rate should be zero after");

            const employerDAIxBalance = await daix.balanceOf({account: employer.address, providerOrSigner: employer})
            console.log("employer daix balance in 2.11...", employerDAIxBalance);

        });


        it("11 borrower closing the loan once completed", async () => {
            //borrower closes loan once complete
            let borrowAmount = ethers.utils.parseEther("1000");
            let interest = 10;
            let paybackMonths = 12;
            let collateralAmount =ethers.utils.parseEther("1000");
            
            await loanFactory.createNewLoan(
                borrowAmount, //borrowing 1000 fDAI tokens
                interest, // 10% annual interest
                paybackMonths, //in months
                collateralAmount, // total collateral amount required
                employer.address, //address of employer
                borrower.address, //address of borrower
                daix.address,
                colx.address,
                sf.settings.config.hostAddress,
                priceFeed.address,
                8
            );
            
            let loanAddress = await loanFactory.idToLoan(1);
            let loan2Address = await loanFactory.idToLoan(2);
            let loan3Address = await loanFactory.idToLoan(3);
            console.log("first loan address", loanAddress);
            console.log("second loan address", loan2Address);
            console.log("third loan address", loan3Address);
            
            let employmentLoan3 = new ethers.Contract(loan3Address, LoanABI, accounts[0]); 

            //send collateral
            let collateral = await employmentLoan3.collateralAmount();

            let colxApprovalOperation = colx.approve({
                receiver: employmentLoan3.address,
                amount: collateral
            });
        
            await colxApprovalOperation.exec(borrower);            
            
            console.log("sending collateral to loan 3");
            
            const borrowerPreSendCollateral = await colx.balanceOf({account: borrower.address, providerOrSigner: borrower});
            console.log("borrower initial collateral balance", borrowerPreSendCollateral)
            await employmentLoan3.connect(borrower).sendCollateral();

            //create flow

            const createLoan3FlowOperation = sf.cfaV1.createFlow({
                superToken: daix.address,
                receiver: employmentLoan3.address,
                flowRate: "3215019290123456"
            });

            console.log("creating a flow from employer to loan 3...");
            const employerDAIxBalance = await daix.balanceOf({account: employer.address, providerOrSigner: employer})
            console.log("employer daix balance...", employerDAIxBalance);

            await createLoan3FlowOperation.exec(employer);

            //lend

            console.log("approving lender spend for loan 3...");


            const lenderApprovalOperation = daix.approve({
                receiver: employmentLoan3.address,
                amount: borrowAmount.toString()
            });

            await lenderApprovalOperation.exec(lender);

            console.log("lender lends for loan 3");

            await employmentLoan3.connect(lender).lend();

            //make sure it worked
            const borrowerFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan3.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });

            const lenderFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan3.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const employerFlow = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employer.address,
                receiver: employmentLoan3.address,
                providerOrSigner: employer
            });

            console.log("Borrower flow: ", borrowerFlow.flowRate);
            console.log("lender flow: ", lenderFlow.flowRate)

            //we will close loan 1 hour after the loan expires
            let passLoanDuration = 86400 * (365 / 12) * paybackMonths + 3600;
            await network.provider.send("evm_increaseTime", [passLoanDuration])
            await network.provider.send("evm_mine");

            const borrowerCollateralBalanceBefore = await colx.balanceOf({account: borrower.address, providerOrSigner: borrower});
            console.log("borrower collateral balance before: ", borrowerCollateralBalanceBefore);

            //close loan before it ends
            await employmentLoan3.connect(borrower).closeCompletedLoan();

            const borrowerFlowAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan3.address,
                receiver: borrower.address,
                providerOrSigner: borrower
            });
  
            const lenderFlowAfter = await sf.cfaV1.getFlow({
                superToken: daix.address,
                sender: employmentLoan3.address,
                receiver: lender.address,
                providerOrSigner: lender
            });

            const borrowerCollateralBalanceAfter = await colx.balanceOf({account: borrower.address, providerOrSigner: borrower});
  
            console.log("borrower collateral balance after: ", borrowerCollateralBalanceAfter);
            
            console.log("borrower flow after: ", borrowerFlowAfter.flowRate);
            console.log("lender flow after: ", lenderFlowAfter.flowRate);

            assert.isBelow(Number(borrowerFlow.flowRate), Number(employerFlow.flowRate), "borrower flow rate should be less than total amount sent into loan by employer prior to the closing of the loan");
            assert.isAbove(Number(lenderFlow.flowRate), 0, "lender should have positive flow rate prior to loan ending");
            assert.equal(borrowerFlowAfter.flowRate, employerFlow.flowRate, "borrower flow after loan ends should be equal to full value of employer flow");
            assert.equal(lenderFlowAfter.flowRate, 0, "lender flow rate should be zero after");

            //TODO - fix this test
            console.log("borrower balance after, ", borrowerCollateralBalanceAfter);
            console.log("borrower balance before,", borrowerCollateralBalanceBefore);
            console.log("collateral amount, " ,collateral.toString());
            assert.equal(Number(borrowerCollateralBalanceAfter), Number(borrowerPreSendCollateral), "borrower should be returned full value of collateral");
            assert.equal(Number(borrowerCollateralBalanceAfter), Number(Number(borrowerCollateralBalanceBefore) + Number(collateral)), "collateral math works out");
        });

    });
