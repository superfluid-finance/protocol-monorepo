const { Framework } = require("@superfluid-finance/sdk-core");
const { assert, expect } = require("chai");
const { expectRevert } = require("@openzeppelin/test-helpers");
const { ethers, web3 } = require("hardhat");
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
let uniV3TWAP;
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
        networkName: "custom",
        provider,
        dataMode: "WEB3_ONLY",
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
    
    let UniV3TWAP = await ethers.getContractFactory("UniV3ObservationMock", accounts[0]); 
    
    //fake price of collateral token to simulate oracle - 10 borrow tokens for 1 collateral tokens, collateralTokenAddress is used
    uniV3TWAP = await UniV3TWAP.deploy(ethers.utils.parseEther("10"), colx.address);
    
    await uniV3TWAP.deployed();

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
        uniV3TWAP.address
    );

    let loanAddress = await loanFactory.idToLoan(1);

    console.log(loanAddress);
    
    
    employmentLoan = new ethers.Contract(loanAddress, LoanABI, accounts[0]); 

});

beforeEach(async function () {

    console.log(colx.address);
    console.log(daix.address);
    
        
    await dai.connect(employer).mint(
        employer.address, ethers.utils.parseEther("1000")
    );

    await dai.connect(lender).mint(
        lender.address, ethers.utils.parseEther("10000")
    );

    await col.connect(borrower).mint(
        borrower.address, ethers.utils.parseEther("1000")
    );    

    await dai.connect(employer).approve(daix.address, ethers.utils.parseEther("1000"));
    await dai.connect(lender).approve(daix.address, ethers.utils.parseEther("10000"));
    await col.connect(borrower).approve(colx.address, ethers.utils.parseEther("1000"));

    
    const employerDaixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("1000")
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
   
    const employerDaixBal = await daix.balanceOf({account: employer.address, providerOrSigner: employer});
    const lenderDaixBal = await daix.balanceOf({account: lender.address, providerOrSigner: lender});
    const borrowerColxBal = await colx.balanceOf({account: borrower.address, providerOrSigner: borrower});

    console.log('Employer DAIx bal for acct 0: ', employerDaixBal);
    console.log('Lender DAIx bal: ', lenderDaixBal);
    console.log('Borrower Collateral Token Balance ', borrowerColxBal);



});

describe("employment loan deployment", async function () {
    it("deploys correctly", async () => {

        console.log(employmentLoan.address);
        

        let borrowAmount = ethers.utils.parseEther("1000");
        let interest = 10;
        let paybackMonths = 12;
        let collateralAmount =ethers.utils.parseEther("1000");     

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
        Superfluid Host: ${await employmentLoan.host()}
        Superfluid CFA: ${await employmentLoan.cfa()}
        `
        );

        let actualBorrowAmount = await employmentLoan.borrowAmount()
        let actualInterest = await employmentLoan.interestRate();
        let actualPaybackMonths = await employmentLoan.paybackMonths()
        let acutalEmployerAddress = await employmentLoan.employer()
        let actualBorrower = await employmentLoan.borrower();
        let actualBorrowToken = await employmentLoan.borrowToken()
        let actualCollateralToken = await employmentLoan.collateralToken()
        let actualTWAP = await employmentLoan.uniV3TWAP()


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
            uniV3TWAP.address, actualTWAP, "wrong uniV3 mock address"
        )
    });

    describe("Loan is initialized properly", async function() {
        it("1 - Depositing collateral works properly", async () => {
            //1) Calling sendCollateral should deposit collateral in contract at proper amount, and set loanInitiated to true
            //2) Calling sendCollateral should revert if borrower has insufficient funds
            let loanInitiationStatusBefore = await employmentLoan.loanInitiated();
            let collateral = await employmentLoan.collateralAmount();

            let colxApprovalOperation = colx.approve({
                receiver: employmentLoan.address,
                amount: collateral
            });
        
            await colxApprovalOperation.exec(borrower);

            await employmentLoan.connect(borrower).sendCollateral();
            
            let loanInitiationStatus = await employmentLoan.loanInitiated();
            let loanContractBalance = await colx.balanceOf({account: employmentLoan.address, providerOrSigner: borrower});
        
            assert.equal(
                loanInitiationStatusBefore, false
            );

            assert.equal(
                loanInitiationStatus, true, "loan should be initiated"
            );

            assert.equal(
                collateral, loanContractBalance, "contract should have all collateral"
            )
        
        });

        it("1.1 - Should fail if called by outsider", async () => {
            await expectRevert(
                employmentLoan.connect(outsider).sendCollateral(),
                "only borrower sends collateral"
            )
        });

        it("2.1 First flow into contract works correctly", async () => {
            let checkLoan = await employmentLoan.loanInitiated();   
            
            let loanContractBalance = await colx.balanceOf({account: employmentLoan.address, providerOrSigner: borrower});
            console.log(loanContractBalance);

            let loadEmployerBalance = await daix.balanceOf({account: employer.address, providerOrSigner: borrower});
            console.log(loadEmployerBalance);
            

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


            let employerFlowRateStatus = await employmentLoan.checkEmployerFlowRate();
            console.log("employer flow rate status", employerFlowRateStatus);

            assert.equal(
                employerNetFlowRate, -3215019290123456
            );

            assert.equal(
                borrowerNetFlowRate, 3215019290123456
            );

            assert.equal(
                contractNetFlowRate, 0
            )

            assert.equal(
                employerFlowRateStatus, true
            )

        })

        it("2.2 Lend Function works correctly", async () => {

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


            const borrowAmount = await employmentLoan.borrowAmount();
            console.log("borrow bal before", borrowerBalBefore);
            console.log("lender bal before", lenderBalBefore);
            console.log("borrow amount", borrowAmount.toString());

            const lenderApprovalOperation = daix.approve({
                receiver: employmentLoan.address,
                amount: borrowAmount.toString()
            });

            await lenderApprovalOperation.exec(lender);

            await employmentLoan.connect(lender).lend().then(console.log);

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

            let expectedLender = await employmentLoan.lender();
            let loanStartedTime = await employmentLoan.loanStartTime();

            console.log("borrower balance before lend", borrowerBalBefore);
            console.log("borrower balance after", borrowerBalAfter);
            console.log("borrower bal increase", borrowerBalAfter - borrowerBalBefore);

            console.log("lender:", expectedLender);
            console.log("lender balance before lend", lenderBalBefore);
            console.log("lender balance after", lenderBalAfter);
            console.log("lender bal increase", lenderBalAfter - lenderBalBefore);

            console.log("borrower flow rate before", borrowerFlowRateBefore);
            console.log("lender flow rate before", lenderFlowRateBefore);
            console.log("borrower flow rate after", borrowerFlowRateAfter);
            console.log("lender flow rate after", lenderFlowRateAfter);
            console.log("loan start time", loanStartedTime);
            

            let expectedFlowRate = await employmentLoan.getPaymentFlowRate();
            console.log("expected lender flow rate", expectedFlowRate);
            

            assert.isAtLeast(
                Number(borrowerBalBefore + borrowAmount), Number(borrowerBalAfter), "borrower bal did not increase enough"
            )

            assert.isAtMost(
                lenderBalBefore - borrowAmount, Number(lenderBalAfter), "lender should have less money"
            )

            assert.equal(
                Number(borrowerFlowRateAfter.flowRate), Number(Number(borrowerFlowRateBefore.flowRate) - Number(lenderFlowRateAfter.flowRate)), "borrower flow rate incorrect"
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

            
            //need to 
            //5) If flow is not from employer in proper amount, loan should not initiate
            //6) Check Flow Reduction cases, if flow is deleted or updated so that the lender can't get paid...
                //make sure that flowRates with borrow token (daix) are properly updated
                //make sure that a flow is created in the collateral token at the correct exchange rate
            //7 check cases of closing the loan
                //if lender closes, no up front payment needed
                //if loan is paid off and non-lender closes, no payment needed
                //if loan is NOT paid off and non-lender closes, require the payment
        })

    });

    })

    //need to test following cases
    //NOTE: some of these may require additional changes to contract
    //X Calling sendCollateral should deposit collateral in contract at proper amount, and set loanInitiated to true
    //X Calling sendCollateral should revert if called by someone other than the borrower
    //X What starts the loan? I.e. what comes next after collateral in contract? Employer should send flow first before a lender can call lend
    //X Check if
        //(X) borrow amount sent to borrower from lender
        //(X) flowRate to borrower has been decreased by paymentFlowRate amount
        //(X) flow has been created to lender for paymentFlowRate amount
        //(X)lender is now the msg.sender of the lend call
        //(X) loan has been started - can check by querying loanStartTime value
    //X Calling lend should send funds to borrower, reduce flow to borrower by paymentFlowRate amount, and create a flow to lender by paymentFlowRate amount
    //5) If flow is not from employer in proper amount, loan should not initiate
    //6) If flow into contract is reduced to amount below payment Flow rate amount, check current price of collateral and stream funds to lender in equal proportion to paymentFlowRate, but with collateral token instead of borrow token (these can be same token)
    //7) If contract is streaming out collateral because the loan became insolvent, and the flowRate into the contract is increased to an amount > the required payment to lender, then delete flow with collateral token and create flow in borrow token
    //8) If time has passed to point where loan period ends, then the borrower can close stream freely and remaining collateral is sent back to borrower
    //9) If loan period has not ended, the lender can close stream at any time and remaining collateral is sent back to borrower
    //10) If loan period has not ended, then anyone who is not the lender may close stream by sending the lender an amount == the remaining loan balance
    //11) lender should not be able to call lend before a loan is initialized
    //12) sending multiple flows to the contract should still work - only the paymentFlowRate amount should be actively send to lender, new increases above payment flow rate should be sent to borrower

