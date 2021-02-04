const { expectRevert } = require("@openzeppelin/test-helpers");
const IChiToken = artifacts.require("IChiToken");
const ChiTokenTester = artifacts.require("ChiTokenTester");
const deployChi = require("../../../scripts/deploy-chi");

const CHI_TOKEN_ADDRESS = "0x0000000000004946c0e9F43F4Dee607b0eF1fA1c";

contract("ChiToken", async accounts => {
    function errorHandler(err) {
        if (err) throw err;
    }

    before(async function() {
        await deployChi(errorHandler, { web3, from: accounts[0] });
        this.chiToken = await IChiToken.at(CHI_TOKEN_ADDRESS);
        this.testHelper = await ChiTokenTester.new();
    });

    it("Should have right contract address", async function() {
        expect(this.chiToken.address).to.be.equal(
            "0x0000000000004946c0e9F43F4Dee607b0eF1fA1c"
        );
        // expect(this.chiToken.address).to.be.equal('0x000000000097f1f995665BE2191193a57c68992C');
    });

    it("Should mint", async function() {
        await this.chiToken.mint(100);
        expect((await this.chiToken.totalSupply()).toString()).to.be.equal(
            "100"
        );
        expect(await this.chiToken.computeAddress2(0)).to.be.equal(
            "0x78D8a376e10F1098d9025A50B0fdAC3954572c8A"
        );
    });

    it("Should fail to free up", async function() {
        expectRevert(
            this.chiToken.free(100, { from: accounts[3] }),
            "ERC20: burn amount exceeds balance"
        );
    });

    it("Should burnGasAndFreeFrom", async function() {
        await this.chiToken.mint(100);
        await this.chiToken.approve(this.testHelper.address, 50);
        await this.testHelper.burnGasAndFreeFrom(
            this.chiToken.address,
            5000000,
            50
        );
        expect((await this.chiToken.totalSupply()).toString()).to.be.equal(
            "150"
        );
    });

    it("Should burnGasAndFree", async function() {
        await this.chiToken.transfer(this.testHelper.address, 75);
        await this.testHelper.burnGasAndFree(
            this.chiToken.address,
            5000000,
            75
        );
        expect((await this.chiToken.totalSupply()).toString()).to.be.equal(
            "75"
        );
    });

    it("Should burnGasAndFreeUpTo", async function() {
        await this.chiToken.mint(75);
        await this.chiToken.transfer(this.testHelper.address, 75);
        await this.testHelper.burnGasAndFreeUpTo(
            this.chiToken.address,
            5000000,
            75
        );
        expect((await this.chiToken.totalSupply()).toString()).to.be.equal(
            "75"
        );
    });

    it("Should burnGasAndFreeFromUpTo", async function() {
        await this.chiToken.mint(100);
        await this.chiToken.approve(this.testHelper.address, 70);
        await this.testHelper.burnGasAndFreeFromUpTo(
            this.chiToken.address,
            5000000,
            70
        );
        expect((await this.chiToken.totalSupply()).toString()).to.be.equal(
            "105"
        );
    });
});
