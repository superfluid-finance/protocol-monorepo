const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-test-helpers");

contract("Super Token Behaviour", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];

    let token;
    let superToken;
    let agreement;

    before(async () => {
        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);
    });

    beforeEach(async () => {

        token = await web3tx(ERC20Mintable.new, "ERC20Mintable.new")(
            {
                from: admin
            });

        await token.mint(user1, toWad(10));
        await token.mint(user2, toWad(10));

        superToken = await web3tx(SuperToken.new, "SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });



        agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")(
            {
                from:admin
            });

        await web3tx(token.approve, "Call: ERC20Mintable.approve - from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );


        await web3tx(token.approve, "Call: ERC20Mintabl.approve - from user2 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );

    });

    it("#1 - Should not invoke update directly - assert revert message", async () => {

        let noise = "0x00000000000000000001";
        let emitError = false;
        try {
            await web3tx(superToken.updateAgreementAccountState, "Call: SuperToken.updateAgreementAccountState - Invoking method directly")(
                user1,
                noise, {
                    from: user1
                });

        } catch(err) {
            emitError = true;
            console.log(err.reason);
            assert.strictEqual(err.reason, "Use the agreement contract");
        }

        if(!emitError) {
            throw ("Call: SuperToken.updateState - error not emitted");
        }
    });

    it("#2 - Should not transfer if user don't have balance - assert revert message", async () => {

        let emitError = false;
        try {
            await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - bad balance")(
                toWad(1), {
                    from: user1
                });
        } catch(err) {
            emitError = true;
            console.log(err.reason);
            assert.strictEqual(err.reason, "amount not allowed");
        }

        if(!emitError) {
            throw ("Call: SuperToken.downgrade - error not emitted");
        }
    });

    it("#3 - Should not upgrade SuperToken without underlaying Token balance", async() => {

        let emitError = false;
        try {
            await web3tx(superToken.upgrade, "Call: SuperToken.upgrade - bad balance")(
                toWad(11), {
                    from: user1
                });
        } catch(err) {
            emitError = true;
            console.log(err.reason);
            assert.strictEqual(err.reason, "ERC20: transfer amount exceeds balance");
        }

        if(!emitError) {
            throw ("Call: SuperToken.upgrade - error not emitted");
        }
    });
});
