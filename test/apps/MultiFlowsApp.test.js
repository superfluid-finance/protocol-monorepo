const { expectRevert } = require("@openzeppelin/test-helpers");

const Tester = require("../superfluid/Tester");
const MultiApp = artifacts.require("MultiFlowsApp");

const {
    web3tx,
    toWad,
} = require("@decentral.ee/web3-helpers");

const FLOW_RATE = toWad(1);

contract("MultiFlowsApp", accounts => {

    const tester = new Tester(accounts);

    let superToken;
    let cfa;
    let superfluid;

    const { INIT_BALANCE } = tester.constants;
    const { alice, bob, carol } = tester.aliases;

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            superToken,
            cfa,
            superfluid
        } = tester.contracts);
    });

    it("#1 MultiFlowsApp", async () => {

        await superToken.upgrade(INIT_BALANCE, {from: alice});
        const app = await web3tx(MultiApp.new, "MultiApp.new")(cfa.address, superfluid.address);

        await web3tx(superfluid.callAppAction, "Superfluid.callAppAction")(
            app.address,
            app.contract.methods.createMultiFlows(
                superToken.address,
                [bob, carol],
                [6, 4],
                "0x"
            ).encodeABI(),
            {
                from: alice
            }
        );

        await web3tx(superfluid.callAgreement, "Alice create flow to app 1x")(
            cfa.address,
            cfa.contract.methods.createFlow(
                superToken.address,
                app.address,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI(),
            {
                from: alice,
            }
        );

        let aliceNetFlow = await cfa.getNetFlow.call(superToken.address, alice);
        let bobNetFlow = await cfa.getNetFlow.call(superToken.address, bob);
        let carolNetFlow = await cfa.getNetFlow.call(superToken.address, carol);

        assert.equal(aliceNetFlow.toString(), -FLOW_RATE, "Alice net flow is wrong");
        assert.equal(bobNetFlow, FLOW_RATE * 6 / 10, "Bob net flow is wrong");
        assert.equal(carolNetFlow, FLOW_RATE * 4 / 10, "Carol net flow is wrong");

        console.log("Bob Receiving : ", FLOW_RATE * 6 / 10);
        console.log("Calor Receiving : ", FLOW_RATE * 4 / 10);

        await expectRevert(web3tx(superfluid.callAgreement,  "Alice update flow to app 1x")(
            cfa.address,
            cfa.contract.methods.updateFlow(
                superToken.address,
                app.address,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI(),
            {
                from: alice,
            }
        ), "MFA: only increasing flow rate");

        // FIXME deposit allowance should be supported
        superToken.transfer(app.address, toWad(5), { from: alice });

        await web3tx(superfluid.callAgreement, "Alice update flow to app 2x")(
            cfa.address,
            cfa.contract.methods.updateFlow(
                superToken.address,
                app.address,
                FLOW_RATE.mul(web3.utils.toBN(2)).toString(),
                "0x"
            ).encodeABI(),
            {
                from: alice,
            }
        );

        aliceNetFlow = await cfa.getNetFlow.call(superToken.address, alice);
        bobNetFlow = await cfa.getNetFlow.call(superToken.address, bob);
        carolNetFlow = await cfa.getNetFlow.call(superToken.address, carol);

        assert.equal(aliceNetFlow.toString(), -FLOW_RATE*2, "Alice net flow is wrong");
        assert.equal(bobNetFlow, FLOW_RATE*2 * 6 / 10, "Bob net flow is wrong");
        assert.equal(carolNetFlow, FLOW_RATE*2 * 4 / 10, "Carol net flow is wrong");

        console.log("Bob Receiving : ", FLOW_RATE*2 * 6 / 10);
        console.log("Calor Receiving : ", FLOW_RATE*2 * 4 / 10);

        const deleteABI = cfa.contract.methods.deleteFlow(
            superToken.address,
            alice,
            app.address,
            "0x"
        ).encodeABI();

        await web3tx(superfluid.callAgreement, "Superfuild.callAgreement alice deleting flow")(
            cfa.address,
            deleteABI,
            {
                from: alice
            }
        );

        const aliceNetFlowAfter = await cfa.getNetFlow.call(superToken.address, alice);
        const bobNetFlowAfter = await cfa.getNetFlow.call(superToken.address, bob);
        const carolNetFlowAfter = await cfa.getNetFlow.call(superToken.address, carol);
        const appNetFlowAfter = await cfa.getFlow.call(
            superToken.address,
            app.address, carol);

        console.log("Alice", aliceNetFlowAfter.toString());
        console.log("Bob", bobNetFlowAfter.toString());
        console.log("Carol", carolNetFlowAfter.toString());
        console.log("App", appNetFlowAfter);

        //aliceDeposit = await superToken.realtimeBalanceOf.call(alice, block.timestamp);
        //bobDeposit = await superToken.realtimeBalanceOf.call(bob, block.timestamp);
        //appDeposit = await superToken.realtimeBalanceOf.call(app.address, block.timestamp);
    });

    it("#2 MultiFlowsApp Batch Call", async () => {

        await superToken.upgrade(INIT_BALANCE, {from: accounts[1]});
        const app = await web3tx(MultiApp.new, "MultiApp.new")(cfa.address, superfluid.address);

        await web3tx(superfluid.batchCall, "Superfluid.batchCall")(
            [
                [
                    5,
                    app.address,
                    app.contract.methods.createMultiFlows(superToken.address, [bob, carol], [6, 4], "0x").encodeABI()
                ],
                [
                    4,
                    cfa.address, cfa.contract.methods.createFlow(
                        superToken.address,
                        app.address,
                        "1000000000000000000",
                        "0x"
                    ).encodeABI()
                ]
            ],
            {
                from: alice
            }
        );

        const aliceNetFlow = await cfa.getNetFlow.call(superToken.address, alice);
        const bobNetFlow = await cfa.getNetFlow.call(superToken.address, bob);
        const carolNetFlow = await cfa.getNetFlow.call(superToken.address, carol);
        assert.equal(aliceNetFlow.toString(), -FLOW_RATE, "Alice net flow is wrong");
        assert.equal(bobNetFlow, FLOW_RATE * 6 / 10, "Bob net flow is wrong");
        assert.equal(carolNetFlow, FLOW_RATE * 4 / 10, "Carol net flow is wrong");
    });
});
