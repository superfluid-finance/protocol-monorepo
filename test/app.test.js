const Tester = require("./Tester");
const MultiApp = artifacts.require("MultiFlowsApp");

const {
    web3tx,
    toWad,
} = require("@decentral.ee/web3-helpers");

const FLOW_RATE = toWad(1);

contract("Superfluid App", accounts => {

    const tester = new Tester(accounts);

    let superToken;
    let flowAgreement;
    let superfluid;

    const { INIT_BALANCE } = tester.constants;
    const { alice, bob, carol } = tester.aliases;

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            superToken,
            flowAgreement,
            superfluid
        } = tester.contracts);
    });

    it("#1 MultiFlowsApp", async () => {

        await superToken.upgrade(INIT_BALANCE, {from: accounts[1]});
        const app = await web3tx(MultiApp.new, "MultiApp.new")(flowAgreement.address, superfluid.address);
        const data = app.contract.methods.createMultiFlows(superToken.address, [bob, carol], [6, 4], "0x").encodeABI();
        await web3tx(superfluid.callAppAction, "Superfluid.callAppAction")(
            app.address,
            data,
            {
                from: alice
            }
        );
        const dataAgreement = flowAgreement.contract.methods.createFlow(
            superToken.address,
            app.address,
            "1000000000000000000",
            "0x"
        ).encodeABI();
        await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice app 1x")(
            flowAgreement.address,
            dataAgreement,
            {
                from: alice,
            }
        );

        const aliceNetFlow = await flowAgreement.getNetFlow.call(superToken.address, alice);
        const bobNetFlow = await flowAgreement.getNetFlow.call(superToken.address, bob);
        const carolNetFlow = await flowAgreement.getNetFlow.call(superToken.address, carol);

        /*
        console.log("Alice", aliceNetFlow.toString());
        console.log("Bob", bobNetFlow.toString());
        console.log("Carol", carolNetFlow.toString());
        console.log("App", appNetFlow);
        */

        assert.equal(aliceNetFlow.toString(), -FLOW_RATE, "Alice net flow is wrong");
        assert.equal(bobNetFlow, FLOW_RATE * 6 / 10, "Bob net flow is wrong");
        assert.equal(carolNetFlow, FLOW_RATE * 4 / 10, "Carol net flow is wrong");

        const deleteABI = flowAgreement.contract.methods.deleteFlow(
            superToken.address,
            alice,
            app.address,
            "0x"
        ).encodeABI();

        await web3tx(superfluid.callAgreement, "Superfuild.callAgreement alice deleting flow")(
            flowAgreement.address,
            deleteABI,
            {
                from: alice
            }
        );


        const aliceNetFlowAfter = await flowAgreement.getNetFlow.call(superToken.address, alice);
        const bobNetFlowAfter = await flowAgreement.getNetFlow.call(superToken.address, bob);
        const carolNetFlowAfter = await flowAgreement.getNetFlow.call(superToken.address, carol);
        const appNetFlowAfter = await flowAgreement.getFlow.call(
            superToken.address,
            web3.utils.soliditySha3(app.address, carol));

        console.log("Alice", aliceNetFlowAfter.toString());
        console.log("Bob", bobNetFlowAfter.toString());
        console.log("Carol", carolNetFlowAfter.toString());
        console.log("App", appNetFlowAfter);


        /*
        //start a multi party
        await web3tx(app.createMultiFlows, "MultiApp.createMultiFlows")(
            superTokenAddr,
            receivers,
            flowRates,{
                from: accounts[1]
            }
        );

        const superTokenAddr = superToken.address.toString();
        const receivers = accounts.slice(2, 4);
        const flowRates = new Array(10000000, 50000000000);

        await web3tx(
            flowAgreement.createFlow,
            "createFlow"
        )(superToken.address, accounts[1], app.address, FLOW_RATE, {from: accounts[1]});

        await web3tx(flowAgreement.updateFlow, "FlowAgreement.updateFlow")(
            superTokenAddr,
            accounts[1],
            app.address,
            "100000000000", {
                from: accounts[1],
                gas: 1e6,
            }
        );

        let tx = await web3tx(flowAgreement.deleteFlow, "FlowAgreement.updateFlow")(
            superTokenAddr,
            accounts[1],
            app.address,
            {
                from: accounts[1],
                gas: 1e6,
            }
        );

        console.log("!!!!!!", tx.logs);
        */
    });
});
