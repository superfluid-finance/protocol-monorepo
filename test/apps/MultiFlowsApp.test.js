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

        await superToken.upgrade(INIT_BALANCE, {from: accounts[1]});
        const app = await web3tx(MultiApp.new, "MultiApp.new")(cfa.address, superfluid.address);
        const data = app.contract.methods.createMultiFlows(superToken.address, [bob, carol], [6, 4], "0x").encodeABI();
        await web3tx(superfluid.callAppAction, "Superfluid.callAppAction")(
            app.address,
            data,
            {
                from: alice
            }
        );
        const dataAgreement = cfa.contract.methods.createFlow(
            superToken.address,
            app.address,
            "1000000000000000000",
            "0x"
        ).encodeABI();
        await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice app 1x")(
            cfa.address,
            dataAgreement,
            {
                from: alice,
            }
        );

        let block = await web3.eth.getBlock("latest");
        const aliceNetFlow = await cfa.getNetFlow.call(superToken.address, alice);
        const bobNetFlow = await cfa.getNetFlow.call(superToken.address, bob);
        const carolNetFlow = await cfa.getNetFlow.call(superToken.address, carol);

        let aliceDeposit = await superToken.realtimeBalanceOf.call(alice, block.timestamp);
        let bobDeposit = await superToken.realtimeBalanceOf.call(bob, block.timestamp);
        let appDeposit = await superToken.realtimeBalanceOf.call(app.address, block.timestamp);

        console.log("Alice Deposit", aliceDeposit[1].toString(), aliceDeposit[2].toString());
        console.log("Bob Deposit", bobDeposit[1].toString(), bobDeposit[2].toString());
        console.log("App Deposit", appDeposit[1].toString(), appDeposit[2].toString());

        /*
        let appToBob = await superToken.getDepositFromData(cfa.address, web3.utils.soliditySha3(app.address, bob));
        let appToCarol = await superToken.getDepositFromData(cfa.address, web3.utils.soliditySha3(app.address, carol));
        console.log(appToBob[0].toString(), appToBob[1].toString(),appToBob[2].toString());
        console.log(appToCarol[0].toString(), appToCarol[1].toString(), appToCarol[2].toString());
        */

        assert.equal(aliceNetFlow.toString(), -FLOW_RATE, "Alice net flow is wrong");
        assert.equal(bobNetFlow, FLOW_RATE * 6 / 10, "Bob net flow is wrong");
        assert.equal(carolNetFlow, FLOW_RATE * 4 / 10, "Carol net flow is wrong");

        console.log("Bob Receiving : ", FLOW_RATE * 6 / 10);
        console.log("Calor Receiving : ", FLOW_RATE * 4 / 10);
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
            web3.utils.soliditySha3(app.address, carol));

        block = await web3.eth.getBlock("latest");
        console.log("Alice", aliceNetFlowAfter.toString());
        console.log("Bob", bobNetFlowAfter.toString());
        console.log("Carol", carolNetFlowAfter.toString());
        console.log("App", appNetFlowAfter);

        aliceDeposit = await superToken.realtimeBalanceOf.call(alice, block.timestamp);
        bobDeposit = await superToken.realtimeBalanceOf.call(bob, block.timestamp);
        appDeposit = await superToken.realtimeBalanceOf.call(app.address, block.timestamp);

        console.log("Alice Deposit", aliceDeposit[1].toString(), aliceDeposit[2].toString());
        console.log("Bob Deposit", bobDeposit[1].toString(), bobDeposit[2].toString());
        console.log("App Deposit", appDeposit[1].toString(), appDeposit[2].toString());
    });

    it("#2 MultiFlowsApp Batch Call", async () => {

        await superToken.upgrade(INIT_BALANCE, {from: accounts[1]});
        const app = await web3tx(MultiApp.new, "MultiApp.new")(cfa.address, superfluid.address);
        const dataApp = [5,
            app.address,
            app.contract.methods.createMultiFlows(superToken.address, [bob, carol], [6, 4], "0x").encodeABI()
        ];

        const dataAgreement = [4,
            cfa.address, cfa.contract.methods.createFlow(
                superToken.address,
                app.address,
                "1000000000000000000",
                "0x"
            ).encodeABI()
        ];

        await web3tx(superfluid.callBatch, "Superfluid.callAppAction")(
            [dataApp, dataAgreement],
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
