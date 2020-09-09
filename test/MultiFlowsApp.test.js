const Tester = require("./Tester");
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

        const aliceNetFlow = await cfa.getNetFlow.call(superToken.address, alice);
        const bobNetFlow = await cfa.getNetFlow.call(superToken.address, bob);
        const carolNetFlow = await cfa.getNetFlow.call(superToken.address, carol);

        let aliceDeposit = await superToken.getDeposit.call(cfa.address, alice);
        let bobDeposit = await superToken.getDeposit.call(cfa.address, bob);
        let appDeposit = await superToken.getDeposit.call(cfa.address, app.address);

        console.log("Alice Deposit", aliceDeposit[0].toString(), aliceDeposit[1].toString());
        console.log("Bob Deposit", bobDeposit[0].toString(), bobDeposit[1].toString());
        console.log("App Deposit", appDeposit[0].toString(), appDeposit[1].toString());

        assert.equal(aliceNetFlow.toString(), -FLOW_RATE, "Alice net flow is wrong");
        assert.equal(bobNetFlow, FLOW_RATE * 6 / 10, "Bob net flow is wrong");
        assert.equal(carolNetFlow, FLOW_RATE * 4 / 10, "Carol net flow is wrong");
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

        console.log("Alice", aliceNetFlowAfter.toString());
        console.log("Bob", bobNetFlowAfter.toString());
        console.log("Carol", carolNetFlowAfter.toString());
        console.log("App", appNetFlowAfter);

        aliceDeposit = await superToken.depositBalanceOf.call(alice);
        bobDeposit = await superToken.depositBalanceOf.call(bob);
        appDeposit = await superToken.depositBalanceOf.call(app.address);

        console.log("Alice Deposit", aliceDeposit.toString());
        console.log("Bob Deposit", bobDeposit.toString());
        console.log("App Deposit", appDeposit.toString());
    });

    it("#2 MultiFlowsApp Batch Call", async () => {

        await superToken.upgrade(INIT_BALANCE, {from: accounts[1]});
        const app = await web3tx(MultiApp.new, "MultiApp.new")(cfa.address, superfluid.address);
        const dataApp = [4,
            app.address,
            app.contract.methods.createMultiFlows(superToken.address, [bob, carol], [6, 4], "0x").encodeABI()
        ];
        const dataAgreement = [3,
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
