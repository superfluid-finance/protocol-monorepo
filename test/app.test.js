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
        await web3tx(superfluid.setWhiteList, "Superfluid.setWhitedListed")(
            app.address,{
                from: accounts[1]
            }
        );

        await web3tx(
            flowAgreement.updateFlow,
            "updateFlow"
        )(superToken.address, accounts[1], app.address, FLOW_RATE, {from: accounts[1]});

        const superTokenAddr = superToken.address.toString();
        const receivers = accounts.slice(2, 4);
        const flowRates = new Array(10000000, 50000000000);

        //start a multi party
        await web3tx(app.createMultiFlows, "MultiApp.createMultiFlows")(
            superTokenAddr,
            receivers,
            flowRates,{
                from: accounts[1]
            }
        );

        await web3tx(flowAgreement.updateFlow, "FlowAgreement.updateFlow")(
            superTokenAddr,
            accounts[1],
            receivers[1],
            //app.address,
            "100000000000", {
                from: accounts[1],
                gas: 1e6,
            }
        );

        let tx = await web3tx(flowAgreement.deleteFlow, "FlowAgreement.updateFlow")(
            superTokenAddr,
            accounts[1],
            receivers[1],
            //app.address,
            {
                from: accounts[1],
                gas: 1e6,
            }
        );

        console.log("!!!!!!", tx.logs);
    });
});
