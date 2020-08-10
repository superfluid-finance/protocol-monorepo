const Tester = require("./Tester");
const MultiApp = artifacts.require("MultiFlowsApp");

const {
    web3tx
} = require("@decentral.ee/web3-helpers");

contract("Superfluid App", accounts => {

    it("#1 MultiFlowsApp", async () => {
        const tester = new Tester(accounts);
        await tester.resetContracts();
        const app = await web3tx(MultiApp.new, "MultiApp.new")(tester.contracts.flowAgreement.address);
        await web3tx(tester.contracts.superfluid.setWhiteList, "Superfluid.setWhitedListed")(
            app.address,{
                from: accounts[1]
            }
        );

        const superTokenAddr = tester.contracts.superToken.address.toString();
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

        console.log(receivers);
        let tx = await web3tx(tester.contracts.flowAgreement.updateFlow, "FlowAgreement.updateFlow")(
            superTokenAddr,
            accounts[1],
            receivers[1],
            //app.address,
            "100000000000", {
                from: accounts[1],
                gas: 1e6,
            }
        );

        tx = await web3tx(tester.contracts.flowAgreement.deleteFlow, "FlowAgreement.updateFlow")(
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
