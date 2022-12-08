const _ = require("lodash");
const getConfig = require("./libs/getConfig");
const {getScriptRunnerFactory: S, getPastEvents} = require("./libs/common");

/**
 * @dev Scans the network for a protocol deployment by looking for AgreementClassRegistered events
 *
 * Usage: npx truffle exec scripts/info-scan-deployments.js
 */

module.exports = eval(`(${S.toString()})()`)(async function () {
    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    const config = getConfig(chainId);

    // infer deployment from AgreementClassRegistered events
    const AgreementClassRegistered = web3.utils.sha3(
        "AgreementClassRegistered(bytes32,address)"
    );
    const events = await getPastEvents({
        config,
        topics: [AgreementClassRegistered],
    });

    const deployments = events.reduce((acc, cur) => {
        acc[cur.address] = _.defaults(acc[cur.address], {
            host: cur.address,
            transactions: new Set(),
            deployers: new Set(),
        });
        acc[cur.address].transactions.add(cur.transactionHash);
        return acc;
    }, {});

    if (deployments.length == 0)
        throw new Error("No Superfluid is ever deployed!?");

    const deployers = new Set();
    await Promise.all(
        Object.values(deployments).map(async (deployment) => {
            const transactions = Array.from(deployment.transactions);
            for (let i = 0; i < transactions.length; ++i) {
                const tx = await web3.eth.getTransaction(transactions[i]);
                deployers.add(tx.from);
                deployment.deployers.add(tx.from);
            }
        })
    );

    console.log("Host contracts:");
    console.log(
        Object.values(deployments).map((i) => {
            return `${i.host} deployed by ${Array.from(i.deployers)}`;
        })
    );

    console.log("All Deployers:");
    console.log(Array.from(deployers));
});
