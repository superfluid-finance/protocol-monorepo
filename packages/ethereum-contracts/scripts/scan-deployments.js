const _ = require("lodash");
const { detectTruffleAndConfigure } = require("./utils");

module.exports = async function (callback) {
    try {
        await eval(`(${detectTruffleAndConfigure.toString()})({})`);

        // infer deployment from AgreementClassRegistered revents
        const AgreementClassRegistered = web3.utils.sha3(
            "AgreementClassRegistered(bytes32,address)"
        );
        const AgreementClassUpdated = web3.utils.sha3(
            "AgreementClassUpdated(bytes32,address)"
        );
        const events = [
            ...(await web3.eth.getPastLogs({
                fromBlock: "0",
                toBlock: "latest",
                topics: [AgreementClassRegistered],
            })),
            ...(await web3.eth.getPastLogs({
                fromBlock: "0",
                toBlock: "latest",
                topics: [AgreementClassUpdated],
            })),
        ];

        const deployments = events.reduce((acc, cur) => {
            acc[cur.address] = _.defaults(acc[cur.address], {
                host: cur.address,
                transactions: new Set(),
                deployers: new Set(),
            });
            acc[cur.address].transactions.add(cur.transactionHash);
            return acc;
        }, {});

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

        callback();
    } catch (err) {
        callback(err);
    }
};
