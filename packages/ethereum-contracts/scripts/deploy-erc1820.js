// Adapted from https://github.com/0xjac/ERC1820/blob/master/js/deployment.js
const assert = require("assert").strict;

const Transaction = require("ethereumjs-tx").Transaction;
const ethUtils = require("ethereumjs-util");
// TODO: this is a weird dependency, should probably get this from elsewhere
const ERC1820Registry = require("./artifacts/ERC1820Registry.json");
const {getScriptRunnerFactory: S, hasCode} = require("./libs/common");

/**
 * @dev Deploy ERC1820 to the network.
 * @param web3 The web3 to be used
 * @param from address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/deploy-erc1820.js
 */
module.exports = eval(`(${S.toString()})({skipArgv: true})`)(async function (
    args,
    options = {}
) {
    web3 = web3 || options.web3;

    const rawTransaction = {
        nonce: 0,
        gasPrice: 100000000000,
        value: 0,
        data: "0x" + ERC1820Registry.bin,
        gasLimit: 800000,
        v: 27,
        r: "0x1820182018201820182018201820182018201820182018201820182018201820",
        s: "0x1820182018201820182018201820182018201820182018201820182018201820",
    };
    const tx = new Transaction(rawTransaction);
    const res = {
        sender: ethUtils.toChecksumAddress(
            "0x" + tx.getSenderAddress().toString("hex")
        ),
        rawTx: "0x" + tx.serialize().toString("hex"),
        contractAddr: ethUtils.toChecksumAddress(
            "0x" +
                ethUtils
                    .generateAddress(
                        tx.getSenderAddress(),
                        ethUtils.toBuffer(0)
                    )
                    .toString("hex")
        ),
    };
    assert.equal("0xa990077c3205cbDf861e17Fa532eeB069cE9fF96", res.sender);
    assert.equal(
        "0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24",
        res.contractAddr
    );

    console.log("Checking ERC1820 deployment at", res.contractAddr);
    if (!(await hasCode(web3, res.contractAddr))) {
        console.log("Deploying...");
        const account = options.from || (await web3.eth.getAccounts())[0];
        console.log("Step 1: send ETH");
        // NOTE: we estimate gas when running test coverage w/ hardhat
        // otherwise there is a weird bug which requires the user to have
        // over 2k ETH to send the transaction
        const estimatedGasObject = {
            from: account,
            to: res.sender,
            value: "100000000000000000", //web3.utils.toWei(0.1)
        };
        if (process.env.IS_HARDHAT) {
            const estimatedGas = await web3.eth.estimateGas(estimatedGasObject);
            await web3.eth.sendTransaction({
                ...estimatedGasObject,
                gas: estimatedGas,
            });
        } else {
            await web3.eth.sendTransaction(estimatedGasObject);
        }
        console.log("Step 2: send signed transaction");
        await web3.eth.sendSignedTransaction(res.rawTx);
        console.log("Deployment done.");
    } else {
        console.log("ERC1820 is already deployed.");
    }
});
