const {getScriptRunnerFactory: S} = require("./libs/common");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const Resolver = artifacts.require("Resolver");

const SuperfluidLoader = artifacts.require("SuperfluidLoader");
const AgreementForwarder = artifacts.require("AgreementForwarder");

/**
 * @dev Deploy specified contract at a deterministic address (defined by sender, nonce)
 * This facilitates deployment of the contract at the same address across networks.
 * In order to make this easy, the script takes a private key of the deployer account and funds it.
 * Recommendation: create a new account (guaranteed to be at nonce 1 everywhere) for the deployer.
 *
 * @param web3 The web3 instance to be used
 * @param from address to use for funding the deployer account
 *
 * Usage: npx truffle exec scripts/deploy-deterministically.js : {PRIVATE KEY} {CONTRACT NAME} [{NONCE}]
 *        CONTRACT NAME must be one of SuperfluidLoader, AgreementForwarder
 *        If NONCE is not defined, 1 is assumed (-> first tx done from the deployer account)
 *
 * (optional) ENV vars:
 *        GASLIMIT: override the gas limit for networks with misbehaving estimation (Arbitrum)
 *        ESTTXCOST: override the estimated tx cost (amount to be sent to deployer)
 *                   for networks with different cost derivation structure (Optimism)
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    let {protocolReleaseVersion} = options;

    console.log("======== Deploying deterministically ========");

    let nonce = 1;
    if (args.length === 3) {
        nonce = parseInt(args.pop());
        if (nonce <= 0) {
            console.error("nonce must be > 0");
            process.exit(1);
        }
    } else if (args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }
    const contractName = args.pop();
    const privKey = args.pop();

    const deployer = web3.eth.accounts.privateKeyToAccount(privKey);
    console.log("deployer:", deployer.address);
    console.log("nonce:", nonce);

    const chainId = await web3.eth.getChainId();
    const resolverAddr = SuperfluidSDK.getConfig(
        chainId,
        protocolReleaseVersion
    ).resolverAddress;
    console.log("resolver addr:", resolverAddr);
    const resolver = await Resolver.at(resolverAddr);
    const hostAddr = await resolver.get(`Superfluid.${protocolReleaseVersion}`);
    console.log("host addr:", hostAddr);

    let ContractArtifact;
    let deployArgs;
    if (contractName === "SuperfluidLoader") {
        ContractArtifact = SuperfluidLoader;
        deployArgs = [resolverAddr];
        console.log(
            `setting up SuperfluidLoader for chainId ${chainId}, resolver ${resolverAddr}`
        );
    } else if (contractName === "AgreementForwarder") {
        ContractArtifact = AgreementForwarder;
        deployArgs = [hostAddr];
        console.log(
            `setting up AgreementForwarder for chainId ${chainId}, host ${hostAddr}`
        );
    } else {
        throw new Error("Contract unknown / not supported");
    }

    const deployerTxCnt = await web3.eth.getTransactionCount(deployer.address);
    if (nonce !== deployerTxCnt + 1) {
        console.error(
            `### ERR: requested nonce is ${nonce}, but next usable nonce is ${
                deployerTxCnt + 1
            }`
        );
        process.exit(1);
    }

    const deployerBalance = await web3.eth.getBalance(deployer.address);
    console.log("deployer balance:", web3.utils.fromWei(deployerBalance));

    const gasPrice = await web3.eth.getGasPrice();
    console.log("gas price:", gasPrice);

    const Contract = new web3.eth.Contract(ContractArtifact.abi);
    const deployTx = Contract.deploy({
        data: ContractArtifact.bytecode,
        arguments: deployArgs,
    });

    const gasLimit = process.env.GASLIMIT || (await deployTx.estimateGas());
    console.log("gas limit:", gasLimit);

    const deployCode = deployTx.encodeABI();

    const BN = web3.utils.BN;
    // calc delta between deployer balance and estimated deployment cost
    // On Optimism, this won't reflect the actual cost bcs of missing L1 component, see https://community.optimism.io/docs/developers/build/transaction-fees/#estimating-the-total-fee
    const estTxCost = process.env.ESTTXCOST
        ? new BN(process.env.ESTTXCOST)
        : new BN(gasLimit).mul(new BN(gasPrice));
    console.log("estimated tx cost:", estTxCost.toString());
    const missingBalance = estTxCost.sub(new BN(deployerBalance));
    if (missingBalance.gt(new BN(0))) {
        console.log("deployer missing balance", missingBalance.toString());
        console.log("sending funding tx...");
        const fundingTx = await web3.eth.sendTransaction({
            from: options.from || (await web3.eth.getAccounts())[0],
            to: deployer.address,
            value: missingBalance,
        });
        console.log(
            `funded deployer account in tx ${fundingTx.transactionHash}`
        );
    } else {
        console.log("deployer account is already sufficiently funded");
    }

    // check if EIP-1559 enabled
    const lastBlock = await web3.eth.getBlock(await web3.eth.getBlockNumber());
    const hasEip1559 = lastBlock.baseFeePerGas !== undefined;

    const unsignedTx = {
        data: deployCode,
        gas: gasLimit,
    };
    if (hasEip1559) {
        console.log("creating EIP-1559 tx");
        unsignedTx.maxFeePerGas = gasPrice;
        unsignedTx.maxPriorityFeePerGas = gasPrice; // may cause overpaying, but not a big deal here
    } else {
        console.log("creating non-EIP-1559 tx");
        unsignedTx.gasPrice = gasPrice;
    }

    const signedTx = await web3.eth.accounts.signTransaction(
        unsignedTx,
        privKey
    );
    console.log("signed tx with tx hash:", signedTx.transactionHash);

    console.log("sending deploy tx...");
    const deployTxReceipt = await web3.eth.sendSignedTransaction(
        signedTx.rawTransaction
    );
    console.log("contract deployed at:", deployTxReceipt.contractAddress);
});
