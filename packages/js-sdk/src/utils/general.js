const validateAddress = (address) => {
    if (!isAddress(address)) throw "Address is invalid";
};

const isAddress = (address) => {
    return /^(0x)?[0-9a-f]{40}$/i.test(address);
};

const getCleanAddress = (address) => {
    validateAddress(address);
    return address.toLowerCase();
};

const completeTransaction = async ({
    sf,
    method,
    args,
    sender,
    onTransaction,
    gasOptions = {},
}) => {
    let tx;
    if (sf.ethers) {
        const tx = await method(...args);
        const receipt = await tx.wait();
        if (receipt.status === 1) onTransaction(receipt.transactionHash);
        tx.receipt = receipt;
    } else {
        tx = await method(...args, {from: sender, ...gasOptions}).on(
            "transactionHash",
            onTransaction
        );
    }
    return tx;
};

module.exports = {
    validateAddress,
    isAddress,
    getCleanAddress,
    completeTransaction,
};
