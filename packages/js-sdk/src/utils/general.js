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
    method,
    args,
    sender,
    onTransaction,
    gasOptions = {},
}) => {
    let tx;
    tx = await method(...args, {from: sender, ...gasOptions}).on(
        "transactionHash",
        onTransaction
    );
    return tx;
};

module.exports = {
    validateAddress,
    isAddress,
    getCleanAddress,
    completeTransaction,
};
