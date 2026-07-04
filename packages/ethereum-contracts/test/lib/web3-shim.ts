import {ethers} from "ethers";

function getHre() {
    return require("hardhat") as typeof import("hardhat");
}

/** Ethers-backed subset of web3 v1 used by legacy tests (no hardhat-web3 plugin). */
export const web3 = {
    eth: {
        abi: {
            encodeParameter: (type: string, value: unknown) =>
                ethers.utils.defaultAbiCoder.encode([type], [value]),
            encodeParameters: (types: string[], values: unknown[]) =>
                ethers.utils.defaultAbiCoder.encode(types, values),
        },
        getAccounts: async () => {
            const signers = await getHre().ethers.getSigners();
            return signers.map((s) => s.address);
        },
        getTransactionReceipt: (hash: string) =>
            getHre().ethers.provider.getTransactionReceipt(hash),
        getTransaction: async (hash: string) => {
            const tx = await getHre().ethers.provider.getTransaction(hash);
            if (!tx) {
                return tx;
            }
            const gasPrice =
                tx.gasPrice ??
                tx.maxFeePerGas ??
                (await getHre().ethers.provider.getGasPrice());
            return {...tx, gasPrice, transactionHash: tx.hash};
        },
        getBalance: (address: string) =>
            getHre().ethers.provider.getBalance(address),
        getCode: (address: string) => getHre().ethers.provider.getCode(address),
        sendTransaction: async (tx: {
            from?: string;
            to?: string;
            value?: string | number;
            data?: string;
            gas?: string | number;
        }) => {
            const signers = await getHre().ethers.getSigners();
            const signer =
                tx.from !== undefined
                    ? (signers.find(
                          (s) =>
                              s.address.toLowerCase() === tx.from!.toLowerCase()
                      ) ?? signers[0])
                    : signers[0];
            const sent = await signer.sendTransaction({
                to: tx.to,
                value:
                    tx.value !== undefined
                        ? (() => {
                              if (
                                  typeof tx.value === "string" &&
                                  !tx.value.startsWith("0x") &&
                                  /^[0-9a-fA-F]+$/.test(tx.value)
                              ) {
                                  return ethers.BigNumber.from(`0x${tx.value}`);
                              }
                              return ethers.utils.hexValue(tx.value);
                          })()
                        : undefined,
                data: tx.data,
                gasLimit: tx.gas,
            });
            return {
                hash: sent.hash,
                ...sent,
            };
        },
        net: {
            getId: async () => getHre().network.config.chainId,
            getChainId: async () => getHre().network.config.chainId,
            getNetworkType: async () => "private",
        },
    },
    utils: {
        sha3: (input: string) => ethers.utils.id(input),
        soliditySha3: (...args: unknown[]) => {
            if (args.length === 1 && typeof args[0] === "string") {
                return ethers.utils.id(args[0]);
            }
            const types = args.filter((_, i) => i % 2 === 0) as string[];
            const values = args.filter((_, i) => i % 2 === 1);
            return ethers.utils.solidityKeccak256(types, values);
        },
        toBN: (value: ethers.BigNumberish) => {
            if (
                typeof value === "string" &&
                !value.startsWith("0x") &&
                /^[0-9a-fA-F]+$/.test(value)
            ) {
                return ethers.BigNumber.from(`0x${value}`);
            }
            return ethers.BigNumber.from(value);
        },
        fromWei: (value: ethers.BigNumberish, unit?: string) =>
            ethers.utils.formatUnits(value, unit === "gwei" ? "gwei" : 18),
    },
    get currentProvider() {
        return getHre().network.provider;
    },
};
