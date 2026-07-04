import {ContractTransaction} from "ethers";

/** Minimal replacement for @decentral.ee/web3-helpers web3tx in tests. */
export async function loggedTx<T extends ContractTransaction>(
    label: string,
    fn: () => Promise<T>
) {
    console.log(`${label}: started`);
    const tx = await fn();
    const receipt = await tx.wait();
    console.log(`${label}: done, gas used ${receipt.gasUsed.toString()}`);
    return {...receipt, tx: receipt.transactionHash};
}
