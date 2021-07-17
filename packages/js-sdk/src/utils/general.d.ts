import type Framework from '../Framework'
interface TX {
    action: string
    txHash: string
    gas: BN
    gasPrice: BN
    cost: BN
}

export function validateAddress(address: string): void;
export function isAddress(address: string): boolean;
export function getCleanAddress(address: string): string;
export function completeTransaction({ sf, method, args, sender, onTransaction }: {
    sf: Framework;
    method: () => TX;
    args: Array<string>;
    sender: string;
    onTransaction: () => null;
}): Promise<TX>;
