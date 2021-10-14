import type { Framework } from '../Framework';
import type { Transaction } from 'web3-core';

export function validateAddress(address: string): void;
export function isAddress(address: string): boolean;
export function getCleanAddress(address: string): string;
export function completeTransaction({ sf, method, args, sender, onTransaction }: {
    sf: Framework;
    method: () => Transaction;
    args: Array<string>;
    sender: string;
    onTransaction: () => any;
}): Promise<Transaction>;
