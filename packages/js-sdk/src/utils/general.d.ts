import type { Framework } from '../Framework';
import type { Transaction } from 'web3-core';
import { GasOptions } from "../types/gasOptions";

export function validateAddress(address: string): void;
export function isAddress(address: string): boolean;
export function getCleanAddress(address: string): string;
export function completeTransaction({ sf, method, args, sender, onTransaction, gasOptions }: {
    sf: Framework;
    method: () => Transaction;
    args: Array<string>;
    sender: string;
    onTransaction: () => any;
    gasOptions?: GasOptions;
}): Promise<Transaction>;
