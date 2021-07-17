export function validateAddress(address: any): void;
export function isAddress(address: any): boolean;
export function getCleanAddress(address: any): any;
export function completeTransaction({ sf, method, args, sender, onTransaction, }: {
    sf: any;
    method: any;
    args: any;
    sender: any;
    onTransaction: any;
}): Promise<any>;
