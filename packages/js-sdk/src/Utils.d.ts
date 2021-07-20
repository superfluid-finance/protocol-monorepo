import type BN from "bn.js";
declare class Utils {
    constructor(sf: any);
    _sf: any;
    normalizeTokenParam(param: string): string;
    normalizeAddressParam(param: string): string;
    normalizeFlowRateParam(param: BN | string): BN;
}

export = Utils;