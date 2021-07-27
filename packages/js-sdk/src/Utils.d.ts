import Framework from "./Framework";
import type BN from 'bn.js'
declare class Utils {
    constructor(sf: Framework);
    _sf: Framework;
    normalizeTokenParam(param: string): string;
    normalizeAddressParam(param: string): string;
    normalizeFlowRateParam(param: string): BN;
}

export = Utils;