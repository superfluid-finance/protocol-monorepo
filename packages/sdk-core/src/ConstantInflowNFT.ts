import { ethers } from "ethers";

import FlowNFTBase from "./FlowNFTBase";
import {
    ConstantInflowNFT__factory,
    IConstantInflowNFT,
} from "./typechain-types";

export default class ConstantInflowNFT extends FlowNFTBase {
    override readonly contract: IConstantInflowNFT;
    constructor(address: string) {
        super(address);
        this.contract = new ethers.Contract(
            address,
            ConstantInflowNFT__factory.abi
        ) as IConstantInflowNFT;
    }
}
