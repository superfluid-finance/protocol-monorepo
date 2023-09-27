import { ethers } from "ethers";

import FlowNFTBase from "./FlowNFTBase";
import {
    ConstantOutflowNFT__factory,
    IConstantOutflowNFT,
} from "./typechain-types";

export default class ConstantOutflowNFT extends FlowNFTBase {
    override readonly contract: IConstantOutflowNFT;
    constructor(address: string) {
        super(address);
        this.contract = new ethers.Contract(
            address,
            ConstantOutflowNFT__factory.abi
        ) as IConstantOutflowNFT;
    }
}
