import {
    ConstantInflowNFT__factory,
    IConstantInflowNFT,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { ethers } from "ethers";

import FlowNFTBase from "./FlowNFTBase";

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
