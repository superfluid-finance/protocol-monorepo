import {ethers as hardhatEthers} from "hardhat";
import ethers from "ethers";

export const toBN = (x: ethers.BigNumberish) => hardhatEthers.BigNumber.from(x);

export const toWad = (x: string) =>
    hardhatEthers.utils.parseUnits(x.toString(), 18);

export const max = (a: ethers.BigNumber, b: ethers.BigNumber) =>
    a.gt(b) ? a : b;
export const min = (a: ethers.BigNumber, b: ethers.BigNumber) =>
    a.gt(b) ? b : a;
