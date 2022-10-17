import {BigNumber} from "ethers";
import {ethers} from "hardhat";

export const toBN = (x: any) => ethers.BigNumber.from(x);

export const toWad = (x: any) => ethers.utils.parseUnits(x.toString(), 18);

export const max = (a: BigNumber, b: BigNumber) => (a.gt(b) ? a : b);
export const min = (a: BigNumber, b: BigNumber) => (a.gt(b) ? b : a);

export const keccak256 = (x: string) =>
    ethers.utils.keccak256(ethers.utils.toUtf8Bytes(x));
