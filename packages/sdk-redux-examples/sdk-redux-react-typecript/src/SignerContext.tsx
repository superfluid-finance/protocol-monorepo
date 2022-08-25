import { ethers } from "ethers";
import { createContext } from "react";

export const SignerContext = createContext<
    [chainId: number, signerAddress: string, signer: ethers.Signer]
>(undefined!);
