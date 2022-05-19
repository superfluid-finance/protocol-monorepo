import { createContext } from "react";

export const SignerContext = createContext<
    [chainId: number, signerAddress: string]
>(undefined!);
