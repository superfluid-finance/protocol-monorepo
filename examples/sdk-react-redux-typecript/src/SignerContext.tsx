import { createContext } from "react";

export const SignerContext = createContext<
    [networkName: string, signerAddress: string]
>(undefined!);
