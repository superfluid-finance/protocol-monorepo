import { createContext } from "react";
import { NetworkName } from "@superfluid-finance/sdk-core"

export const SignerContext = createContext<
    [networkName: NetworkName, signerAddress: string]
>(undefined!);
