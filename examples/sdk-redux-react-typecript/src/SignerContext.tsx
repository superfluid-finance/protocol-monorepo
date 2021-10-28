import { createContext } from "react";
import {ChainId} from "@superfluid-finance/sdk-core";

export const SignerContext = createContext<
    [chainId: ChainId, signerAddress: string]
>(undefined!);
