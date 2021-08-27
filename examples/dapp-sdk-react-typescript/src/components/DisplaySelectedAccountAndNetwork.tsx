import { ReactElement } from "react";
import { useContext } from "react";
import { FC } from "react";
import {AccountAndNetworkScopedContext} from "./AccountScoper";

export const DisplaySelectedAccountAndNetwork: FC<any> = (): ReactElement => {
    const accountAndNetworkScopedContext = useContext(AccountAndNetworkScopedContext);

    return (
        <>
            <h1>Selected Account</h1>
            <h2>Network</h2>
            <strong>{accountAndNetworkScopedContext.network.id}</strong>
            <h2>Address</h2>
            <strong>{accountAndNetworkScopedContext.account.accountAddress}</strong>
        </>
    );
};
