import { ReactElement } from "react";
import { useContext } from "react";
import { FC } from "react";
import {AccountAndNetworkScopedContext} from "./AccountScoper";

export const DisplaySelectedAccountAndNetwork: FC<any> = (): ReactElement => {
    const accountAndNetworkScopedContext = useContext(AccountAndNetworkScopedContext);

    return (
        <>
            <p>Selected network ID: {accountAndNetworkScopedContext.network.id}</p>
            <p>Selected account address: {accountAndNetworkScopedContext.account.address}</p>
        </>
    );
};
