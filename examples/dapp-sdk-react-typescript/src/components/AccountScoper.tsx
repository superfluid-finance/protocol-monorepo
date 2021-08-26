import {ReactElement} from "react";
import {Account, Network, RootState} from "dapp-sdk";
import {FC} from "react";
import {createContext} from "react";
import {useSelector} from "react-redux";

export interface AccountAndNetworkScopedContextState {
    network: Network,
    account: Account
}

interface Props {
    networkId: number;
    accountAddress: string;
}

// TODO/NOTE: These should never be NULL for consuming components.
export const AccountAndNetworkScopedContext = createContext<AccountAndNetworkScopedContextState>({
    account: null!,
    network: null!
});

export const AccountScoper: FC<Props> = ({networkId, accountAddress, children}): ReactElement => {
    const {network, account} = useSelector((state: RootState) => {
        return {
            network: state.networks.networks[networkId],
            account: state.accounts.accounts[accountAddress]
        }
    });

    const contextValue = {
        account: account,
        network: network
    };

    return (
        <AccountAndNetworkScopedContext.Provider value={contextValue}>
            {children}
        </AccountAndNetworkScopedContext.Provider>
    );
};
