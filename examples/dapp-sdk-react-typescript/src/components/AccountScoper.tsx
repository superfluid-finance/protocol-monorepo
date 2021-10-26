import { ReactElement, useState } from "react";
import { Account, Network, DAppSdkRootState } from "sdk-redux";
import { FC } from "react";
import { createContext } from "react";
import { useDispatch, useSelector } from "react-redux";
import { StreamIndex } from "dapp-sdk/build/main/Account";
import { fetchAccountStreamIndexes } from "dapp-sdk/build/main/mainSlice";

export interface AccountAndNetworkScopedContextState {
    network: Network;
    account: Account;
    activeStreams?: Array<StreamIndex>;
    inactiveStreams?: Array<StreamIndex>;
}

interface Props {
    networkId: number;
    accountAddress: string;
}

// TODO/NOTE: These should never be NULL for consuming components.
export const AccountAndNetworkScopedContext =
    createContext<AccountAndNetworkScopedContextState>({
        account: null!,
        network: null!,
    });

export const AccountScoper: FC<Props> = ({
    networkId,
    accountAddress,
    children,
}): ReactElement => {
    const { network, account } = useSelector((state: DAppSdkRootState) => {
        return {
            isLoading: state.normalizedData.isLoading,
            network: state.normalizedData.networks[networkId],
            account:
                state.normalizedData.networks[networkId].accounts[
                    accountAddress
                ],
        };
    });
    const dispatch = useDispatch();
    const [hasFetchedActive, setHasFetchedActive] = useState(false);
    const [hasFetchedInactive, setHasFetchedInactive] = useState(false);

    const contextValue = {
        account: account,
        network: network,
        activeStreams: account.activeStreams,
        inactiveStreams: account.inactiveStreams,
    };

    if (!account.activeStreams && !hasFetchedActive) {
        dispatch(
            fetchAccountStreamIndexes({
                chainId: account.chainId,
                accountAddress: account.accountAddress,
                isActive: true,
            })
        );
        setHasFetchedActive(true);
    } else {
        if (
            account.activeStreams &&
            !account.inactiveStreams &&
            !hasFetchedInactive
        ) {
            dispatch(
                fetchAccountStreamIndexes({
                    chainId: account.chainId,
                    accountAddress: account.accountAddress,
                    isActive: false,
                })
            );
            setHasFetchedInactive(true);
        }
    }

    return (
        <AccountAndNetworkScopedContext.Provider value={contextValue}>
            {children}
        </AccountAndNetworkScopedContext.Provider>
    );
};
