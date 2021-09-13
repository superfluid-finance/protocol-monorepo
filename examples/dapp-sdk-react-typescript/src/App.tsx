import React from 'react';
import AddressAndNetworkForm from "./components/AddressAndNetworkForm";
import {AccountScoper} from "./components/AccountScoper";
import {useState} from 'react';
import {DisplaySelectedAccountAndNetwork} from "./components/DisplaySelectedAccountAndNetwork";
import { useSelector} from "react-redux";
import { DAppSdkRootState } from "dapp-sdk";

import {Streams} from "./components/Streams";

interface AccountScoperProps {
    networkId: number;
    accountAddress: string;
}

function App() {
    const [accountScoperProps, setAccountScoperProps] = useState<AccountScoperProps | null>();
    const error = useSelector((state: DAppSdkRootState) => state.normalizedData.error)
    const isLoading = useSelector((state: DAppSdkRootState) => state.normalizedData.isLoading)
    const isAccountLoaded = useSelector((state: DAppSdkRootState) => state.normalizedData.networks[accountScoperProps?.networkId || 0]?.accounts[accountScoperProps?.accountAddress || 0])

    return (
        <>
            <h1>Dummy data info</h1>
            <div>
                <h2>All accounts</h2>
                <table>
                    <thead>
                    <tr>
                        <th>Network</th>
                        <th>Account</th>
                    </tr>
                    </thead>

                    <tbody>

                    <tr>
                        <td>137</td>
                        <td>address1</td>
                    </tr>
                    <tr>
                        <td>421611</td>
                        <td>address2</td>
                    </tr>
                    <tr>
                        <td>137</td>
                        <td>address3</td>
                    </tr>
                    </tbody>

                </table>
            </div>
            {
                isLoading && <><h1>Global info</h1><strong>Loading...</strong></>
            }
            {
                error && !isLoading && <><h1>Global info</h1><strong>{error}</strong></>
            }
            <AddressAndNetworkForm
                onSubmitted={(networkId, accountAddress) => {
                    setAccountScoperProps({networkId, accountAddress})
                }}>
            </AddressAndNetworkForm>
            {
                accountScoperProps && isAccountLoaded &&
                <AccountScoper networkId={accountScoperProps.networkId}
                               accountAddress={accountScoperProps.accountAddress}>
                    <DisplaySelectedAccountAndNetwork/>
                    <Streams/>
                </AccountScoper>
            }
        </>
    );
}

export default App;
