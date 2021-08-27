import React from 'react';
import AddressAndNetworkForm from "./components/AddressAndNetworkForm";
import {AccountScoper} from "./components/AccountScoper";
import {useState} from 'react';
import {DisplaySelectedAccountAndNetwork} from "./components/DisplaySelectedAccountAndNetwork";
import {useDispatch, useSelector} from "react-redux";
import {RootState} from "dapp-sdk";
import {Streams} from "./components/Streams";
import {fetchAccount} from "dapp-sdk/build/main/mainSlice";

interface AccountScoperProps {
    networkId: number;
    accountAddress: string;
}

function App() {
    const [accountScoperProps, setAccountScoperProps] = useState<AccountScoperProps | null>();
    const error = useSelector((state: RootState) => state.main.error)
    const isLoading = useSelector((state: RootState) => state.main.isLoading)
    const isAccountLoaded = useSelector((state: RootState) => state.main.networks[accountScoperProps?.networkId || 0]?.accounts[accountScoperProps?.accountAddress || 0])
    // const useThunkDispatch = () => useDispatch<typeof store.dispatch>();
    // const thunkDispatch = useThunkDispatch();
    const dispatch = useDispatch();

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
                    dispatch(fetchAccount({
                        networkId,
                        accountAddress
                    }))
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
