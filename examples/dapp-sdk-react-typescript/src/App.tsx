import React from 'react';
import AddressAndNetworkForm from "./components/AddressAndNetworkForm";
import {AccountScoper} from "./components/AccountScoper";
import {useState} from 'react';
import {DisplaySelectedAccountAndNetwork} from "./components/DisplaySelectedAccountAndNetwork";

interface AccountScoperProps {
    networkId: number;
    accountAddress: string;
}

function App() {
    const [accountScoperProps, setAccountScoperProps] = useState<AccountScoperProps | null>();

    return (
        <>
            <AddressAndNetworkForm
                onSubmitted={(networkId, accountAddress) => setAccountScoperProps({networkId, accountAddress})}>
            </AddressAndNetworkForm>
            {
                !!accountScoperProps &&
                <AccountScoper networkId={accountScoperProps.networkId}
                               accountAddress={accountScoperProps.accountAddress}>
                    <DisplaySelectedAccountAndNetwork/>
                </AccountScoper>
            }
        </>
    );
}

export default App;
