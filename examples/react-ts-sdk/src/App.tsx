import { useState, useEffect, useCallback } from 'react'
import Navbar from './components/Navbar'
import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { Web3Provider } from '@ethersproject/providers'
import type { DetailsType } from '@superfluid-finance/js-sdk'
import Details from './components/Details'
import NewFlow from './components/NewFlow'

function App() {

    // metamask injects .ethereum into window
    const windowWeb3 = window as any
    const [address, setAddress] = useState<string>('')
    const [userDetails, setUserDetails] = useState<undefined|DetailsType>()
    const fUSDCx = '0x8ae68021f6170e5a766be613cea0d75236ecca9a'

    const getDetails = useCallback(
        async () => {
            const sf = new SuperfluidSDK.Framework({
                ethers: new Web3Provider(windowWeb3.ethereum)
            })
            await sf.initialize()
            const user = sf.user({
                address,
                token: fUSDCx
            })

            const details = await user.details()
            setUserDetails(details)
        },
        [address, windowWeb3.ethereum]
    )

    useEffect(() => {
        if (address !== '') {
            getDetails()
        }
    }, [address, getDetails])

    const handleWallet = async () => {
        const walletAddr = await windowWeb3.ethereum.request({
            method: 'eth_requestAccounts',
            params: [{ eth_accounts: {} }]
        })
        setAddress(walletAddr[0])
    }

    return (
        <main>
            <Navbar/>
            <div className='content'>
                {
                    userDetails !== undefined ? (
                        <>
                            <Details
                                address={address}
                                netFlow={userDetails.cfa.netFlow}
                                inFlows={userDetails.cfa.flows.inFlows}
                                outFlows={userDetails.cfa.flows.outFlows}
                            />
                            <NewFlow address={address} token={fUSDCx} />
                        </>
                    ) : (
                        <div className='card'>
                            <div className='card-header'>
                                <h3>Connect Wallet</h3>
                            </div>
                            <div className='card-content'>
                                <button onClick={handleWallet}>Metamask</button>
                            </div>
                        </div>
                    )
                }
            </div>
        </main>
    )
}

export default App