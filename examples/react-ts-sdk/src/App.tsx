import { useState, useEffect } from 'react'
import Navbar from './components/Navbar'
import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { Web3Provider } from '@ethersproject/providers'
import type { DetailsType } from '@superfluid-finance/js-sdk/src/User'

function App() {
    // metamask injects .ethereum into window
    const windowWeb3 = window as any
    const [address, setAddress] = useState<string>('')
    const [userDetails, setUserDetails] = useState<undefined|DetailsType>()
    const fDAIx = '0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00'

    useEffect(() => {
        if (address !== '') {
            getDetails()
        }
    }, [address])

    const handleWallet = async () => {
        const walletAddr = await windowWeb3.ethereum.request({
            method: 'eth_requestAccounts',
            params: [{ eth_accounts: {} }]
        })
        setAddress(walletAddr[0])
    }

    const getUser = async () => {
    }

    const getDetails = async () => {
        const sf = new SuperfluidSDK.Framework({
            ethers: new Web3Provider(windowWeb3.ethereum)
        })
        await sf.initialize()
        const user = sf.user({
            address,
            token: fDAIx
        })

        const details = await user.details()
        setUserDetails(details)
    }

    const flowList = () => {
        if (userDetails !== undefined) {
            const { inFlows, outFlows } = userDetails.cfa.flows
            const allFlows = inFlows.concat(outFlows)
            return allFlows.map(flow => {
                return (
                    <div>
                        <p>Sender: {flow.sender}</p>
                        <p>Receiver: {flow.receiver}</p>
                        <p>Flow Rate: {flow.flowRate}</p>
                    </div>
                )
            })
        } else {
            return null
        }
    }

    return (
        <main>
            <Navbar/>
            <div className='content'>
                {
                    userDetails !== undefined ? (
                        <div className='card'>
                            <div className='card-header'>
                                <h3>Superfluid User Details</h3>
                            </div>
                            <div className='card-content'>
                                <p>Net Flow: {userDetails.cfa.netFlow}</p>
                                {flowList}
                            </div>
                        </div>
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

export default App;
