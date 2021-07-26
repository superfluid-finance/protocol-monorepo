import { Web3Provider } from '@ethersproject/providers'
import SuperfluidSDK from '@superfluid-finance/js-sdk'
import { useState } from 'react'

type NewFlowProps = {
    address: string
    token: string
}

export default function NewFlow({ address, token }: NewFlowProps) {

    // metamask injects .ethereum into window
    const windowWeb3 = window as any
    const [recipient, setRecipient] = useState<string>('')
    const [flowRate, setFlowRate] = useState<string>('')

    const createFlow = async () => {
        if (recipient && flowRate) {
            const sf = new SuperfluidSDK.Framework({
                ethers: new Web3Provider(windowWeb3.ethereum)
            })
            await sf.initialize()
            const user = sf.user({ address, token })
            await user.flow({ recipient, flowRate })
        }
    }

    return (
        <div className='card'>
            <div className='card-header'>
                <h3>Create New Flow</h3>
            </div>
            <div className='card-content'>
                <p>Recipient Address: </p>
                <input
                    onChange={e => setRecipient(e.target.value)}
                    value={recipient}
                />
                <p>Flow Rate: </p>
                <div style={{ display: 'flex' }}>
                    <input
                        onChange={e => setFlowRate(e.target.value)}
                        value={flowRate}
                    />
                    <p style={{ marginLeft: 4 }}>per second</p>
                </div>
                <button
                    onClick={createFlow}
                    style={{ marginTop: 8 }}
                >
                    Sumbit
                </button>
            </div>
        </div>
    )
}