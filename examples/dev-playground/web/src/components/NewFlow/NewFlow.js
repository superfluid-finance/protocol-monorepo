import { useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import FlowForm from 'src/components/FlowForm'
import { useAuth } from '@redwoodjs/auth'

import { Web3Provider } from '@ethersproject/providers'
import SuperfluidSDK from '@superfluid-finance/js-sdk'

const NewFlow = ({ to }) => {
  const { currentUser } = useAuth()
  const { addMessage } = useFlash()

  const [error, setError] = React.useState(null)
  const [loading, setLoading] = React.useState(false)

  React.useEffect(() => {
    newFlow()
  },[])

  const newFlow = async (input) => {
    setLoading(true)

    const walletProvider = new Web3Provider(window.ethereum)
    const sf = new SuperfluidSDK.Framework({
      version: 'v1', // Protocol release version
      web3Provider: walletProvider, // your web3 provider
    })
    await sf.initialize()
    // const owner = sf.user({
    //   address: input.ownerAddress,
    //   token: input.tokenAddress,
    // })
    // const tx = await owner.flow({
    //   recipient: input.recipientAddress,
    //   flowRate: input.flowRate,
    // })
    // await tx.wait()
    //
    // setLoading(false)
    // addMessage('Flow created.', { classes: 'rw-flash-success' })
    // navigate(
    //   routes.flow({ from: input.ownerAddress, to: input.recipientAddress })
    // )
  }

  return (
    <div className="rw-segment">
      <header className="rw-segment-header">
        <h2 className="rw-heading rw-heading-secondary">New Flow</h2>
      </header>
      <div className="rw-segment-main">
        <FlowForm
          flow={{
            recipientAddress: to,
            ownerAddress: currentUser.address,
          }}
          onSave={newFlow}
          loading={loading}
          error={error}
        />
      </div>
    </div>
  )
}

export default NewFlow
