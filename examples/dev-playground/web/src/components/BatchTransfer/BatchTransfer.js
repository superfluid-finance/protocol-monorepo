import BatchTransferForm from 'src/components/BatchTransferForm'
import { useAuth } from '@redwoodjs/auth'

import { Web3Provider } from '@ethersproject/providers'
import SuperfluidSDK from '@superfluid-finance/js-sdk'

const BatchTransfer = ({ token }) => {
  const { currentUser } = useAuth()

  const [error, setError] = React.useState(null)
  const [loading, setLoading] = React.useState(false)

  const sf = new SuperfluidSDK.Framework({
    ethers: new Web3Provider(window.ethereum),
  })

  const batchTransfer = async (input) => {
    setLoading(true)

    await sf.initialize()

    setLoading(false)
  }

  return (
    <>
      <div className="rw-segment">
        <header className="rw-segment-header">
          <h2 className="rw-heading rw-heading-secondary">Batch Transfer</h2>
        </header>
        <div className="rw-segment-main">
          <BatchTransferForm
            tokenAddress={token}
            onSave={batchTransfer}
            loading={loading}
            error={error}
          />
        </div>
      </div>
    </>
  )
}

export default BatchTransfer
