import BatchTransferForm from 'src/components/BatchTransferForm'
import { useAuth } from '@redwoodjs/auth'
import { batchTransfer as callBatchTransfer } from 'src/utils/batchTransfer'
import toast from 'react-hot-toast'

const BatchTransfer = ({ token }) => {
  const { currentUser } = useAuth()

  const [error, setError] = React.useState(null)
  const [loading, setLoading] = React.useState(false)
  const [transactionHashes, setTransactionHashes] = React.useState([])

  const batchTransfer = async (input) => {
    setLoading(true)
    let recipients = []
    let amounts = []
    input.recipientData.split(/\n/).map((item) => {
      const [recipient, amount] = item.split(/\s/)
      if (!recipient || !amount) throw Error('At least one entry is invalid')
      recipients.push(recipient)
      amounts.push(amount)
    })
    // toast.error('Something went wrong')
    // token
    const { tx, error } = await callBatchTransfer({
      recipients,
      amounts,
      tokenAddress: token,
    })

    if (error) return toast.error(error)

    toast.promise(tx.wait(), {
      loading: 'Waiting for confirmation',
      success: (receipt) => {
        setTransactionHashes([...transactionHashes, receipt.transactionHash])
        setLoading(false)
        return <b>Complete!</b>
      },
      error: (err) => {
        setLoading(false)
        console.log(err)
        return <b>Something went wrong. {err?.message}</b>
      },
    })
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
        <p>Transactions:</p>
        <ul>
          {transactionHashes.map((hash) => (
            <li>{hash}</li>
          ))}
        </ul>
      </div>
    </>
  )
}

export default BatchTransfer
