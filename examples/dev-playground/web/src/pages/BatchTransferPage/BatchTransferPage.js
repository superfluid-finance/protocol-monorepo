import { Link, routes } from '@redwoodjs/router'
import BatchTransferForm from 'src/components/BatchTransferForm'

const BatchTransferPage = ({ token }) => {
  return (
    <>
      <h1>Batch Transfer</h1>
      <p>Token: {token}</p>
      <BatchTransferForm token={token} />
    </>
  )
}

export default BatchTransferPage
