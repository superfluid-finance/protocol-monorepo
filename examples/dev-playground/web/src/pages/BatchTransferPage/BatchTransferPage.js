import { Link, routes } from '@redwoodjs/router'
import BatchTransfer from 'src/components/BatchTransfer'

const BatchTransferPage = ({ token }) => {
  return (
    <>
      <BatchTransfer token={token} />
    </>
  )
}

export default BatchTransferPage
