import { Link, routes } from '@redwoodjs/router'
import BatchStream from 'src/components/BatchStream'

const BatchStreamPage = ({ token }) => {
  return (
    <>
      <BatchStream token={token} />
    </>
  )
}

export default BatchStreamPage
