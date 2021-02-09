import { Link, routes } from '@redwoodjs/router'

import Flows from 'src/components/Flows'

export const QUERY = gql`
  query FLOWS {
    flows {
      id
      createdAt
      updatedAt
      flowRate
      recipientAddress
      ownerAddress
      userId
    }
  }
`

export const Loading = () => <div>Loading...</div>

export const Empty = () => {
  return (
    <div className="rw-text-center">
      {'No flows yet. '}
      <Link to={routes.newFlow()} className="rw-link">
        {'Create one?'}
      </Link>
    </div>
  )
}

export const Success = ({ flows }) => {
  return <Flows flows={flows} />
}
