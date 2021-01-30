import Flows from 'src/components/Flows'

export const QUERY = gql`
  query FlowsQuery {
    flows {
      id
    }
  }
`

export const Loading = () => <div>Loading...</div>

export const Empty = () => <div>Empty</div>

export const Failure = ({ error }) => <div>Error: {error.message}</div>

export const Success = ({ flows }) => {
  return <Flows flows={flows} />
}
