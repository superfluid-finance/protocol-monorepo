import Flow from 'src/components/Flow'

export const QUERY = gql`
  query FIND_FLOW($from: String!, $to: String!, $token: String!) {
    flow: flow(
      ownerAddress: $from
      recipientAddress: $to
      tokenAddress: $token
    ) {
      flowRate
    }
  }
`

export const Loading = () => <div>Loading...</div>

export const Empty = () => <div>Flow not found</div>

export const Success = ({ flow }) => {
  return <Flow flow={flow} />
}
