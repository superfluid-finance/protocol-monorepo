import Flow from 'src/components/Flow'

export const QUERY = gql`
  query FIND_FLOW_BY_ID($id: String!) {
    flow: flow(id: $id) {
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

export const Empty = () => <div>Flow not found</div>

export const Success = ({ flow }) => {
  return <Flow flow={flow} />
}
