import { useMutation, useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import FlowForm from 'src/components/FlowForm'

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
const UPDATE_FLOW_MUTATION = gql`
  mutation UpdateFlowMutation($id: String!, $input: UpdateFlowInput!) {
    updateFlow(id: $id, input: $input) {
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

export const Success = ({ flow }) => {
  const { addMessage } = useFlash()
  const [updateFlow, { loading, error }] = useMutation(UPDATE_FLOW_MUTATION, {
    onCompleted: () => {
      navigate(routes.flows())
      addMessage('Flow updated.', { classes: 'rw-flash-success' })
    },
  })

  const onSave = (input, id) => {
    updateFlow({ variables: { id, input } })
  }

  return (
    <div className="rw-segment">
      <header className="rw-segment-header">
        <h2 className="rw-heading rw-heading-secondary">Edit Flow {flow.id}</h2>
      </header>
      <div className="rw-segment-main">
        <FlowForm flow={flow} onSave={onSave} error={error} loading={loading} />
      </div>
    </div>
  )
}
