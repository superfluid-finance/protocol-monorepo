import { useMutation, useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import FlowForm from 'src/components/FlowForm'

import { QUERY } from 'src/components/FlowsCell'

const CREATE_FLOW_MUTATION = gql`
  mutation CreateFlowMutation($input: CreateFlowInput!) {
    createFlow(input: $input) {
      id
    }
  }
`

const NewFlow = () => {
  const { addMessage } = useFlash()
  const [createFlow, { loading, error }] = useMutation(CREATE_FLOW_MUTATION, {
    onCompleted: () => {
      navigate(routes.flows())
      addMessage('Flow created.', { classes: 'rw-flash-success' })
    },
  })

  const onSave = (input) => {
    createFlow({ variables: { input } })
  }

  return (
    <div className="rw-segment">
      <header className="rw-segment-header">
        <h2 className="rw-heading rw-heading-secondary">New Flow</h2>
      </header>
      <div className="rw-segment-main">
        <FlowForm onSave={onSave} loading={loading} error={error} />
      </div>
    </div>
  )
}

export default NewFlow
