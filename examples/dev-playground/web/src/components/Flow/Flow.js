import { useMutation, useFlash } from '@redwoodjs/web'
import { Link, routes, navigate } from '@redwoodjs/router'

import { QUERY } from 'src/components/FlowsCell'

const DELETE_FLOW_MUTATION = gql`
  mutation DeleteFlowMutation($id: String!) {
    deleteFlow(id: $id) {
      id
    }
  }
`

const jsonDisplay = (obj) => {
  return (
    <pre>
      <code>{JSON.stringify(obj, null, 2)}</code>
    </pre>
  )
}

const timeTag = (datetime) => {
  return (
    <time dateTime={datetime} title={datetime}>
      {new Date(datetime).toUTCString()}
    </time>
  )
}

const checkboxInputTag = (checked) => {
  return <input type="checkbox" checked={checked} disabled />
}

const Flow = ({ flow }) => {
  const { addMessage } = useFlash()
  const [deleteFlow] = useMutation(DELETE_FLOW_MUTATION, {
    onCompleted: () => {
      navigate(routes.flows())
      addMessage('Flow deleted.', { classes: 'rw-flash-success' })
    },
  })

  const onDeleteClick = (id) => {
    if (confirm('Are you sure you want to delete flow ' + id + '?')) {
      deleteFlow({ variables: { id } })
    }
  }

  return (
    <>
      <div className="rw-segment">
        <header className="rw-segment-header">
          <h2 className="rw-heading rw-heading-secondary">
            Flow {flow.id} Detail
          </h2>
        </header>
        <table className="rw-table">
          <tbody>
            <tr>
              <th>Owner address</th>
              <td>{flow.ownerAddress}</td>
            </tr>
            <tr>
              <th>Recipient address</th>
              <td>{flow.recipientAddress}</td>
            </tr>
            <tr>
              <th>Token address</th>
              <td>{flow.tokenAddress}</td>
            </tr>
            <tr>
              <th>Flow rate</th>
              <td>{flow.flowRate}</td>
            </tr>
          </tbody>
        </table>
      </div>
      <nav className="rw-button-group">
        <Link
          to={routes.editFlow({
            from: flow.ownerAddress,
            to: flow.recipientAddress,
            tokenAddress: flow.tokenAddress,
          })}
          className="rw-button rw-button-blue"
        >
          Edit
        </Link>
      </nav>
    </>
  )
}

export default Flow
