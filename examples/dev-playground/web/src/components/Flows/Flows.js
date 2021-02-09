import { useMutation, useFlash } from '@redwoodjs/web'
import { Link, routes } from '@redwoodjs/router'

import { QUERY } from 'src/components/FlowsCell'

const DELETE_FLOW_MUTATION = gql`
  mutation DeleteFlowMutation($id: String!) {
    deleteFlow(id: $id) {
      id
    }
  }
`

const MAX_STRING_LENGTH = 150

const truncate = (text) => {
  let output = text
  if (text && text.length > MAX_STRING_LENGTH) {
    output = output.substring(0, MAX_STRING_LENGTH) + '...'
  }
  return output
}

const jsonTruncate = (obj) => {
  return truncate(JSON.stringify(obj, null, 2))
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

const FlowsList = ({ flows }) => {
  const { addMessage } = useFlash()
  const [deleteFlow] = useMutation(DELETE_FLOW_MUTATION, {
    onCompleted: () => {
      addMessage('Flow deleted.', { classes: 'rw-flash-success' })
    },
    // This refetches the query on the list page. Read more about other ways to
    // update the cache over here:
    // https://www.apollographql.com/docs/react/data/mutations/#making-all-other-cache-updates
    refetchQueries: [{ query: QUERY }],
    awaitRefetchQueries: true,
  })

  const onDeleteClick = (id) => {
    if (confirm('Are you sure you want to delete flow ' + id + '?')) {
      deleteFlow({ variables: { id } })
    }
  }

  return (
    <div className="rw-segment rw-table-wrapper-responsive">
      <table className="rw-table">
        <thead>
          <tr>
            <th>Id</th>
            <th>Created at</th>
            <th>Updated at</th>
            <th>Flow rate</th>
            <th>Recipient address</th>
            <th>Owner address</th>
            <th>User id</th>
            <th>&nbsp;</th>
          </tr>
        </thead>
        <tbody>
          {flows.map((flow) => (
            <tr key={flow.id}>
              <td>{truncate(flow.id)}</td>
              <td>{timeTag(flow.createdAt)}</td>
              <td>{timeTag(flow.updatedAt)}</td>
              <td>{truncate(flow.flowRate)}</td>
              <td>{truncate(flow.recipientAddress)}</td>
              <td>{truncate(flow.ownerAddress)}</td>
              <td>{truncate(flow.userId)}</td>
              <td>
                <nav className="rw-table-actions">
                  <Link
                    to={routes.flow({ id: flow.id })}
                    title={'Show flow ' + flow.id + ' detail'}
                    className="rw-button rw-button-small"
                  >
                    Show
                  </Link>
                  <Link
                    to={routes.editFlow({ id: flow.id })}
                    title={'Edit flow ' + flow.id}
                    className="rw-button rw-button-small rw-button-blue"
                  >
                    Edit
                  </Link>
                  <a
                    href="#"
                    title={'Delete flow ' + flow.id}
                    className="rw-button rw-button-small rw-button-red"
                    onClick={() => onDeleteClick(flow.id)}
                  >
                    Delete
                  </a>
                </nav>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

export default FlowsList
