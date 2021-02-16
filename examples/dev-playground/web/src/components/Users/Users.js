import { useMutation, useFlash } from '@redwoodjs/web'
import { Link, routes } from '@redwoodjs/router'

import { QUERY } from 'src/components/UsersCell'

const DELETE_USER_MUTATION = gql`
  mutation DeleteUserMutation($id: String!) {
    deleteUser(id: $id) {
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

const UsersList = ({ users }) => {
  const { addMessage } = useFlash()
  const [deleteUser] = useMutation(DELETE_USER_MUTATION, {
    onCompleted: () => {
      addMessage('User deleted.', { classes: 'rw-flash-success' })
    },
    // This refetches the query on the list page. Read more about other ways to
    // update the cache over here:
    // https://www.apollographql.com/docs/react/data/mutations/#making-all-other-cache-updates
    refetchQueries: [{ query: QUERY }],
    awaitRefetchQueries: true,
  })

  const onDeleteClick = (id) => {
    if (confirm('Are you sure you want to delete user ' + id + '?')) {
      deleteUser({ variables: { id } })
    }
  }

  return (
    <div className="rw-segment rw-table-wrapper-responsive">
      <table className="rw-table">
        <thead>
          <tr>
            <th>Address</th>
            <th>&nbsp;</th>
          </tr>
        </thead>
        <tbody>
          {users.map((user) => (
            <tr key={user.id}>
              <td>
                <Link
                  to={routes.user({ id: user.id })}
                  title={'Flow to ' + user.address}
                  className="rw-button rw-button-small"
                >
                  {truncate(user.address)}
                </Link>
              </td>
              <td>
                <nav className="rw-table-actions">
                  <Link
                    to={routes.user({ id: user.id })}
                    title={'Show user ' + user.id + ' detail'}
                    className="rw-button rw-button-small"
                  >
                    Show
                  </Link>
                </nav>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

export default UsersList
