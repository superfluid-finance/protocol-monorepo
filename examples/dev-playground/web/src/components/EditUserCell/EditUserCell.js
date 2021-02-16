import { useMutation, useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import UserForm from 'src/components/UserForm'

export const QUERY = gql`
  query FIND_USER_BY_ID($id: String!) {
    user: user(id: $id) {
      id
      address
      authDetailId
    }
  }
`
const UPDATE_USER_MUTATION = gql`
  mutation UpdateUserMutation($id: String!, $input: UpdateUserInput!) {
    updateUser(id: $id, input: $input) {
      id
      address
      authDetailId
    }
  }
`

export const Loading = () => <div>Loading...</div>

export const Success = ({ user }) => {
  const { addMessage } = useFlash()
  const [updateUser, { loading, error }] = useMutation(UPDATE_USER_MUTATION, {
    onCompleted: () => {
      navigate(routes.users())
      addMessage('User updated.', { classes: 'rw-flash-success' })
    },
  })

  const onSave = (input, id) => {
    updateUser({ variables: { id, input } })
  }

  return (
    <div className="rw-segment">
      <header className="rw-segment-header">
        <h2 className="rw-heading rw-heading-secondary">Edit User {user.id}</h2>
      </header>
      <div className="rw-segment-main">
        <UserForm user={user} onSave={onSave} error={error} loading={loading} />
      </div>
    </div>
  )
}
