import { useMutation, useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import UserForm from 'src/components/UserForm'

import { QUERY } from 'src/components/UsersCell'

const CREATE_USER_MUTATION = gql`
  mutation CreateUserMutation($input: CreateUserInput!) {
    createUser(input: $input) {
      id
    }
  }
`

const NewUser = () => {
  const { addMessage } = useFlash()
  const [createUser, { loading, error }] = useMutation(CREATE_USER_MUTATION, {
    onCompleted: () => {
      navigate(routes.users())
      addMessage('User created.', { classes: 'rw-flash-success' })
    },
  })

  const onSave = (input) => {
    createUser({ variables: { input } })
  }

  return (
    <div className="rw-segment">
      <header className="rw-segment-header">
        <h2 className="rw-heading rw-heading-secondary">New User</h2>
      </header>
      <div className="rw-segment-main">
        <UserForm onSave={onSave} loading={loading} error={error} />
      </div>
    </div>
  )
}

export default NewUser
