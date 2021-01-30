import UsersLayout from 'src/layouts/UsersLayout'
import EditUserCell from 'src/components/EditUserCell'

const EditUserPage = ({ id }) => {
  return (
    <UsersLayout>
      <EditUserCell id={id} />
    </UsersLayout>
  )
}

export default EditUserPage
