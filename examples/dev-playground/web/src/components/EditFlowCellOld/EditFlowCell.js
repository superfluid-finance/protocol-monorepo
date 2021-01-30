import { useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import FlowForm from 'src/components/FlowForm'

export const Loading = () => <div>Loading...</div>

export const Empty = () => <div>null</div>

export const Success = ({ flow }) => {
  const { addMessage } = useFlash()
  const [error, setError] = React.useState(null)
  const [loading, setLoading] = React.useState(false)

  const updateFlow = (input) => {
    console.log(input)
    // navigate(routes.users())
    setError('Something went wrong')
    // addMessage('Flow updated.', { classes: 'rw-flash-success' })
  }

  const onSave = (input, id) => {
    updateFlow({ variables: { recipient, flowRate } })
  }

  return (
    <div className="rw-segment">
      <header className="rw-segment-header">
        <h2 className="rw-heading rw-heading-secondary">Edit Flow</h2>
      </header>
      <div className="rw-segment-main">
        <FlowForm flow={flow} onSave={onSave} error={error} loading={loading} />
      </div>
    </div>
  )
}
