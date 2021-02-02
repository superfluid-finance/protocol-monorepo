import FlowsLayout from 'src/layouts/FlowsLayout'
import NewFlow from 'src/components/NewFlow'

const NewFlowPage = ({ to }) => {
  return (
    <FlowsLayout>
      <NewFlow to={to} />
    </FlowsLayout>
  )
}

export default NewFlowPage
