import FlowsLayout from 'src/layouts/FlowsLayout'
import EditFlow from 'src/components/EditFlow'

const NewFlowPage = ({ to }) => {
  return (
    <FlowsLayout>
      <EditFlow to={to} />
    </FlowsLayout>
  )
}

export default NewFlowPage
