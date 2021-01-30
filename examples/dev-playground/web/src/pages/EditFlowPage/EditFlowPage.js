import FlowsLayout from 'src/layouts/FlowsLayout'
import EditFlowCell from 'src/components/EditFlowCell'

const EditFlowPage = ({ id }) => {
  return (
    <FlowsLayout>
      <EditFlowCell id={id} />
    </FlowsLayout>
  )
}

export default EditFlowPage
