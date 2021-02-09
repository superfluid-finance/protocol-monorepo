import FlowsLayout from 'src/layouts/FlowsLayout'
import EditFlow from 'src/components/EditFlow'

const EditFlowPage = ({ from, to, token }) => {
  return (
    <FlowsLayout>
      <EditFlow from={from} to={to} token={token} />
    </FlowsLayout>
  )
}

export default EditFlowPage
