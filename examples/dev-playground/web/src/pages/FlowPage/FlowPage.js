import FlowsLayout from 'src/layouts/FlowsLayout'
import FlowCell from 'src/components/FlowCell'

const FlowPage = ({ id }) => {
  return (
    <FlowsLayout>
      <FlowCell id={id} />
    </FlowsLayout>
  )
}

export default FlowPage
