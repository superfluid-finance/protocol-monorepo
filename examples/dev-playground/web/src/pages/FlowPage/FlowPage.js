import FlowsLayout from 'src/layouts/FlowsLayout'
import FlowCell from 'src/components/FlowCell'

const FlowPage = ({ from, to, token }) => {
  return (
    <FlowsLayout>
      <FlowCell from={from} to={to} token={token} />
    </FlowsLayout>
  )
}

export default FlowPage
