import { Link, routes } from '@redwoodjs/router'
import EditFlowCell from 'src/components/EditFlowCell'
import FlowLayout from 'src/layouts/FlowLayout'

const FlowPage = ({ address }) => {
  return (
    <>
      <h1>Edit Flow</h1>
      <FlowLayout>
        <EditFlowCell recipient={address} />
      </FlowLayout>
    </>
  )
}

export default FlowPage
