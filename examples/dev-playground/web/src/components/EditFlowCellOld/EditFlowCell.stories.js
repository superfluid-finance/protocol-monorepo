import { Loading, Empty, Failure, Success } from './EditFlowCell'
import { standard } from './EditFlowCell.mock'

export const loading = () => {
  return Loading ? <Loading /> : null
}

export const empty = () => {
  return Empty ? <Empty /> : null
}

export const success = () => {
  return Success ? <Success {...standard()} /> : null
}

export default { title: 'Cells/EditFlowCell' }
