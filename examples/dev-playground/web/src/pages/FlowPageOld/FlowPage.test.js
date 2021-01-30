import { render } from '@redwoodjs/testing'

import FlowPage from './FlowPage'

describe('FlowPage', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<FlowPage />)
    }).not.toThrow()
  })
})
