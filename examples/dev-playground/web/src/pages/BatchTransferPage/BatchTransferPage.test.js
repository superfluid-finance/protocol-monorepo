import { render } from '@redwoodjs/testing'

import BatchTransferPage from './BatchTransferPage'

describe('BatchTransferPage', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<BatchTransferPage />)
    }).not.toThrow()
  })
})
