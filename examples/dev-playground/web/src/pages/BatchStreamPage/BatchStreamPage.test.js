import { render } from '@redwoodjs/testing'

import BatchStreamPage from './BatchStreamPage'

describe('BatchStreamPage', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<BatchStreamPage />)
    }).not.toThrow()
  })
})
