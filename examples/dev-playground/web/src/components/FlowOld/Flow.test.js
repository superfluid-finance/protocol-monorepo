import { render } from '@redwoodjs/testing'

import Flow from './Flow'

describe('Flow', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<Flow />)
    }).not.toThrow()
  })
})
