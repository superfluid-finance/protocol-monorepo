import { render } from '@redwoodjs/testing'

import DefaultLayout from './DefaultLayout'

describe('DefaultLayout', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<DefaultLayout />)
    }).not.toThrow()
  })
})
