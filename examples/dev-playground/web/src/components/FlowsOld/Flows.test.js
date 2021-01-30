import { render } from '@redwoodjs/testing'

import Flows from './Flows'

describe('Flows', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<Flows />)
    }).not.toThrow()
  })
})
