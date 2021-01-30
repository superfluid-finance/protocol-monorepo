import { render } from '@redwoodjs/testing'

import FlowForm from './FlowForm'

describe('FlowForm', () => {
  it('renders successfully', () => {
    expect(() => {
      render(<FlowForm />)
    }).not.toThrow()
  })
})
