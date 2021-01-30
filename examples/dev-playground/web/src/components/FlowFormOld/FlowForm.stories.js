import FlowForm from './FlowForm'

const inputs = [
  {
    type: 'textfield',
    description: 'recipient',
    placeholder: 'i.e. 0x261b45d85ccfeabb11f022eba346ee8d1cd488c0',
  },
  { type: 'textfield', description: 'amount', placeholder: 'i.e. 255' },
]

export const generated = () => {
  return (
    <div className="m-4">
      <FlowForm inputs={inputs} />
    </div>
  )
}

export default { title: 'Components/FlowForm' }
