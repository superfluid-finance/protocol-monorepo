import {
  Form,
  FormError,
  FieldError,
  Label,
  TextField,
  Submit,
} from '@redwoodjs/forms'

const FlowForm = (props) => {
  const onSubmit = (data) => {
    props.onSave(data)
  }

  return (
    <div className="rw-form-wrapper">
      <Form onSubmit={onSubmit} error={props.error}>
        <FormError
          error={props.error}
          wrapperClassName="rw-form-error-wrapper"
          titleClassName="rw-form-error-title"
          listClassName="rw-form-error-list"
        />
        <Label
          name="ownerAddress"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Owner address
        </Label>
        <TextField
          readOnly
          name="ownerAddress"
          defaultValue={props.flow?.ownerAddress}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{
            required: true,
            pattern: {
              value: /^0x([A-Fa-f0-9]{40})$/,
            },
          }}
        />
        <FieldError name="ownerAddress" className="rw-field-error" />
        <Label
          name="recipientAddress"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Recipient address
        </Label>
        <TextField
          readOnly={props.flow?.recipientAddress}
          name="recipientAddress"
          defaultValue={props.flow?.recipientAddress}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{
            required: true,
            pattern: {
              value: /^0x([A-Fa-f0-9]{40})$/,
            },
          }}
        />
        <FieldError name="recipientAddress" className="rw-field-error" />

        <Label
          name="tokenAddress"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Token address
        </Label>
        <TextField
          name="tokenAddress"
          readOnly={props.flow?.tokenAddress}
          defaultValue={props.flow?.tokenAddress}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{
            required: true,
            pattern: {
              value: /^0x([A-Fa-f0-9]{40})$/,
            },
          }}
        />
        <FieldError name="tokenAddress" className="rw-field-error" />

        <Label
          name="flowRate"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Flow rate (tokens per second)
        </Label>
        <TextField
          name="flowRate"
          placeholder="eg. 385802469135802"
          defaultValue={props.flow?.flowRate}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{
            required: true,
            pattern: {
              value: /^[0-9]+$/,
            },
          }}
        />
        <FieldError name="flowRate" className="rw-field-error" />

        <div className="rw-button-group">
          <Submit disabled={props.loading} className="rw-button rw-button-blue">
            Submit
          </Submit>
        </div>
      </Form>
    </div>
  )
}

export default FlowForm
