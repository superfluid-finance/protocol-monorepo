import {
  Form,
  FormError,
  FieldError,
  Label,
  TextField,
  TextAreaField,
  Submit,
} from '@redwoodjs/forms'

const BatchTransferForm = (props) => {
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
          name="tokenAddress"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Token Address
        </Label>
        <TextField
          readOnly
          defaultValue={props.tokenAddress}
          name="tokenAddress"
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
          name="recipientData"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Recipient Data
        </Label>
        <TextAreaField
          name="recipientData"
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{
            required: true,
            pattern: {
              value: /0x([A-Fa-f0-9]{40})\s[0-9]+/,
            },
          }}
        />
        <FieldError name="recipientData" className="rw-field-error" />

        <div className="rw-button-group">
          <Submit disabled={props.loading} className="rw-button rw-button-blue">
            Submit
          </Submit>
        </div>
      </Form>
    </div>
  )
}

export default BatchTransferForm
