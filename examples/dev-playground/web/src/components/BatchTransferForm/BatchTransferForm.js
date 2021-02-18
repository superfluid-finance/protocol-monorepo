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
          placeholder={`0xa5cd9237694936f7c22c3a2d59e0ead4baf0061d 1000000000000000000\n0x98d562c7a4781e3e6c0d16f67469b0a3b0cb25c7 1000000000000000000\n0x7a22e36fc847b379b358d97dae992cc5ea9c4eea 1000000000000000000`}
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
