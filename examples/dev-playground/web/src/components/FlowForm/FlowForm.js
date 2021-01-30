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
    props.onSave(data, props?.flow?.id)
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
          name="flowRate"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Flow rate
        </Label>
        <TextField
          name="flowRate"
          defaultValue={props.flow?.flowRate}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{ required: true }}
        />
        <FieldError name="flowRate" className="rw-field-error" />

        <Label
          name="recipientAddress"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Recipient address
        </Label>
        <TextField
          name="recipientAddress"
          defaultValue={props.flow?.recipientAddress}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{ required: true }}
        />
        <FieldError name="recipientAddress" className="rw-field-error" />

        <Label
          name="ownerAddress"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Owner address
        </Label>
        <TextField
          name="ownerAddress"
          defaultValue={props.flow?.ownerAddress}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{ required: true }}
        />
        <FieldError name="ownerAddress" className="rw-field-error" />

        <Label
          name="userId"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          User id
        </Label>
        <TextField
          name="userId"
          defaultValue={props.flow?.userId}
          className="rw-input"
          errorClassName="rw-input rw-input-error"
          validation={{ required: true }}
        />
        <FieldError name="userId" className="rw-field-error" />

        <div className="rw-button-group">
          <Submit disabled={props.loading} className="rw-button rw-button-blue">
            Save
          </Submit>
        </div>
      </Form>
    </div>
  )
}

export default FlowForm
