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
    props.onSave(data, props?.user?.id)
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
          name="address"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Recipient Address
        </Label>
        <TextField
          name="address"
          className="rw-input"
          placeholder="0x28D96a4125766467E65809E4E4b14145a1858de2"
          errorClassName="rw-input rw-input-error"
          validation={{
            required: true,
            // pattern: {
            //   value: /^0x([A-Fa-f0-9]{41})$/,
            // },
          }}
        />
        <FieldError name="address" className="rw-field-error" />

        <Label
          name="authDetailId"
          className="rw-label"
          errorClassName="rw-label rw-label-error"
        >
          Flowrate (tokens per second)
        </Label>
        <TextField
          name="flowRate"
          className="rw-input"
          placeholder="11111111"
          errorClassName="rw-input rw-input-error"
          validation={{ required: true }}
        />
        <FieldError name="authDetailId" className="rw-field-error" />

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
