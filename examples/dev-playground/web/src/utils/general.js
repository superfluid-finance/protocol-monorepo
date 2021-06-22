export const truncate = (text, length = 50) => {
  if (typeof text !== 'string') return ''
  return text.substring(0, length) + '...'
}

export const getErrorResponse = (error, functionName) => {
  let errorText = typeof error === 'string' ? error : error.message
  if (errorText.includes('execution reverted')) {
    const after = errorText.split('execution reverted:')[1]
    errorText = after.split(`"`)[0]
  }
  console.log(error.message)
  const res = {
    /* eslint-disable-nextline i18next/no-literal-string */
    message: `Error ${functionName}(): ${errorText}`,
  }
  const ABORTED = 'aborted'
  const EXCEPTION = 'exception'
  const UNKOWN = 'unknown error type'
  if (error.code) {
    res.code = error.code
    switch (error.code) {
      case 4001:
        res.txErrorType = ABORTED
        break
      case -32016:
        res.txErrorType = EXCEPTION
        break
      default:
        res.txErrorType = UNKOWN
    }
  }
  return { error: res }
}
