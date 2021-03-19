export const truncate = (text, length = 50) => {
  if (typeof text !== 'string') return ''
  return text.substring(0, length) + '...'
}

export const getErrorResponse = (error, functionName) => {
  const errorText = typeof error === 'string' ? error : error.message
  const res = {
    /* eslint-disable-nextline i18next/no-literal-string */
    message: `Error web3.${functionName}(): ${errorText}`,
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
