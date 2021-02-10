const { zipFunctions } = require('@netlify/zip-it-and-ship-it')

const zipNetlifyFunctions = async function () {
  const archives = await zipFunctions('dist/functions', '.')
  return archives
}
zipNetlifyFunctions()
