// Useful links: https://github.com/ajv-validator/ajv/issues/1470 & https://ajv.js.org/guide/formats.html#formats-and-standalone-validation-code

module.exports = {
    addressOrEmpty: /(^(0x)?[0-9a-fA-F]{40}$)?/,
    stringInteger: /\d+/,
};
