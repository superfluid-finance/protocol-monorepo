const { getErrorResponse } = require("./error")

const validateAddress = (address) => {
  if (!isAddress(address)) throw "Address is invalid"
};

const isAddress = (address) => {
  return /^(0x)?[0-9a-f]{40}$/i.test(address);
};

export const getCleanAddress = (address) => {
  validateAddress(address);
  return address.toLowerCase();
};
