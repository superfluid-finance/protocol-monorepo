const getErrorResponse = (error, className, functionName) => {
    const errorText = typeof error === "string" ? error : error.message;
    let helperText = ` ${className}`;
    if (functionName) helperText.concat(`.${functionName}() `);
    return `Error @superfluid-finance/ethereum-contracts${helperText}: ${errorText}`;
};

module.exports = { getErrorResponse };
