const getErrorResponse = (error, className, functionName) => {
    const errorText = typeof error === "string" ? error : error.message;
    let helperText = ` ${className}`;
    if (functionName) helperText = helperText.concat(`.${functionName}() `);
    return `Error @superfluid-finance/js-sdk${helperText}: ${errorText}`;
};

module.exports = { getErrorResponse };
