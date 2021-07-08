const getErrorResponse = (error, className, functionName) => {
    const errorText = typeof error === "string" ? error : error.message;
    let helperText = ` ${className}`;
    if (functionName) helperText = helperText.concat(`.${functionName}() `);
    return `Error: @superfluid-finance/js-sdk${helperText}: ${errorText}`;
};

const getMissingArgumentError = (argumentName, helpText) => {
    return `You did not provide a required argument for "${argumentName}" ${helpText}`;
};

const getBatchCallHelpText = (index) =>
    ` in item #${index} in your batch call array. Please see https://docs.superfluid.finaince/batchCall for more help`;

module.exports = {
    getErrorResponse,
    getMissingArgumentError,
    getBatchCallHelpText,
};
