export function getErrorResponse(
    error: Error | string,
    className: string,
    functionName: string
): string;

export function getMissingArgumentError(
    argumentName: string,
    helpText: string
): string;

export function getBatchCallHelpText(index: number): string;