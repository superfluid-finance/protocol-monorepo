/**
 * Credit of everything here goes to Redux Toolkit.
 */

/**
 * @public
 */
export interface SerializedError {
    name?: string;
    message?: string;
    stack?: string;
    code?: string;
}

const commonProperties: Array<keyof SerializedError> = [
    "name",
    "message",
    "stack",
    "code",
];

/**
 * Serializes an error into a plain object.
 * Reworked from https://github.com/sindresorhus/serialize-error
 *
 * @public
 */
export const miniSerializeError = (value: any): SerializedError => {
    if (typeof value === "object" && value !== null) {
        const simpleError: SerializedError = {};
        for (const property of commonProperties) {
            if (typeof value[property] === "string") {
                simpleError[property] = value[property];
            }
        }

        return simpleError;
    }

    return { message: String(value) };
};
