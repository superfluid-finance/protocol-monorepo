/**
 * Credit of everything here goes to Redux Toolkit (RTK).
 */

export interface SerializedError {
    name?: string;
    message?: string;
    code?: string;
}

const commonProperties: Array<keyof SerializedError> = [
    "name",
    "code",
    "message",
];

/**
 * Serializes an error into a plain object.
 * Reworked from https://github.com/sindresorhus/serialize-error
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
