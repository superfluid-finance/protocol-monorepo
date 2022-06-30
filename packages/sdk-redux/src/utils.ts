/**
 * Helper function to make sure an object instance does not have any fields extra or missing and is in fact the correct type.
 * The problem stems from the fact that `return obj as T` and `return <T>obj` are not safe type casts.
 * @private
 */
export const typeGuard = <T>(obj: T) => obj;

/**
 * A helper function that returns the inputted elements when condition is true or an empty array otherwise.
 * The reasoning for such a helper is to conditionally add items to an array in a nice syntax.
 * @private
 */
export function insertIf<T>(condition: boolean | unknown, getElements: () => T[]): T[] {
    return condition ? getElements() : [];
}

/**
 *
 * @private
 */
export enum MillisecondTimes {
    OneSecond = 1000,
    FiveSeconds = 5000,
    TwentySeconds = 20000,
    OneMinute = 60000,
    TenMinutes = 600000,
    ThreeMinutes = 180000,
    FiveMinutes = 300000,
}

/**
 * @private
 */
export const mutationOverridesKey = 'overrides' as const;

/**
 * @private
 */
export const mutationSignerKey = 'signer' as const;
