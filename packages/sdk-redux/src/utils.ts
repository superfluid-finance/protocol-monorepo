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
export function insertIf<T>(condition: boolean | unknown, ...elements: T[]): T[] {
    return condition ? elements : [];
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

const wait = (intervalMs: number) => new Promise((resolve) => setTimeout(resolve, intervalMs));

// Inspired by: https://gist.github.com/briancavalier/842626?permalink_comment_id=3702076#gistcomment-3702076
export async function retry<T>(fn: () => Promise<T>, retriesLeft = 3, intervalMs = 200): Promise<T> {
    try {
        return await fn();
    } catch (error) {
        await wait(intervalMs);
        if (retriesLeft === 0) {
            throw error;
        }
        return await retry(fn, --retriesLeft, intervalMs);
    }
}
