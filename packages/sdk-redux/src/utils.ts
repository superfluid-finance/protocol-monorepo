// Why? Because `return obj as T` and `return <T>obj` are not safe type casts.
export const typeGuard = <T>(obj: T) => obj;

export function insertIf<T>(condition: boolean | unknown, ...elements: T[]): T[] {
    return !!condition ? elements : [];
}

export enum MsTimes {
    OneSecond = 1000,
    FiveSeconds = 5000,
    TwentySeconds = 20000,
    OneMinute = 60000,
    TenMinutes = 600000,
    ThreeMinutes = 180000,
    FiveMinutes = 300000,
}
