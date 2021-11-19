// Why? Because `return obj as T` and `return <T>obj` are not safe type casts.
export const typeGuard = <T>(obj: T) => obj;

export function insertIf<T>(condition: boolean | unknown, ...elements: T[]) {
    return !!condition ? elements : [];
}
