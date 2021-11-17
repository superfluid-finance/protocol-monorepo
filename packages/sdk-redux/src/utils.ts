// Why? Because `return obj as T` and `return <T>obj` are not safe type casts.
export const typeGuard = <T>(obj: T) => obj;

export function insertIf<T>(condition: boolean, ...elements: T[]) {
    // (A)
    return condition ? elements : [];
}
