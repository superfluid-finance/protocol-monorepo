/**
 * Declaration merge more keys to this type to extend @see TransactionKey
 */
export interface TransactionKeyOverrides {}

/**
 * For easily recognizing what was the intent of the transaction.
 */
export type TransactionKey =
    | 'FLOW_CREATE'
    | 'FLOW_UPDATE'
    | 'FLOW_DELETE'
    | 'INDEX_CREATE'
    | 'INDEX_DISTRIBUTE'
    | 'INDEX_UPDATE_SUBSCRIPTION_UNITS'
    | 'INDEX_SUBSCRIPTION_APPROVE'
    | 'INDEX_SUBSCRIPTION_CLAIM'
    | 'INDEX_DELETE_SUBSCRIPTION'
    | 'INDEX_SUBSCRIPTION_REVOKE'
    | 'SUPER_TOKEN_UPGRADE'
    | 'SUPER_TOKEN_DOWNGRADE'
    | 'SUPER_TOKEN_TRANSFER'
    | keyof TransactionKeyOverrides;
