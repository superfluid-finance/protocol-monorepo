/**
 * Declaration merge more keys to this type to extend @see TransactionTitle
 */
export interface TransactionTitleOverrides {}

/**
 * For easily recognizing what was the intent of the transaction.
 */
export type TransactionTitle =
    | 'Create Stream'
    | 'Update Stream'
    | 'Close Stream'
    | 'Create Index'
    | 'Distribute Index'
    | 'Update Index Subscription Units'
    | 'Approve Index Subscription'
    | 'Claim from Index Subscription'
    | 'Delete Index Subscription'
    | 'Revoke Index Subscription'
    | 'Upgrade to Super Token'
    | 'Downgrade from Super Token'
    | 'Transfer Super Token'
    | keyof TransactionTitleOverrides;
