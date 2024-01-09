# Changes Since ToB 2023 Audit

Use `git diff 4ece1a3f4aff8b5a9cbf37118d261023960c0f0f.. packages/ethereum-contracts/contracts` to see the changes in the contract code since the audit commit hash.

## High Level Summary of Changes

### GeneralDistributionAgreementV1
- The representation of totalBuffer is modified to ensure proper data fitting in a 256-bit field.
- `realtimeBalanceVectorAt` removed
- `PoolConnectionUpdated` event only emitted if the connection was changed

### SuperfluidPool
The method for obtaining timestamps and checking member connections is updated to use Superfluid framework methods instead of Ethereum's native functionalities.