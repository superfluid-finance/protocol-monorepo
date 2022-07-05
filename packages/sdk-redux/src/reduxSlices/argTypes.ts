import {ISuperTokenModifyFlowParams} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';

import {mutationOverridesKey, mutationSignerKey} from '../utils';

/**
 * Information about the transaction to locate it.
 */
export interface TransactionInfo {
    /** The transaction's network chain ID. */
    chainId: number;
    /** The transaction hash. */
    hash: string;
    // TODO(KK): Consider adding signer address here to for use of redux matching utilities which can't use the async getAddress call on the signer.
}

export interface BaseQuery<_TReturns> {
    /** Queried network's chain ID. */
    chainId: number;
}

/**
 * A query that returns a list of objects in paginated manner.
 */
export interface BasePaginatedQuery<TReturns> extends BaseQuery<TReturns> {
    /** How many entries to skip. */
    skip: number;
    /** How many entries to return. */
    take: number;
}

/**
 * A mutation that changes the state of a super token.
 */
export interface BaseSuperTokenMutation {
    /**  The network's chain ID where transactions will be broadcast. */
    chainId: number;
    /** SuperToken the mutation will affect. */
    superTokenAddress: string;
    /**
     * Whether to wait for one confirmation to the transaction or not.
     *
     * If set to `true` then the mutation will be blocking, i.e. the react hook / redux thunk will return control flow after first confirmation for the transaction broadcast.
     * If set to `false` then the mutation will not be blocking, i.e. the react hook / redux thunk will not return.
     */
    waitForConfirmation: boolean | NothingBoolean;

    /**
     * Custom data to included in transaction tracking. Has to be serializable for redux!
     */
    transactionExtraData?: Record<string, unknown>;

    [mutationOverridesKey]?: ISuperTokenModifyFlowParams['overrides'];
    [mutationSignerKey]: Signer;
}

/**
 * Input for strings that is ignored.
 */
export type NothingString = '' | undefined;

/**
 * Input for numbers that is ignored.
 */
export type NothingNumber = undefined;

/**
 * Input for booleans that is ignored.
 */
export type NothingBoolean = undefined;
