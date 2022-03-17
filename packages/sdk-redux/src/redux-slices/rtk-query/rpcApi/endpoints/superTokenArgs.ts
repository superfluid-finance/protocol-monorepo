import {BaseSuperTokenMutation} from '../../../argTypes';

/**
 * Downgrade `amount` SuperToken's.
 */
export interface SuperTokenDowngrade extends BaseSuperTokenMutation {
    /** The amount to be downgraded. */
    amountWei: string;
}

/**
 * Upgrade `amount` SuperToken's.
 * NOTE: Initiates request for allowance if necessary.
 */
export interface SuperTokenUpgrade extends BaseSuperTokenMutation {
    /** The amount to be upgraded. */
    amountWei: string;
}

/**
 * Transfer `receiver` `amount` tokens.
 */
export interface SuperTokenTransfer extends BaseSuperTokenMutation {
    /** The receiver of the transfer. */
    receiverAddress: string;
    /** The amount to be transferred. */
    amountWei: string;
}
