IDA will be needed to account for balance changes

-   just use balanceOf whenever an event occurs

AccountToken - a link between account and token
Purpose is to track all account balances, but also whether the user has interacted with a token

If we query AccountToken for a user, then any items that are returned will include every token they've interacted with, regardless of their balance. Same is true if we query for the Token as well.

Balance should be updated any time there is transfer event. Or via a CFA stream opening/closing

We also need a simple way to get the netFlow for a user's total flows. This can then be combined with the realTimeBalance to get the current user balance
