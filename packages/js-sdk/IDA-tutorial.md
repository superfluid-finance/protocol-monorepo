---
description: Learn how use instant token distributions
---

# Perform an Instant Distribution using the SDK

![](../.gitbook/assets/image%20%284%29.png)

## Introduction

An **Instant Distribution Agreement \(IDA\)** can be used to make one-time-payments to multiple recipients. Use-cases include revenue sharing or air-drops. An IDA consists of a **Publisher** with one or more **Subscribers**. To simplify things this process, the SDK wraps much of the IDA logic.

## Steps

The first step is to create a new **pool** with a unique `poolId`

```js
await alice.createPool({ poolId: 1 });
```

Then we give out shares

```js
await alice.giveShares({ poolId: 1, shares: 90, recipient: bob });
await alice.giveShares({ poolId: 1, shares: 10, recipient: carol });
```

And finally we can distribute funds to everyone in the pool, based on the amount of shares they have.

```js
await alice.distributeToPool({ poolId: 1, amount: 1000 });
```

In this example, bob will receive 90% of the tokens alice sent, while carol only receives 10%. Bob will have 900 and carol will have 100.

Thats it! One thing to pay attention - for a recipient's balance to reflect the distribution event, they should first call `approveSubscription` one time. If they fail to do this, no worries, they can still receive their tokens after calling the `claim` function at any time.
