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
alice.createPool({ poolId: 1 });
```

Then we give shares to our friends

```js
await alice.giveShares({ poolId: 1, shares: 100, recipient: bob });
await alice.giveShares({ poolId: 1, shares: 50, recipient: carol });
```

And finally we can distribute funds to everyone in the pool, based on the amount of shares they have.

```js
await alice.distributeToPool({ poolId: 1, amount: 100 });
```
