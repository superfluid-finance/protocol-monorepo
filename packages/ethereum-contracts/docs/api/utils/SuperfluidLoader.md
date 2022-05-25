# SuperfluidLoader

A on-chain utility contract for loading framework objects in one view function.

NOTE:
Q: Why don't we just use https://www.npmjs.com/package/ethereum-multicall?
A: Well, no strong reason other than also allowing on-chain one view function loading.

## Functions

### constructor

```solidity
function constructor(
    contract IResolver resolver
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `resolver` | contract IResolver |  |

### loadFramework

```solidity
function loadFramework(
    string releaseVersion
) external returns (struct SuperfluidLoader.Framework result)
```

Load framework objects

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `releaseVersion` | string | Protocol release version of the deployment |

