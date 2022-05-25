# Solidity API

## SuperfluidLoader

_A on-chain utility contract for loading framework objects in one view function.

NOTE:
Q: Why don&#x27;t we just use https://www.npmjs.com/package/ethereum-multicall?
A: Well, no strong reason other than also allowing on-chain one view function loading._

### _resolver

```solidity
contract IResolver _resolver
```

### Framework

```solidity
struct Framework {
  contract ISuperfluid superfluid;
  contract ISuperTokenFactory superTokenFactory;
  contract ISuperAgreement agreementCFAv1;
  contract ISuperAgreement agreementIDAv1;
}
```

### constructor

```solidity
constructor(contract IResolver resolver) public
```

### loadFramework

```solidity
function loadFramework(string releaseVersion) external view returns (struct SuperfluidLoader.Framework result)
```

_Load framework objects_

| Name | Type | Description |
| ---- | ---- | ----------- |
| releaseVersion | string | Protocol release version of the deployment |

