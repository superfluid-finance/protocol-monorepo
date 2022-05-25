# Solidity API

## SuperfluidFrameworkDeployer

This is NOT for deploying public nets, but rather only for tesing envs

### Framework

```solidity
struct Framework {
  contract TestGovernance governance;
  contract Superfluid host;
  contract ConstantFlowAgreementV1 cfa;
  struct CFAv1Library.InitData cfaLib;
  contract InstantDistributionAgreementV1 ida;
  struct IDAv1Library.InitData idaLib;
  contract SuperTokenFactory superTokenFactory;
}
```

### governance

```solidity
contract TestGovernance governance
```

### host

```solidity
contract Superfluid host
```

### cfa

```solidity
contract ConstantFlowAgreementV1 cfa
```

### ida

```solidity
contract InstantDistributionAgreementV1 ida
```

### superTokenFactory

```solidity
contract SuperTokenFactory superTokenFactory
```

### constructor

```solidity
constructor() public
```

Deploys everything... probably

### getFramework

```solidity
function getFramework() external view returns (struct SuperfluidFrameworkDeployer.Framework sf)
```

Fetches the framework contracts

### deployWrapperSuperToken

```solidity
function deployWrapperSuperToken(string name, string symbol) external returns (contract ERC20PresetMinterPauser token, contract SuperToken superToken)
```

Deploy new wrapper super token

