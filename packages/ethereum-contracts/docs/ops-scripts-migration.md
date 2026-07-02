# Legacy ops-scripts migration

> v1.16.0 removes Truffle compile and `ops-scripts/` (Truffle `exec`). Use `new-ops-scripts/` and Foundry forge scripts for operational tasks going forward.

We **do not** need 1:1 replacements for every legacy script. **Gap** rows below are accepted until an ops need arises.

Legend: **Covered** | **Partial** | **Gap (accepted)** | **N/A**

## CI workflows by trigger

Truffle-backed workflows are **all manually triggered** (`workflow_dispatch`). None run on `push` / `pull_request`. Automated CI does **not** depend on `truffle exec`.

### Automated (runs on PR / push / schedule)

| Workflow | Trigger | Truffle? |
|----------|---------|----------|
| `ci.feature.yml` | `pull_request`, `merge_group` | **No** — `yarn test` (Foundry + Hardhat) |
| `handler.publish-dev-release-packages.yml` | `push` to `dev`, `release` published | **No** |
| `cd.packages-stable.create-release-drafts.yml` | `push` to `dev` (path-filtered) | **No** |
| `daily-query-check.yml` | `schedule` | **No** |

`test/ops-scripts/deployment.test.sh` (truffle exec) is a **local/package script**, not part of automated CI.

### Manual only (`workflow_dispatch`)

| Workflow | Truffle scripts used |
|----------|---------------------|
| `handler.deploy-to-mainnet.yml` | `deploy-framework.js` |
| `handler.deploy-to-testnets.yml` | `deploy-test-environment.js`, `info-print-contract-addresses.js` |
| `handler.run-ethereum-contracts-script.yml` | arbitrary `ops-scripts/` path |
| `handler.verify-contracts.yml` | `info-print-contract-addresses.js` |
| `handler.list-super-token.yml` | resolver list script |

### Reusable, no in-repo caller found

| Workflow | Truffle scripts | Notes |
|----------|-----------------|-------|
| `call.deploy-dry-run.yml` | `deploy-test-environment.js`, `gov-upgrade-super-token-logic.js` | `workflow_call` only; no caller in this repo |

### Local `tasks/*.sh` (not CI)

`tasks/deploy-*-forwarder.sh` and `tasks/etherscan-verify-framework.sh` were migrated to `new-ops-scripts/` in v1.16.0.

**Implication:** removing Truffle does **not** block merging v1.16.0 CI if automated tests pass. Manual deploy workflows can be updated incrementally.

---

## Deployment

| Legacy script | Status | Replacement / notes |
|---------------|--------|---------------------|
| `deploy-framework.js` | **Partial** | `UpgradeFramework.s.sol` + `upgrade-framework.sh` (upgrade path). Fresh deploy / full parity: not required for v1.16.0. |
| `deploy-test-environment.js` | **Gap (accepted)** | Port if testnet workflow still used |
| `deploy-deterministically.js` | **Covered** | `new-ops-scripts/deploy-deterministic-forwarder.ts` |
| `deploy-super-token.js` | **Gap (accepted)** | |
| `deploy-unlisted-*.js` | **Gap (accepted)** | |
| `deploy-test-token.js` | **N/A** | Foundry `TestToken` / dev-scripts |
| `deploy-erc1820.js` | **N/A** | dev-scripts / `ERC1820RegistryCompiled` |
| `deploy-aux-contracts.js` | **Gap (accepted)** | |
| `deploy-mfa.ts` | **N/A** | |

## Governance

| Legacy script | Status | Replacement / notes |
|---------------|--------|---------------------|
| `gov-set-token-min-deposit.js` | **Covered** | `gov-action.sh` |
| `gov-set-3Ps-config.js` | **Covered** | `gov-action.sh` |
| `gov-set-reward-address.js` | **Covered** | `gov-action.sh` |
| `gov-set-trusted-forwarder.js` | **Covered** | `gov-action.sh` / `activate-forwarder.sh` |
| `gov-upgrade-governance.js` | **Covered** | `gov-action.sh replaceGovernance` |
| `gov-transfer-framework-ownership.js` | **Gap (accepted)** | |
| `gov-authorize-app-deployer.js` | **Partial** | `acl-grant-superapp-registration.sh` — verify semantics |
| `gov-upgrade-super-token-logic.js` | **Gap (accepted)** | Only needed if dry-run workflow revived |

## Resolver / tokens / info

| Legacy script | Status | Notes |
|---------------|--------|-------|
| `resolver-set-key-value.js` | **Covered** | `resolver-set-key.sh` |
| `resolver-register/list/unlist-*.js` | **Gap (accepted)** | Manual workflows |
| `info-print-contract-addresses.js` | **Gap (accepted)** | Port if verify workflow still used |
| `info-*` (other) | **Gap (accepted)** | |
| `validate-deployment.ts` | **Partial** | Hardhat — keep, not truffle exec |
| `tmp-trigger-token-transfer.js` | **N/A** | Delete |

## CI & tasks (truffle exec) — priority

| Target | Trigger | Priority |
|--------|---------|----------|
| `handler.deploy-to-mainnet.yml` | manual | P0 — `upgrade-framework.sh` |
| `tasks/deploy-*-forwarder.sh` | local | P1 — use `new-ops-scripts/` |
| `handler.deploy-to-testnets.yml` | manual | P2 — port when next testnet deploy |
| `handler.run-ethereum-contracts-script.yml` | manual | P2 — restrict to `new-ops-scripts/` or remove |
| `handler.verify-contracts.yml` | manual | P2 |
| `handler.list-super-token.yml` | manual | P3 — gap accepted |
| `call.deploy-dry-run.yml` | reusable, no caller | P3 — orphan |
| `tasks/etherscan-verify-framework.sh` | local | P2 |

## npm artifact migration

`build/truffle/*.json` is removed from the published package in v1.16.0. Use:

- `build/hardhat/**` — nested Hardhat artifacts (canonical)
- `build/bundled-abi.json` — ABI-only consumers

```diff
- require(".../build/truffle/Superfluid.json")
+ require(".../build/hardhat/contracts/superfluid/Superfluid.sol/Superfluid.json")
```

See also [v1.16.0 tooling modernization plan](plans/2026-07-v1.16.0-tooling-modernization.md).
