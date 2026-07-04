#!/usr/bin/env bash
#
# Compare on-chain forwarder runtime bytecode to the local Forge deployedBytecode artifact.
# Immutable words (host, SimpleACL, …) are zeroed in on-chain code before compare — same
# approach as ops-scripts/libs/common.js codeChanged().
#
# Usage:
#   ./sfops/check-forwarder-bytecode.sh
#   ./sfops/check-forwarder-bytecode.sh --network xdai-mainnet
#   ./sfops/check-forwarder-bytecode.sh --include-testnets
#   ./sfops/check-forwarder-bytecode.sh --allow-not-deployed
#
# Exit 0 — all checked networks match (or skippable rows only).
# Exit 1 — mismatch, error, or not_deployed (unless --allow-not-deployed).
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PKG_ROOT"

METADATA_JSON="${METADATA_JSON:-$PKG_ROOT/../metadata/networks.json}"
CONTRACT="${CONTRACT:-ClearMacroForwarderV1WithPermit2}"
RESOLVER_KEY="${RESOLVER_KEY:-$CONTRACT}"
INCLUDE_TESTNETS=0
ALLOW_NOT_DEPLOYED=0
NETWORK_FILTER=()

ARTIFACT="$PKG_ROOT/build/foundry/default/${CONTRACT}.sol/${CONTRACT}.json"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --contract) CONTRACT="$2"; RESOLVER_KEY="${RESOLVER_KEY:-$CONTRACT}"; ARTIFACT="$PKG_ROOT/build/foundry/default/${CONTRACT}.sol/${CONTRACT}.json"; shift 2 ;;
    --resolver-key) RESOLVER_KEY="$2"; shift 2 ;;
    --network) NETWORK_FILTER+=("$2"); shift 2 ;;
    --include-testnets) INCLUDE_TESTNETS=1; shift ;;
    --allow-not-deployed) ALLOW_NOT_DEPLOYED=1; shift ;;
    -h|--help)
      sed -n '2,14p' "$0"
      exit 0
      ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

# shellcheck source=/dev/null
[[ -f "$PKG_ROOT/.env" ]] && source "$PKG_ROOT/.env"
# shellcheck source=/dev/null
[[ -f "$PKG_ROOT/../.env" ]] && source "$PKG_ROOT/../.env"

# shellcheck source=/dev/null
source "$PKG_ROOT/new-ops-scripts/lib/network-config.sh"

for cmd in forge cast jq python3; do
  command -v "$cmd" >/dev/null || { echo "Missing: $cmd" >&2; exit 1; }
done

if [[ ! -f "$ARTIFACT" ]]; then
  echo "Building $CONTRACT…"
  forge build --contracts "contracts/utils/${CONTRACT}.sol"
fi

COMPILER_BYTECODE=$(jq -r '.bytecode.object // empty' "$ARTIFACT")
COMPILER_DEPLOYED=$(jq -r '.deployedBytecode.object // empty' "$ARTIFACT")
if [[ -z "$COMPILER_BYTECODE" || "$COMPILER_BYTECODE" == "null" ]]; then
  echo "No bytecode in $ARTIFACT — run forge build" >&2
  exit 1
fi
# Legacy codeChanged uses creation bytecode; deployedBytecode is used for slot alignment.
if [[ -z "$COMPILER_DEPLOYED" || "$COMPILER_DEPLOYED" == "null" ]]; then
  COMPILER_DEPLOYED="$COMPILER_BYTECODE"
fi

# Compare on-chain vs artifact (see python block below).
bytecode_matches() {
  python3 - "$COMPILER_BYTECODE" "$COMPILER_DEPLOYED" "$1" "${@:2}" <<'PY'
import sys

# ap(addr) from ops-scripts/libs/common.js — address as 32-byte word (hex, no 0x)
def ap(addr: str) -> str:
    a = addr.lower().removeprefix("0x")
    return ("000000000000000000000000" + a)[-64:]


def trim_runtime(hexstr: str) -> str:
    h = hexstr.lower().removeprefix("0x")
    i = h.find("6080604052")
    return h[i:] if i >= 0 else h


def strip_solc_metadata(h: str) -> str:
    """Drop trailing CBOR metadata (ipfs hash + solc version); layout varies by compiler."""
    idx = h.rfind("64736f6c63")  # 'solc'
    if idx < 0:
        return h
    for back in range(4, 96):
        start = idx - back
        if start >= 0 and h[start : start + 2] == "a2":
            return h[:start]
    return h[:idx]


def apply_replacements(code: str, replacements: list[str]) -> str:
    c = code.lower().removeprefix("0x")
    for r in replacements:
        word = r.lower().removeprefix("0x")
        if len(word) != 64:
            word = ap(r)
        c = c.replace(word, "0" * 64)
    return c


def normalize_immutable_slots(creation: str, deployed: str, onchain: str) -> str:
    """Map runtime words filled at deploy time back to the artifact template.

    Immutables (EIP-712 domain, etc.) often share a 32-byte slot with bytecode; Forge
    leaves the unlinked template in both creation and deployedBytecode. Where those
    agree but on-chain differs, use the artifact word (same idea as legacy codeChanged
    plus address nulling).
    """
    if not (len(creation) == len(deployed) == len(onchain)):
        return onchain
    out: list[str] = []
    for i in range(0, len(onchain), 64):
        cw, dw, ow = creation[i : i + 64], deployed[i : i + 64], onchain[i : i + 64]
        if len(cw) < 64 or len(dw) < 64 or len(ow) < 64:
            out.append(ow)
            break
        out.append(dw if cw == dw and ow != dw else ow)
    return "".join(out)


def bytecode_matches_compiler(
    creation_hex: str, deployed_hex: str, onchain_hex: str, replacements: list[str]
) -> tuple[bool, str]:
    creation = strip_solc_metadata(trim_runtime(creation_hex))
    deployed = strip_solc_metadata(trim_runtime(deployed_hex))
    code = strip_solc_metadata(trim_runtime(onchain_hex))

    if len(code) <= 3:
        return False, "no on-chain code"

    # Replacements only on on-chain code (legacy codeChanged).
    code_replaced = apply_replacements(code, replacements)
    code_replaced = normalize_immutable_slots(creation, deployed, code_replaced)

    if code_replaced == deployed:
        return True, "ok"

    pos = creation.find(code_replaced)
    if pos < 0:
        return False, "runtime bytecode differs from local artifact (after immutables normalized)"

    end_index = pos + len(code_replaced)
    tail = creation[end_index:]
    if end_index == len(creation) or tail.startswith("6080604052"):
        return True, "ok"

    if pos == 0 and creation.startswith(code_replaced):
        return True, "ok (creation bytecode tail only)"

    return False, f"creation bytecode has {len(tail) // 2} unexpected bytes after match"


creation_hex = sys.argv[1]
deployed_hex = sys.argv[2]
onchain_hex = sys.argv[3]
replacements = [a for a in sys.argv[4:] if a]

ok, msg = bytecode_matches_compiler(creation_hex, deployed_hex, onchain_hex, replacements)
print(msg)
sys.exit(0 if ok else 1)
PY
}

read_resolver_address() {
  local resolver=$1 rpc=$2 key=$3
  local addr
  addr=$(cast call "$resolver" "get(string)(address)" "$key" --rpc-url "$rpc" 2>/dev/null | tr -d '[:space:]') || return 1
  if [[ -z "$addr" || "$addr" == "0x0000000000000000000000000000000000000000" ]]; then
    return 1
  fi
  echo "$addr"
}

resolve_rpc_for_row() {
  local network=$1
  if [[ -n "${RPC_URL:-}" ]]; then echo "$RPC_URL"; return; fi
  get_rpc_url "$network"
}

MATCH=0
MISMATCH=0
NOT_DEPLOYED=0
SKIP=0
ERROR=0

echo "Forwarder bytecode check — $CONTRACT"
echo "Artifact: $ARTIFACT"
echo "Metadata: $METADATA_JSON"
echo "Method: deployedBytecode vs on-chain (immutables zeroed, legacy codeChanged-style)"
echo ""

mapfile -t NETWORK_ROWS < <(jq -r '.[] | select(.contractsV1.host != null) | [.name, .chainId, .contractsV1.host, (.contractsV1.resolver // "")] | @tsv' "$METADATA_JSON")

for row in "${NETWORK_ROWS[@]}"; do
  IFS=$'\t' read -r network _chain_id host resolver <<<"$row"

  if [[ "$INCLUDE_TESTNETS" -eq 0 ]]; then
    is_testnet=$(jq -r --arg n "$network" '.[] | select(.name == $n) | .isTestnet' "$METADATA_JSON")
    [[ "$is_testnet" == "true" ]] && continue
  fi

  if [[ ${#NETWORK_FILTER[@]} -gt 0 ]]; then
    found=0
    for f in "${NETWORK_FILTER[@]}"; do
      [[ "$f" == "$network" ]] && found=1 && break
    done
    [[ "$found" -eq 0 ]] && continue
  fi

  if ! rpc=$(resolve_rpc_for_row "$network" 2>/dev/null); then
    printf "%-20s  %-18s  (no RPC)\n" "$network" "skip_no_rpc"
    SKIP=$((SKIP + 1))
    continue
  fi

  check_addr=""
  if [[ -n "$resolver" && "$resolver" != "null" ]]; then
    check_addr=$(read_resolver_address "$resolver" "$rpc" "$RESOLVER_KEY" || true)
  fi

  if [[ -z "$check_addr" ]]; then
    printf "%-20s  %-18s  (resolver missing %s)\n" "$network" "skip_no_resolver" "$RESOLVER_KEY"
    SKIP=$((SKIP + 1))
    continue
  fi

  onchain=$(cast code "$check_addr" --rpc-url "$rpc" 2>/dev/null || true)
  if [[ -z "$onchain" || "$onchain" == "0x" ]]; then
    printf "%-20s  %-18s  %s\n" "$network" "not_deployed" "$check_addr"
    NOT_DEPLOYED=$((NOT_DEPLOYED + 1))
    continue
  fi

  simple_acl=""
  if ! simple_acl=$(cast call "$host" "getSimpleACL()(address)" --rpc-url "$rpc" 2>/dev/null | tr -d '[:space:]'); then
    printf "%-20s  %-18s  %s\n" "$network" "error" "$check_addr"
    printf "%-20s    getSimpleACL failed\n" ""
    ERROR=$((ERROR + 1))
    continue
  fi

  compare_msg=$(bytecode_matches "$onchain" "$host" "$simple_acl" 2>&1) && cmp_ok=1 || cmp_ok=0

  if [[ "$cmp_ok" -eq 1 ]]; then
    printf "%-20s  %-18s  %s  %s\n" "$network" "match" "$check_addr" "$compare_msg"
    MATCH=$((MATCH + 1))
  else
    printf "%-20s  %-18s  %s\n" "$network" "MISMATCH" "$check_addr"
    printf "%-20s    %s\n" "" "$compare_msg"
    MISMATCH=$((MISMATCH + 1))
  fi
done

echo ""
echo "Summary: match=$MATCH mismatch=$MISMATCH not_deployed=$NOT_DEPLOYED skip=$SKIP error=$ERROR"

if [[ "$MISMATCH" -gt 0 || "$ERROR" -gt 0 ]]; then
  exit 1
fi
if [[ "$NOT_DEPLOYED" -gt 0 && "$ALLOW_NOT_DEPLOYED" -eq 0 ]]; then
  exit 1
fi
exit 0
