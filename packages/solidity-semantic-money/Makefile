TEST_OPTS ?= -vv
CERTORA_OPTS ?=
DEV_TARGET ?= test-all

dev:
	nodemon -e sol -x sh -- -c "make $(DEV_TARGET) || exit 1"

test-all:
	forge test $(TEST_OPTS)

test-SemanticMoney:
	forge test --match-contract SemanticMoney $(TEST_OPTS)

test-ToySuperToken:
	forge test --match-contract ToySuperTokenTest $(TEST_OPTS)

test-Aqueduct:
	forge test --match-contract AqueductTest $(TEST_OPTS)

verify-ToySuperToken:
	certoraRun \
		test/ref-impl/ToySuperToken_certora.sol:ToySuperTokenCertora \
		test/ref-impl/ToySuperToken_certora.sol:ToySuperTokenPoolCertora \
		--verify ToySuperTokenCertora:certora/ToySuperToken.spec \
		--packages @openzeppelin=../../node_modules/@openzeppelin @superfluid-finance/solidity-semantic-money/src=src \
		--solc solc-0.8.19 \
		$(CERTORA_OPTS)

.PHONY: build-* dev test-* verify-*
