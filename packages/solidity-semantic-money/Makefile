TEST_OPTS ?= -vv
DEV_TARGET ?= test-all

dev:
	nodemon -e sol -x sh -- -c "make $(DEV_TARGET) || exit 1"

test-all:
	forge test $(TEST_OPTS)

test-ToySuperToken:
	forge test --match-contract ToySuperTokenTest $(TEST_OPTS)

test-Aqueduct:
	forge test --match-contract AqueductTest $(TEST_OPTS)

verify-ToySuperToken:
	certoraRun \
		src/ref-impl/ToySuperToken.sol:ToySuperTokenPool \
		src/ref-impl/ToySuperToken.sol:ToySuperToken \
		--verify ToySuperToken:certora/ToySuperToken.spec \
		--packages @openzeppelin=../../node_modules/@openzeppelin @superfluid-finance/solidity-semantic-money/src=src \
		--solc solc-0.8.19

.PHONY: build-* dev test-* verify-*
