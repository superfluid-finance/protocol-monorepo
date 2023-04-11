TEST_OPTS ?= -vv
DEV_TARGET ?= test-all
CERTORA_OPTS = \
	--packages @openzeppelin=../../node_modules/@openzeppelin @superfluid-finance/solidity-semantic-money/src=src \
	--optimistic_loop \
	--loop_iter 3 \
	--solc solc-0.8.19

# Other CERTORA_OPTS fragments:
	#--solc_args "['--via-ir', '--optimize']" \
	# --settings -t=2000 \

dev:
	nodemon -e sol -x sh -- -c "make $(DEV_TARGET) || exit 1"

build-all:
	forge build --sizes

test-all:
	FOUNDRY_PROFILE=test forge test $(TEST_OPTS)

test-SemanticMoney:
	FOUNDRY_PROFILE=test forge test --match-contract SemanticMoney $(TEST_OPTS)

test-ToySuperToken:
	FOUNDRY_PROFILE=test forge test --match-contract ToySuperTokenTest $(TEST_OPTS)

test-Aqueduct:
	FOUNDRY_PROFILE=test forge test --match-contract AqueductTest $(TEST_OPTS)

verify-ToySuperToken-invariances:
	certoraRun \
		test/ref-impl/CertoraHarness.sol:CertoraSuperfluidToken \
		test/ref-impl/CertoraHarness.sol:CertoraSuperfluidPool \
		--verify CertoraSuperfluidToken:certora/ToySuperToken.invariances.spec \
		$(CERTORA_OPTS) \
		$(CERTORA_EXTRA_OPTS)

verify-ToySuperToken-rules:
	certoraRun \
		test/ref-impl/CertoraHarness.sol:CertoraSuperfluidToken \
		--verify CertoraSuperfluidToken:certora/ToySuperToken.rules.spec \
		$(CERTORA_OPTS) \
		$(CERTORA_EXTRA_OPTS)

.PHONY: build-* dev test-* verify-*
