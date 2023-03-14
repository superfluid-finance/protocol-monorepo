verify-ToySuperToken:
	certoraRun \
		src/ref-impl/ToySuperToken.sol:ToySuperTokenPool \
		src/ref-impl/ToySuperToken.sol:ToySuperToken \
		--verify ToySuperToken:certora/ToySuperToken.spec \
		--packages @openzeppelin=../../node_modules/@openzeppelin @superfluid-finance/solidity-semantic-money/src=src \
		--solc solc-0.8.19

.PHONY: verify-*
