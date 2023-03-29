TEST_OPTS ?= -vv
DEV_TARGET ?= test-all

dev:
	nodemon -e sol -x sh -- -c "make $(DEV_TARGET) || exit 1"

test-all:
	forge test $(TEST_OPTS)

.PHONY: dev
