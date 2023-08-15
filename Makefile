export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: test
test: cask
	SELECTOR=$(SELECTOR)
	cask emacs -Q --batch -L . -L tests \
		-l tests/markdown-special-keys-test.el \
		--eval '(ert-run-tests-batch-and-exit $(SELECTOR))'
