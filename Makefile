export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: test
test: cask
	cask emacs -Q --batch -L . -L tests -l tests/markdown-special-keys-test.el -f ert-run-tests-batch
