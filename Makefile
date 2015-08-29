all: test

test:
	cask exec emacs -batch -Q -L . -l test/test-runner.el

.PHONY: test
