all :unittest

unittest:
	cask exec emacs -batch -Q -L . -l test/run-test.el -f ert-run-tests-batch-and-exit
