## run all ERT tests in batch mode
test:
    emacs -Q -batch -l tests/run-tests.el -f ert-run-tests-batch-and-exit
