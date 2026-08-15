## My Emacs configuration

This is my minimal Emacs configuration, just basic and python stuff. `use-package` is used
heavily, it should install all 3rd party stuff on first start, so you might get yourself a
cup of tea. You will need some system packages to support 3rd party stuff. Just grep the
source for `apt install` to see those. Tested with Emacs 29.

## Tests

Custom functions (the `sta:...` defuns etc.) are covered by [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/) tests
under `tests/`, run in batch mode so they don't touch your real init files or installed
packages.

Run the whole suite with [just](https://github.com/casey/just):

```sh
just test
```

Or directly with Emacs, without `just`:

```sh
emacs -Q -batch -l tests/run-tests.el -f ert-run-tests-batch-and-exit
```

`tests/run-tests.el` loads every `tests/*-tests.el` file automatically, so add new
`<name>-tests.el` files under `tests/` alongside new config files — no need to touch the
justfile or this command.
