# AGENTS.md — emeld

Single-file Emacs Lisp package (Meld-Inspired Local Diff).

## Commands

```sh
# Run all tests
emacs --batch -l emeld-tests.el

# Interactive usage
emacs -Q -L . -l emeld.el --eval '(emeld-diff "dir1" "dir2")'
```

## Structure

- `emeld.el` — the package, single entrypoint. `M-x emeld-diff` (pick two dirs) or `M-x emeld-diff-dwim` (guesses from Dired buffers).
- `emeld-tests.el` — `ert` tests. Loads `emeld.el` via `(load-file "emeld.el")` — run from repo root.
- No Makefile, no CI, no package manager config.

## Key conventions

- `lexical-binding: t` — be careful with closures in scan threads.
- Uses Emacs `make-thread` for async scanning — UI must update via `run-at-time` from main thread.
- `diff -q` for content comparison; set `emeld-simple-comparison` for size-only (faster).
- Glob filter: `"*.elc"` matches filenames; `"tmp/*"` matches relative paths (contains `/`).
- `emeld-node` struct stores the entire tree; properties attached via text-property `'emeld-node`.
- Buffer-local vars (`emeld-show-*`) must be `make-local-variable`'d in mode body.
- Node rendering uses standard Emacs `diff-*` faces for theming.

## Minimum Emacs version

Requires Emacs 27+ (uses `make-thread`, 4-arg `copy-directory`).
