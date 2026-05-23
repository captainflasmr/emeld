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
- Uses `make-process` for async `diff -rq` scanning — UI stays responsive.
- Only `diff -rq` (no `fd`). Filter groups are passed as `--exclude` to `diff`.
- Glob filter: `"*.elc"` matches filenames; `"tmp/*"` matches relative paths (contains `/`).
- `emeld-node` struct stores the entire tree; properties attached via text-property `'emeld-node`.
- Buffer-local vars (`emeld-show-*`) must be `make-local-variable`'d in mode body.
- Node rendering uses standard Emacs `diff-*` faces for theming.
- Use `car`/`cdr` instead of `pop` in `lexical-binding: t` threads (gv-let-place compatibility).
- `directory-files-recursively` expands "Only in" directories found by `diff -rq`.

## Minimum Emacs version

Requires Emacs 27+ (uses `make-process`, 4-arg `copy-directory`).
