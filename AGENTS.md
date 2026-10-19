# AGENTS.md — emeld

Multi-file Emacs Lisp package (Meld-Inspired Local Diff).

## Commands

```sh
# Run all tests (requires the repo root on load-path; the test file adds it)
emacs --batch -l emeld-tests.el

# Byte-compile all modules (must be warning-free)
emacs --batch -L . -f batch-byte-compile emeld-core.el emeld-git.el emeld-sidebar.el emeld.el

# Interactive usage (-L . so the parts resolve)
emacs -Q -L . -l emeld.el --eval '(emeld-diff "dir1" "dir2")'
emacs -Q -L . -l emeld.el --eval '(emeld-git-diff default-directory "HEAD~3" "HEAD")'
```

## Structure

- `emeld.el` — thin umbrella: `(require 'emeld-core/-git/-sidebar)`, `(provide 'emeld)`. `(require 'emeld)` loads the whole package.
- `emeld-core.el` — the shared core: `emeld-node` struct, buffer-local state (incl. `emeld--git-*` mode vars), faces, async scan, filtering/gitignore/content filters, render, navigation, `emeld-mode`, file ops, recent comparisons (persisted to `emeld-recent-file`), benchmarking, transient, and git *utilities* shared by the others (`emeld--git-rev-label`, `emeld--git-toplevel`, `emeld--git-work-tree-p`, `emeld--git-rev-args`). `(provide 'emeld-core)`.
- `emeld-git.el` — `(require 'emeld-core)`. Git-diff feature: `M-x emeld-git-diff` (two revisions, or a rev vs working tree / index), `M-x emeld-git-step-history` (`,` older / `.` newer), history/commit/blob helpers, `emeld--git-action`/`-soft-diff`. `(provide 'emeld-git)`.
- `emeld-sidebar.el` — `(require 'emeld-core)`. `M-x emeld-sidebar` (treemacs-style single-tree explorer; see below). `(provide 'emeld-sidebar)`.
- `emeld-tests.el` — `ert` tests. Adds the repo root to `load-path` and `(require 'emeld)` — run from repo root.
- No Makefile, no CI, no package manager config.

**Dependency rule:** core depends on nothing in the package; feature files require core. Core's dispatch into git (from `emeld-refresh`, file ops, render, keymap/transient) is covered by a `declare-function` block at the top of `emeld-core.el` — extend it when core must call a new git function.

## Key conventions

- `lexical-binding: t` — be careful with closures in scan threads.
- Uses `make-process` for async `diff -rq` scanning — UI stays responsive.
- Only `diff -rq` (no `fd`). Filter groups are passed as `--exclude` to `diff`.
- Content filters (`emeld-ignore-whitespace`/`-blank-lines`/`-line-endings`/`-matching-lines`/`-case`) are assembled by `emeld--content-filter-args` and appended to *every* diff invocation: classification, same-files, quick/inline diffs, and patch export. The optional GIT-MODE argument drops `-i` (`git diff` has no case-insensitive mode) and switches line endings to git's `--ignore-cr-at-eol` (`--strip-trailing-cr` for `diff`); git's `--name-status` fast path can still list a file whose only change is blank lines unless another content filter is active.
- Depth-limited scan (`emeld-diff` prefix; `emeld--scan-max-depth`): `emeld--start-bounded-diff` scans level by level and marks below-limit and one-sided directories `scanned: nil` (rendered with a dim `…`); `G` re-runs the bounded scan at the same depth. Expanding one (`emeld-cycle-subtree`/`emeld-toggle-expand` → `emeld--maybe-populate-lazy-dir`) enumerates it in place — diffing the subtree when both sides exist, listing the existing side when one-sided — marks it scanned, and `emeld--refresh-ancestor-statuses` recomputes its ancestors' statuses and flags so drill marks clear once nothing below them is unscanned. There is no separate subdir-rescan command.
- Change navigation: `emeld-next-change`/`-previous-change` (`M-n`/`M-p`) walk rendered rows via the `emeld-node`/`emeld-inline-diff` text properties, skipping directories, `same` files and inline-preview lines; `emeld--change-count` is reset and recounted by `emeld--render` and feeds the header count.
- Git mode (`emeld-git-diff`): buffer-local `emeld--git-mode` switches the scan to async `git diff --name-status -z` (parsed by `emeld--parse-git-name-status`, fed to the same tree builder). Guards in `emeld--make-file-node` / `emeld--entries-to-nodes(-progressive)` skip filesystem probing; nodes carry the relpath, and file ops use `git show` (`emeld--git-blob-buffer`) / `git diff` (`emeld--git-diff-show`). Copy/delete are disabled. Renames split into delete + add.
- History stepping (`emeld-git-step-history`): `emeld--git-history` (first-parent SHA chain) + `emeld--git-history-index` track position; `,`/`.` (`emeld-git-history-older`/`-newer`) step, each comparing `history[i]` against its parent via `emeld--git-history-apply`. Root commits compare against `emeld--git-empty-tree`. Stepping `.` past the newest commit (when it is HEAD) reaches the virtual index `-1`, the working-tree slot: newest commit vs the working tree (uncommitted changes).
- Glob filter: `"*.elc"` matches filenames; `"tmp/*"` matches relative paths (contains `/`).
- `emeld-node` struct stores the entire tree; properties attached via text-property `'emeld-node`.
- Inline diff peek: `TAB` on a file (`emeld-cycle-subtree` → `emeld-toggle-inline-diff`) caches propertized `diff-*`-faced lines in the node's `inline-diff` slot; `emeld--render-nodes` draws them under the row. `emeld--inline-diff-text` uses `git diff` (git mode) or `emeld--write-patch-hunk` (dir mode) — capture buffer-local repo/revs before `with-temp-buffer`. `emeld--inline-diff-lines` then word-refines the peek: `emeld--inline-diff-spots` finds the removed-run + added-run pairs, `smerge-refine-regions` (with `diff-refine-preproc`; hence the top-level `(require 'smerge-mode)`) computes the changed character ranges of each pair, and `emeld--inline-diff-apply-refine` adds `diff-refine-removed`/`diff-refine-added` over them with the line's base face underneath; skipped past `emeld-inline-diff-refine-max-spots` spots (each spot costs a small `diff` run). `a` (`emeld-copy-hunk`) applies the `@@` hunk at point to the other file (a prefix reverses the direction): peek lines carry an `emeld-inline-diff-index` property, `emeld--inline-diff-hunk-at`/`emeld--hunk-sides` rebuild the hunk's old and new texts, and `emeld--replace-in-file` rewrites the target file (match nearest the hunk header, coding system preserved), logging a `:type 'hunk` op for undo; `emeld--reclassify-hunk-node` then re-statuses the node by an actual content diff (not mere presence).
- Sidebar (`emeld-sidebar`): treemacs-style single-tree explorer, own mode/render (`emeld-sidebar-mode`, `emeld--sidebar-render`), reuses `emeld-node` + `emeld--root-node`. Lazy per-dir load (node `scanned` slot = "loaded?"); root via `emeld--sidebar-project-root` (git → project.el → dir). `emeld-sidebar-follow-mode` (global) hooks both `window-selection-change-functions` and `window-buffer-change-functions` to reveal the active file (the latter is needed to catch opening a file in the current window). The follow prefers the selected window's file but falls back to any *other* window whose buffer changed, tracked via the `emeld--sidebar-follow-seen` window→buffer map (`window-buffer-change-functions` reports only the frame) — that is what makes xref candidate previews keep the sidebar on the file being shown, since a preview (minibuffer-driven completing-read preview *or* the `*xref*' buffer preview) always switches a non-selected window (`switch-to-buffer` norecord; note `set-window-buffer` does NOT fire the hook, only `switch-to-buffer` does). For `*xref*` buffers there is also a buffer-local `post-command-hook` (`emeld--sidebar-follow-xref`, installed via `xref--xref-buffer-mode-hook` → `emeld--sidebar-xref-mode-setup`) that reveals the candidate at point (`emeld--sidebar-xref-candidate-file` resolves it via the item's location marker) instantly, without waiting for the window-change round-trip; enabling follow mode retro-fits existing xref buffers, disabling scrubs them, and re-loading the file wires it when the mode is already on (bottom-of-file `when` form). Candidates outside the sidebar root are skipped by the same `string-prefix-p` guard. The current file is marked with an explicit overlay (not `hl-line-mode`, which only tracks the selected window) and `set-window-point` scrolls the unselected sidebar. Don't entangle it with the diff renderer.
- Buffer-local vars (`emeld-show-*`) must be `make-local-variable`'d in mode body.
- Node rendering uses standard Emacs `diff-*` faces for theming.
- Use `car`/`cdr` instead of `pop` in `lexical-binding: t` threads (gv-let-place compatibility).
- `directory-files-recursively` expands "Only in" directories found by `diff -rq`.

## Minimum Emacs version

Requires Emacs 27+ (uses `make-process`, 4-arg `copy-directory`).
