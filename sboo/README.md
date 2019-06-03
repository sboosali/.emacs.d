# `sboo` 

My Emacs configuration.



## Jargon

* `feature`: an Emacs module, which can be `require`d.


## Structure / Organization

### Subdirectories

* `initialization/**.el`: Boostrapping. a few files to bootstrap `init.el` (e.g. for the `load-path`).
* `configuration/**.el`:  Configuring. these `feature`s are `require`d by `init.el`. most package-configuration `feature`s (i.e. those named `sboo-<PACKAGE>`) define a `sboo-config-<PACKAGE>!`.
* `snippets/**.snippet`: `YASnippet`s. mostly for `emacs-lisp-mode` and `haskell-mode`. 

### `init.el` and `sboo-init.el`

`init.el` is like a `main` procedure, while each `sboo-<PACKAGE>.el` is like a Haskell `module`. i.e. `init.el` has statements with effects (whose sequencing matters), while a `sboo-<PACKAGE>.el` has only declarations (i.e. `(require 'sboo-<PACKAGE>)` shouldn't execute any statements). 

to configure a package named `<PACKAGE>`, `init.el` has a statement like:

```elisp
(when (require 'sboo-<PACKAGE> nil t)
  (sboo-config-<PACKAGE>!))
```

## Bootstrapping



## `use-package`

NOTE `use-package` is used (by `init.el`) like so:

* always configures external (/installed) packages (for convenience and legibility)
* /never/ configures internal (/builtin) packages (for robustness).

## Dependencies

### Vendored Dependencies

several *Single-File Libraries* are vendored under `./sboo/lisp`, with demos under `./sboo/lisp/img`. 

most have *Minimal Dependencies*, either:

* no external dependencies
* only *Extended Standard-Library* dependencies (e.g. `dumb-jump.el` has `Package-Requires: ((f "0.20.0") (s "1.11.0") (dash "2.9.0") (popup "0.5.3"))`, which are utility libraries that are themselves vendored).
* only *intra-package* dependencies (e.g. `know-your-http-well.el` re-exports its `http-*.el`, which themselves are Single-File Libraries under the same Package).

*Vendored Features* include:

* `awesome-tab.el`
* `color-identifiers-mode.el` — 
* `dumb-jump.el`              — *v0.5.2*
* `edit-indirect.el`
* `eimp.el`
* `elisp-docstring-mode.el`
* `elisp-refs.el`
* `flycheck-package.el`       — `;; Package-Requires: ((flycheck "0.22") (package-lint "0.2"))`
* `find-file-in-project.el` *v5.7.4*
* `goto-last-change.el`
* `goto-line-preview.el`
* `graphviz-dot-mode.el`
* `helm-swoop.el`
* `helpful.el`
* `highlight-blocks.el`
* `highlight-cl.el`
* `highlight-defined.el`
* `highlight-escape-sequences.el`
* `highlight-numbers.el`
* `highlight-quoted.el`
* `jq-mode.el`
* `link-hint.el`
* `llvm-mode.el`
* `lua-mode.el`
* `makefile-runner.el`
* `mediawiki.el`
* `neotree.el`
* `package-lint.el`
* `peep-dired.el`
* `rainbow-blocks.el`
* `rainbow-delimiters.el`
* `rainbow-identifiers.el`
* `rainbow-mode.el`
* `refine.el`                 — `;; Package-Requires: ((s "1.11.0") (dash "2.12.0") (list-utils "0.4.4") (loop "1.2"))`
* `shell-pop.el`
* `simpleclip.el`
* `string-edit.el`
* `unicode-fonts.el`
* `wrap-region.el`
* `xmodmap-mode.el`
* `yasnippet.el`
* `auto-highlight-symbol.el`  — *v1.53*. automatically highlight the current symbol (c.f. *Eclipse*).
* `highlight2clipboard.el`    — `;; Package-Requires: ((htmlize "1.47"))`
* `syntax-subword.el` — like `subword-mode` but finer.
* `helm-navi.el` `;; Package-Requires: ((helm "1.9.4") (navi-mode "2.0") ...)`
* `navi-mode.el` *v2.0*
* `visual-fill-column.el`
* `writeroom-mode.el` — `;; Package-Requires: ((visual-fill-column "1.9"))` — `;; Version: 3.8`
* `olivetti.el` — `;; Version: 1.7.1`
* `flycheck.el` — `;; Version: 32-cvs`. `Package-Requires: ((emacs "24.3") ... (dash "2.12.1") (let-alist "1.0.4"))`
* `htmlize.el` — *v1.55*

* `pandoc-mode`:

    - `pandoc-mode.el`
    - `pandoc-mode-utils.el`

*Vendored Utilities* include:

* `s.el`          — *v1.12.0*
* `dash.el`       — *v2.15.0*
* `f.el`          — *v0.20.0*
* `ht.el`         — *v2.3*
* `popup.el`      — *v0.5.3*
* `ov.el`         — *v*
* `loop.el`       — *v1.3*
* `list-utils.el` — *v0.4.4*
* `memoize.el`    — *v*
* `let-alist.el`  — *v*

Miscellaneous *Vendored Libraries* (which are useful, but used infrequently) include:

* `know-your-http-well`:

    - `know-your-http-well.el`
    - `http-headers.el`
    - `http-methods.el`
    - `http-relations.el`
    - `http-status-codes.el	`
    - `media-types.el	`

* `operate-on-number.el`
* `desktop-environment.el`
* `smartscan.el`

##
