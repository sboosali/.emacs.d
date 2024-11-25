# `sboo` 

`sboosali`'s Personal Emacs Configuration.

## Jargon

* *feature*: an Emacs Library, which can be `require`d.
* *window*: an Emacs Window (not an OS Window, which are Emacs Frames).

## Installation

I install via:

``` bash
git clone https://github.com/sboosali/.emacs.d.git ~/.emacs.d/ --recurse-submodules && cd ~/.emacs.d/

git submodule init submodules/use-package submodules/ submodules/helm submodules/real-auto-save
git submodule init submodules/dash submodules/s submodules/f
git submodule init submodules/magit submodules/company-mode submodules/yasnippets submodules/yasnippet-snippets submodules/flycheck submodules/projectile submodules/simpleclip
git submodule init submodules/solarized
git submodule init submodules/haskell-mode submodules/nix-mode
git submodule update
```

## Usage

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

* `Geiser`                       — *Major Mode* for *Guile/Scheme* <https://gitlab.com/jaor/geiser>
* `auto-highlight-symbol.el`     — *v1.53*. automatically highlight the current symbol (c.f. *Eclipse*).
* `awesome-tab.el`
* `back-button.el`               — a global *Back Button*. has several external dependencies: `;; Package-Requires: ((nav-flash "1.0.0") (smartrep "0.0.3") (ucs-utils "0.7.2") (list-utils "0.4.2") (persistent-soft "0.8.8") (pcache "0.2.3"))`. <https://github.com/rolandwalker/back-button>
* `cfrs.el`                      — *cfrs* abbreviates *Child-Frame Read-String*. `cfrs-read` provides the Frame-based `cfrs-read` (c.f. the Minibuffer-based `read-string`). <https://github.com/Alexander-Miller/cfrs>
* `char-menu.el`                 — Insert Unicode Characters efficiently (via `avy`). Supports `wrap-region`-like . <https://github.com/mrkkrp/char-menu>
* `color-identifiers-mode.el`   — 
* `company-restclient.el`        — *Company Backend* for` restclient`. other requirements: `(require 'know-your-http-well)`.
* `company-shell.el`             — *company-shell* provides *Company Backends* for shell Executables (via `'company-shell`) & shell Environment Variables (via `'company-shell-env`). <https://github.com/Alexander-Miller/company-shell>
* `company-web.el`               — *Company Backend* for Web Buffers. <https://github.com/osv/company-web>
* `compilation-recenter-end.el`  — Recenter `compilation-mode` buffers once `M-x compile` finishes (to maximize the onscreen text). <http://user42.tuxfamily.org/compilation-recenter-end/index.html>
* `compile-command-default.el`   — Guesses a default `compile-command` for `M-x compile`. <http://user42.tuxfamily.org/compile-command-default/index.html>
* `css-eldoc`                    — *Eldoc* for *CSS*. <https://github.com/zenozeng/css-eldoc>
* `dimmer.el`                    — highlight which buffer is active (by dimming the others.) <https://github.com/gonewest818/dimmer.el>
* `dired-toggle.el`              — toggle a *Sidebar* with *Dired* in the `cwd`. <https://github.com/fasheng/dired-toggle>
* `drag-stuff.el`                — <https://github.com/rejeep/drag-stuff.el>
* `dumb-jump.el`                 — *v0.5.2*
* `edit-indirect.el` — 
* `eimp.el` — 
* `elisp-docstring-mode.el` — 
* `elisp-refs.el` — 
* `emacsql`                      — *EmacSQL* is a *DSL* to write *SQL* with *S-Exp*s <https://github.com/skeeto/emacsql>
* `expand-region`                — Provides many *Mode-Specific* `expand-region` subcommands (in the `*-mode-expansions.el` files.)  <https://github.com/magnars/expand-region.el>
* `fdlcap.el`                    — from *2007*. *Cycle through case/capitalization of words.*
* `find-file-in-project.el`     — *v5.7.4*.
* `fix-word.el`                  — <https://github.com/mrkkrp/fix-word>
* `flycheck-package.el`         — `;; Package-Requires: ((flycheck "0.22") (package-lint "0.2"))`
* `flycheck.el`                  — `;; Version: 32-cvs`. `Package-Requires: ((emacs "24.3") ... (dash "2.12.1") (let-alist "1.0.4"))`
* `flyspell-lazy.el`             — increase keyboard responsiveness when `flyspell` is enabled, which (with the default settings) makes typing sluggish. <https://github.com/rolandwalker/flyspell-lazy>
* `gmail-message-mode.el`        — *Major Mode* for editing *Gmail* messages in *Markdown* syntax. `Package-Requires: ((ham-mode "1.0"))`. <https://github.com/Malabarba/gmail-mode/>
* `goto-last-change.el`
* `goto-line-preview.el`
* `graphviz-dot-mode.el`
* `ham-mode.el`                  — **H**tml **A**s **M**arkdown. `Package-Requires: ((html-to-markdown "1.2") (markdown-mode "2.0"))`. <https://github.com/Malabarba/ham-mode>
* `helm-ag.el`                   — *Helm* *TUI* for `ag` (a.k.a. *The Silver Searcher*). `;; Package-Requires: ((helm "2.0"))`. <https://github.com/syohex/emacs-helm-ag>
* `helm-flyspell.el`             — Correct `flyspell` suggestions via `helm`.
* `helm-make.el`                 — *Helm* *TUI* for the `make` program; complete all *Targets* in a `Makefile`. <https://github.com/abo-abo/helm-make>
* `helm-navi.el`                 — `;; Package-Requires: ((helm "1.9.4") (navi-mode "2.0") ...)`
* `helm-pass.el`                 — *Helm* *TUI* for the `pass` program, a *Password Manager*. `;; Package-Requires: ((emacs "25") (helm "0") (password-store "0") (auth-source-pass "4.0.0"))`. <https://github.com/emacs-helm/helm-pass>
* `helm-swoop.el` — 
* `helm-system-packages-dpkg.el` — *Helm* *TUI* for the `dpkg` program. <https://github.com/emacs-helm/helm-system-packages>
* `helpful.el` — 
* `highlight-blocks.el` — 
* `highlight-cl.el` — 
* `highlight-defined.el` — 
* `highlight-escape-sequences.el` — 
* `highlight-numbers.el` — 
* `highlight-quoted.el` — 
* `highlight2clipboard.el`       — `;; Package-Requires: ((htmlize "1.47"))`
* `history.el`                   — <https://github.com/boyw165/history>
* `html-to-markdown.el`          — *Native Elisp* HTML-to-Markdown-converter. <https://github.com/Malabarba/html-to-markdown>
* `htmlize.el`                   — *v1.55*
* `jq-mode.el` — 
* `link-hint.el`                 — `Package-Requires: ((avy "0.4.0"))`
* `llvm-mode.el` — 
* `lua-mode.el` — 
* `magin.el`                     — *DSL* for Git Ignore Files (`.gitignore`). <https://francismurillo.github.io/2017-01-25-A-DSL-For-Git-And-Projectile-Ignore-File/>
* `makefile-executor.el`         — <https://github.com/thiderman/makefile-executor.el>
* `makefile-runner.el` — 
* `mediawiki.el` — 
* `navi-mode.el`                 — *v2.0*
* `neotree.el` — 
* `nsis-mode.el`                 — *Major Mode* for editing <abbrv title="Nullsoft Scriptable Install System"><i>NSIS</i></abbrv> (`.nsi`) files. *NSIS* is *DSL* for writing *Windows* Installers. <https://github.com/mattfidler/nsis-mode>
* `olivetti.el`                  — `;; Version: 1.7.1`
* `package-lint.el` — 
* `pandoc-mode`                  — *Minor Mode* for `compile`ing via *Pandoc*.
* `peep-dired.el` — 
* `projin.el`                    — *DSL* for Projectile Ignore Files (`.projectile`). <https://francismurillo.github.io/2017-01-25-A-DSL-For-Git-And-Projectile-Ignore-File/> — 
* `rainbow-blocks.el` — 
* `rainbow-delimiters.el` — 
* `rainbow-identifiers.el` — 
* `rainbow-mode.el` — 
* `refine.el`                    — `;; Package-Requires: ((s "1.11.0") (dash "2.12.0") (list-utils "0.4.4") (loop "1.2"))` — 
* `restclient`                   — `restclient.el` is an *HTTP Client* as an interactive *Emacs* *TUI*. plus, `restclient-helm.el`, a *Helm* *TUI*. “Run queries from a plain-text query sheet, displays results as pretty-printed XML, JSON, even images.” <https://github.com/pashky/restclient.el> — 
* `shell-pop.el` — 
* `simpleclip.el`
* `sql-postgres.el`              — Indents *PL/pgSQL*. <https://github.com/nicferrier/emacs-sql-postgres>. 
* `string-edit.el`
* `sublimity.el`                 — *Sublime*-like *Minimap* and *Smooth-Scrolling*. <https://github.com/zk-phi/sublimity>
* `syntax-subword.el`            — like `subword-mode` but finer.
* `unicode-fonts.el`
* `validate.el`                  — *v1.0.4*. has no external dependencies: `;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (seq "2.16"))`. <http://endlessparentheses.com/validate-el-schema-validation-for-emacs-lisp.html>
* `visual-fill-column.el`
* `wrap-region.el`
* `writeroom-mode.el`            — `;; Package-Requires: ((visual-fill-column "1.9"))` — `;; Version: 3.8`
* `xmodmap-mode.el`
* `yasnippet.el`

*Vendored Utilities* include:

* `s.el`                 — *v1.12.0*
* `dash.el`              — *v2.15.0*
* `f.el`                 — *v0.20.0*
* `ht.el`                — *v2.3*
* `popup.el`             — *v0.5.3*
* `ov.el`                — *v*
* `loop.el`              — *v1.3*
* `list-utils.el`        — *v0.4.4*
* `memoize.el`           — *v*
* `let-alist.el`         — *v*
* `transient`            — for `magit`. <https://magit.vc/manual/transient>
* `whole-line-or-region` — <https://github.com/purcell/whole-line-or-region>
* `html-to-markdown`     — <https://github.com/Malabarba/html-to-markdown#html-to-markdown-converter-written-in-emacs-lisp>

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

## Debugging

`M-x list-load-path-shadows`, e.g.:

```
/home/sboosali/.emacs.d/sboo/real-auto-save hides /home/sboosali/.emacs.d/elpa/real-auto-save-20200505.1537/real-auto-save
```

## Notes

>