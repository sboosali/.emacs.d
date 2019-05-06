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

- `./highlight-*`
- ``./rainbow-*`

## 