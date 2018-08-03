;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-autosave)
;; ^ how frequently to autosave files and to back them up,
;; and to which location.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO mv
(require 'sboo-shell)
;; ^ `shell-mode` is a shell-interface (but not a full terminal-emulator),
;; where I can use emacs' macros, editing features, navigation features,
;; and auto-completion features.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-compilation)
;; ^ my `compilation-mode` configuration.
;; `compilation` is emacs's bultin-support for compiling programs (e.g. via `make`),
;; and formating/hyperlinking the error-messages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-helm)
;; ^ my `helm` configuration.
;; `helm` is a framework for autocompletion / selection-narrowing.
;;
;; it integrates with:
;; - commands (`M-x`),
;; - buffers (`C-x b`),
;; - files ('C-x f`),
;; - etc.

;;;TODO helm [1] ignores mouse clicks and [2] doesn't respect CUA-mode. [3] overrides my f9 binding.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-haskell)
;; ^ my `haskell-mode` configuration. 

(require 'sboo-dante)
;; ^ my `dante` configuration.
;; `dante` is a lightweight, configurable Haskell IDE.

;(require 'sboo-ghcid)
;; ^ my custom `ghcid`-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-projectile)
;; ^ my `projectile` configuration.
;; `projectile` does project-management.

(require 'sboo-magit)
;; ^ my `magit` configuration.
;; i.e. an elegant interface to `git` version control.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-config) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;