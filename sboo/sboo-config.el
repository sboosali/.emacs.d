;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================================================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-use-package)
;; ^ my `use-package` configuration.
;; Most `sboo-*` features call `use-package`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================================================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-platform-specific)
;; ^ conditional configuration, platform-specific.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-window-system-specific)
;; ^ conditional configuration, terminal-versus-desktop.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================================================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-autosave)
;; ^ how frequently to autosave files and to back them up,
;; and to which location.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(require 'sboo-dabbrev)
;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-linum)

(when (>= emacs-major-version 26)
  (sboo-linum-config))
  ;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-wdired)
;; ^ my `wdired` and `dired` configuration.
;; 

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

(require 'sboo-winner-mode)
;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-bookmarks)
;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================================================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

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

(require 'sboo-helm-swoop)

(require 'sboo-yasnippets)
;; ^ Activate before any `prog-mode'-package configuration.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Project-Management

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Editing

(require 'sboo-wrap-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FlyCheck

(require 'sboo-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects

(require 'sboo-projectile)
;; ^ my `projectile` configuration.
;; `projectile` does project-management.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filesystem Navigation/Manipulation

(require 'sboo-neotree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'multi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VersionControl (Git)

;;TODO it breaks autosave ;; (require 'sboo-magit)
;; ^ my `magit` configuration.
;; i.e. an elegant interface to `git` version control.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `prog-mode's

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(require 'sboo-direnv)
;; ^ `dante` needs `direnv`,
;; since it doesn't (?) expect both `cabal.project` and `nix-shell`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(require 'sboo-haskell)
;; ^ my `haskell`-language configuration. 

(require 'sboo-dante)
;; ^ my `dante` configuration.
;; `dante` is a lightweight, configurable Haskell IDE.

;(require 'sboo-ghcid)
;; ^ my custom `ghcid`-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nix

(require 'sboo-nix)
;; ^ my `nix`-language configuration.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formats

(require 'sboo-markdown)
;; ^

(require 'sboo-edit-indirect)
;; ^

(require 'sboo-yaml)
;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================================================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-config)
