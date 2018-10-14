;;; `init.el' for the `sboo' profile. -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports (Bootstrapped) ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((EmacsDirectory     (or user-emacs-directory
                               "~/.emacs.d/"))

       (SbooDirectory      (file-name-as-directory (concat EmacsDirectory
                                                           "sboo/")))

       (SbooFile           (file-truename (concat SbooDirectory
                                                  "sboo-definitions.el"))))   ;;TODO EnvironmentVars

  (require 'sboo-definitions SbooFile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-load-path! (FilePath)

  "Register `FilePath' with `load-path'.

  `FilePath':
  
  * /must/ be an absolute filepath to a directory; (TODO)
  
  * /should/ use forward-slashes, e.g. `.../.../...'
    (they're automatically converted to the platform-specifc directory-separator character);
  
  * /may/ start with `~/' 
    (tildes are expanded to the user's home directory);

  * /may/ end with a forward-slash (e.g. `sboo/' or `sboo')
    (a trailing is added if absent).
  "

  (add-to-list 'load-path
     (file-name-as-directory (file-truename FilePath))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-startup-hook! (FunctionSymbol)

  "Register `FunctionSymbol' with `emacs-startup-hook'."

  (add-hook 'emacs-startup-hook FunctionSymbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file sboo-custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-load-path! sboo-root-directory)
(add-to-load-path! sboo-lisp-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-load-file! "sboo-settings.el")
(sboo-load-file! "sboo-aliases.el")
(sboo-load-file! "sboo-commands.el")
(sboo-load-file! "sboo-keybindings.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 26)

  (require 'sboo-autosave)

  (sboo-autosave-init!)

  (add-startup-hook! #'sboo-autosave-config!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (add-startup-hook! #'server-start-unless-running))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-widgets)
;;;  (sboo-minibuffer-config))
;;;  (sboo-config-fonts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Installation ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcase (sboo-install-p)

  ('submodules (progn
                 (sboo-register-submodule! "use-package/")
                 (sboo-register-submodule! "helm/")
                 
                 (when (< emacs-major-version 26)
                   (sboo-register-submodule! "real-auto-save/"))))

  ('melpa      (progn
                 (sboo-load-file! "sboo-packages-by-installing.el")))

  ('nixpkgs    (progn))

  (_           (progn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Core Configuration ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-load-file! "sboo-init-helm.el")
(sboo-load-file! "sboo-init-use-package.el")

(when (< emacs-major-version 26)
  (sboo-load-file! "sboo-init-real-auto-save.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Haskell Configuration ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-haskell nil t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package haskell
    :demand t

    :commands    (haskell-mode)

    :interpreter (("runhaskell"  . haskell-mode)
                  ("runghc"      . haskell-mode)
                  ("stack"       . haskell-mode))

    :mode        (("\\.hs\\'"    . haskell-mode)
                  ("\\.lhs\\'"   . haskell-mode)
                  ("\\.hsig\\'"  . haskell-mode)
                  ("\\.hsc\\'"   . haskell-mode))

    ;;; :hook        ((haskell-mode . interactive-haskell-mode))

    :init
    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook #'sboo-haskell-prettify-symbols)

    (setq haskell-doc-current-info #'sboo-haskell-doc-current-info)

    :config
    
    (progn
      (custom-set-variables

       '(haskell-process-type             (quote cabal-new-repl))
       
       '(haskell-process-path-ghci        "cabal")

       ;; '(haskell-process-type             (quote stack-ghci))
       ;; '(haskell-process-path-ghci        "stack")

       '(haskell-process-args-cabal-repl  (list "--ghc-option=-ferror-spans"))

       '(haskell-ask-also-kill-buffers    nil)

       '(haskell-interactive-popup-errors nil))

      ()))

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package haskell-decl-scan
    :after    haskell
    
    :commands (haskell-decl-scan-mode)
    
    ;;;:hook     ((haskell-mode . haskell-decl-scan-mode))

    :init
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode))

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package haskell-cabal
    :after    haskell

    :commands (haskell-cabal-mode)

    :mode        (("\\.cabal\\'"      . haskell-cabal-mode)
                  ("\\.project\\'"    . haskell-cabal-mode)
                  ("\\`\\.project\\'" . haskell-cabal-mode)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package dante
    :after    haskell

    :commands (dante-mode dante-restart)

    :bind (:map haskell-mode-map
                (("C-c d" . sboo-dante-mode)))

;;;  :hook ((haskell-mode . flycheck-mode)
;;;         (haskell-mode . dante-mode))

    :init
    (add-hook 'haskell-mode-hook #'flycheck-mode)
    (add-hook 'haskell-mode-hook #'dante-mode)

    :config
    (setq dante-repl-command-line-methods-alist
          sboo-dante-repl-command-line-methods-alist)

    (setq sboo-haskell-eldoc 'dante)

    ())

  ;; ^ Configure `dante':
  ;;
  ;; * load `dante.el', which registers `dante-target' and `dante-project-root' as `safe-local-var's.
  ;; * `autoload' the `dante-mode' command, so we can run 《 M-x dante-mode 》 manually.
  ;; 
  ;; 

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: `company-*' Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-company nil t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package company

    :init (global-company-mode)

    :config (progn

              (setq company-backends sboo-company-backends)

              (bind-key [remap completion-at-point] #'company-complete company-mode-map)
              ;; ^ Use Company for completion
              ;;
              ;; NOTE `:bind' syntax is (`kbd' ...) only, no [`remap' ...]

              (setq company-tooltip-align-annotations t)
              ;; ^

              (setq company-show-numbers t)
              ;; ^ 
              ;; FYI candidate selection via `M-{number}'
              ;; (i.e. `M-1', `M-2', ..., `M-9').
              
              (setq company-dabbrev-downcase nil)
              ;; ^

              (setq company-minimum-prefix-length 1)

              ;; ^ minimum prefix length for idle completion.
              ;;
              ;; Default is 3.
              ;;

              ())

    :diminish company-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)

  (define-key company-active-map (kbd "<backtab>") #'sboo-company-complete-common-or-previous-cycle)

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package company-cabal

    :init
    (add-to-list 'company-backends #'company-cabal)

    ())

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package company-ghci

    :init

    (push #'company-ghci company-backends)
    
    (add-hook 'haskell-mode-hook             #'company-mode)
    (add-hook 'haskell-interactive-mode-hook #'company-mode)
    ;; ^ for completions in the REPL

    ())

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (use-package company-web

  ;;   :init
  ;;   (dolist (HOOK '(js-mode-hook
  ;;                   js2-mode-hook
  ;;                   js3-mode-hook
  ;;                   inferior-js-mode-hook))

  ;;     (add-hook HOOK #'sboo-company-javascript))
  ;;   ())

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (use-package company-anaconda
  ;;   ;
  ;;   :init
  ;;   (add-hook 'python-mode-hook #'sboo-company-python)
  ;;   ;
  ;;   ())

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-haskell-compilation nil t)

  

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: (Other) Configuration ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-projectile nil t)

  (use-package projectile

    ;;;TODO :delight '(:eval (concat " " (projectile-project-name)))  
    ;; ^
    ;; [1] Hide the mode name for projectile-mode;
    ;; [2] Show the project name instead.

    :config
    (setq projectile-globally-ignored-directories    (append sboo-exclusions-directories       projectile-globally-ignored-directories))
    (setq projectile-globally-ignored-files          (append sboo-exclusions-file-names        projectile-globally-ignored-files))
    (setq projectile-globally-ignored-file-suffixes  (append sboo-exclusions-file-extensions   projectile-globally-ignored-file-suffixes))
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-yasnippets nil t)

  (use-package yasnippet

    :demand t

    :mode ("\\.yasnippet\\'" . snippet-mode)

    :bind (("<kp-home>" . yas-next-field-or-maybe-expand)
          )
    
    :init
    (setq yas-snippet-dirs (list sboo-snippets-directory))

    ;; ^ `setq' vs `add-to-list': remove the default.

    :config
    (yas-reload-all t)
    (yas-global-mode 1)
    ())

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `wgrep': "Writeable GREP".

(use-package wgrep

  ;; :bind (:map grep-mode-map
  ;;             ;; ^
  ;;             ;; NOTE **not** `wgrep-mode-map', which binds:
  ;;             ;;
  ;;             ;;     ("C-x C-q" . wgrep-exit)
  ;;             ;;              
  ;;             ("C-x C-q" . wgrep-toggle-readonly-area)
  ;;             ;; ^
  ;;             ;; the standard keybinding for `toggle-read-only'.
  ;;             ;; for consistency, e.g. with `wdired'.
  ;;             ;;
  ;;             ;; [TODO doesn't work]
  ;;             )

  :config

  (setq wgrep-auto-save-buffer t)
  ;; ^ save all edited buffers automatically, after `wgrep-finish-edit'.

  (setq wgrep-enable-key "w")
  ;; ^ keybinding to switch to wgrep.
  ;; Mnemonic: "w" for "write-mode" (from "read-only-mode").

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :bind (:map markdown-mode-map
              ("TAB" . dabbrev-expand)
         :map gfm-mode-map
              ("TAB" . dabbrev-expand))

  :init
  (setq markdown-command "multimarkdown")

  :commands (markdown-mode gfm-mode))

;; ^ 
;;
;; `gfm-mode' abbreviates "GitHub-flavored markdown".
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `wgrep' notes

;; KeyBindings:
;;
;; You can edit the text in the grep buffer after typing C-c C-p. After that the changed text is highlighted. The following keybindings are defined:
;;
;; C-c C-e: Apply the changes to file buffers.
;; C-c C-u: All changes are unmarked and ignored.
;; C-c C-d: Mark as delete to current line (including newline).
;; C-c C-r: Remove the changes in the region (these changes are not applied to the files. Of course, the remaining changes can still be applied to the files.)
;; C-c C-p: Toggle read-only area.
;; C-c C-k: Discard all changes and exit.
;; C-x C-q: Exit wgrep mode.
;;
;; i.e.:
;;
;; ("\C-c\C-c" . 'wgrep-finish-edit)
;; ("\C-c\C-d" . 'wgrep-mark-deletion)
;; ("\C-c\C-e" . 'wgrep-finish-edit)
;; ("\C-c\C-p" . 'wgrep-toggle-readonly-area)
;; ("\C-c\C-r" . 'wgrep-remove-change)
;; ("\C-x\C-s" . 'wgrep-finish-edit)
;; ("\C-c\C-u" . 'wgrep-remove-all-change)
;; ("\C-c\C-[" . 'wgrep-remove-all-change)
;; ("\C-c\C-k" . 'wgrep-abort-changes)
;; ("\C-x\C-q" . 'wgrep-exit)
;; 

;; See: https://github.com/mhayashi1120/Emacs-wgrep
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `markdown-mode' notes

;; markdown-specific editing features:
;; 
;; - Change heading level two ways.
;; (1) By cycling with:
;;     * ‘C-c C--’ or `M-<left>’
;;     * `C-c C-=’ or `M-<right>’
;; (2) By re-issuing a heading insertion command when the point is at a heading. e.g.:
;;     * ‘C-c C-t 4’
;; will replace the current heading with a level-four heading.
;; 
;; - Section navigation via `outline-minor-mode', using the same keybindings as in org-mode:
;;     * ‘C-c C-f’
;;     * ‘C-c C-b’
;;     * ‘C-c C-u’
;;     * ‘C-c C-p’
;;     * ‘C-c C-n’
;; 
;; - Reorder list items with:
;;     * `M-<up>’
;;     * `M-<down>’
;; 

;; See:
;;     - https://jblevins.org/projects/markdown-mode/
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init)