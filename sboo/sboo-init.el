;;; `init.el' for the `sboo' profile. -*- lexical-binding: t -*-

;;; Commentary:

;; Most `use-package' statements are in this file.

;;; Code:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 26)

  (require 'sboo-autosave)

  (sboo-autosave-init!)

  (add-startup-hook! #'sboo-autosave-config!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-auto-mode nil t)

  (sboo-add-auto-mode-basename       "LICENSE" #'text-mode)
  (sboo-add-auto-mode-basename       "TODO"    #'text-mode)
  (sboo-add-auto-mode-basename       "NOTES"   #'text-mode)

  (sboo-add-auto-mode-file-extension "knsrc"   #'conf-mode)
  ;; ^ `.knsrc' files have the `INI' format, which `conf-mode' supports.

  ;;TODO any file that ends in `rc`, should we default to 'conf-mode or to 'sh-mode?
  ;;;(add-to-list 'auto-mode-alist ("rc\\'" . #'conf-mode))
  ;;;(add-to-list 'auto-mode-alist ("rc\\'" . #'sh-mode))

  (sboo-add-auto-mode-file-extension "xml"     #'sgml-mode)
  ;; ^ `nxml-mode' vs `sgml-mode'.

  ())

;; ^ `auto-mode-alist' maps filepaths to major-modes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (>= emacs-major-version 24)
           (require 'sboo-theme nil :no-error))

  (add-to-list 'custom-theme-load-path sboo-theme-directory)

  (sboo-theme-set!)

  ())

;; ^ `load-theme':
;;
;; (defun load-theme (THEME &optional NO-CONFIRM NO-ENABLE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-desktop nil t)
  (sboo-desktop-init!)
  (add-startup-hook! #'sboo-desktop-config!))

;; ^ `desktop-mode'.

;; ^ NOTE We launch via « emacs --no-desktop »,
;; then configure and enable `desktop-mode' ourselves.
;;
;; This delays loading files until all modes have been properly configured.
;;
;; Otherwise, for example, `.nix` files aren't properly registered with `nix-mode`
;; when they are opened, even when `sboo-desktop` follows `sboo-nix`;
;; and thus need `revert-buffer`. 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package compile
;;   :bind (("s-m" . compile))
;;   :config 
;;          
;;                 compilation-read-command nil
;;                 compile-command "make")
;;  ())

(when (require 'sboo-compilation nil t)
  (sboo-compilation-init!)
  ())

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

(when (require 'sboo-make nil t)
  (add-hook 'makefile-mode-hook #'sboo-show-trailing-whitespace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (require 'sboo-prog nil t)
;;   (dolist (HOOK sboo-prog-mode-hooks)
;;     (add-hook 'prog-mode-hook HOOK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-shell nil t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defadvice term-char-mode (after term-char-mode-fixes ())

    (set (make-local-variable 'cua-mode) nil)
    ;; ^ Disable `cua-mode' to enable `?\C-x' for escaping.
    (set (make-local-variable 'transient-mark-mode) nil)
    (set (make-local-variable 'global-hl-line-mode) nil)

    (ad-activate 'term-char-mode)

    (term-set-escape-char ?\C-x))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-hook 'term-mode-hook #'sboo-local-unset-tab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'dired nil t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (bind-keys :map dired-mode-map
             ("w" . wdired-change-to-wdired-mode) ;; Mnemonic: [w]dired.
             )

  ;; ^ Shadows `dired-copy-filename-as-kill':
  ;;
  ;; (define-key map "w" 'dired-copy-filename-as-kill)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
                  ("stack"       . haskell-mode)
                  )

    :mode        (("\\.hs\\'"    . haskell-mode)
                  ("\\.lhs\\'"   . haskell-mode)
                  ("\\.hsig\\'"  . haskell-mode)
                  ("\\.hsc\\'"   . haskell-mode)
                  ("\\.chs\\'"   . haskell-mode)
                  )

    ;;; :hook        ((haskell-mode . interactive-haskell-mode))

    :init
    (setq haskell-doc-current-info #'sboo-haskell-doc-current-info)

    (add-hook 'haskell-mode-hook #'sboo-haskell-prettify-symbols)

    ;; (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    ;;
    ;; ^ `haskell-process' repeatedly spams errors for working projects,
    ;; stealing focus from the current buffer.

    (remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

    ;; ^ See « https://wiki.haskell.org/Emacs/Inferior_Haskell_processes ».

    :config
    
    (progn
      (custom-set-variables

       '(haskell-process-type             (quote cabal-new-repl))
       
       ;;'(haskell-process-path-ghci        "cabal")

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
;;; External Packages: ProgrammingLanguages ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-nix nil t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package nix-mode

    :interpreter (("nix"       . nix-mode)
                  ("nix-build" . nix-mode)
                  ("nix-env"   . nix-mode)
                  )

    :mode        (("\\.nix\\'" . nix-mode)
                  )
    ;; :init
    ;; (add-hook 'nix-mode-hook #')

    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package nix-repl)

  ;;;;;;;;;;;;;;;;;;;;;;;;;

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

  (bind-keys :map company-active-map

             ("<kp-1>"      . sboo-company-complete-1)
             ("<kp-end>"    . sboo-company-complete-1)

             ("<kp-2>"      . sboo-company-complete-2)
             ("<kp-down>"   . sboo-company-complete-2)

             ("<kp-3>"      . sboo-company-complete-3)
             ("<kp-next>"   . sboo-company-complete-3)

             ("<kp-4>"      . sboo-company-complete-4)
             ("<kp-left>"   . sboo-company-complete-4)

             ("<kp-5>"      . sboo-company-complete-5)
             ("<kp-begin>"  . sboo-company-complete-5)

             ("<kp-6>"      . sboo-company-complete-6)
             ("<kp-right>"  . sboo-company-complete-6)

             ("<kp-7>"      . sboo-company-complete-7)
             ("<kp-home>"   . sboo-company-complete-7)

             ("<kp-8>"      . sboo-company-complete-8)
             ("<kp-up>"     . sboo-company-complete-8)

             ("<kp-9>"      . sboo-company-complete-9)
             ("<kp-prior>"  . sboo-company-complete-9)

             ("<kp-0>"      . sboo-company-complete-10)
             ("<kp-insert>" . sboo-company-complete-10)

             )

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
    (setq projectile-globally-ignored-directories
          (append sboo-exclusions-directories       projectile-globally-ignored-directories))
    (setq projectile-globally-ignored-files
          (append sboo-exclusions-file-names        projectile-globally-ignored-files))
    (setq projectile-globally-ignored-file-suffixes
          (append sboo-exclusions-file-extensions   projectile-globally-ignored-file-suffixes))
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
;;; `magit': "eMAcs GIT".

(use-package magit

  :bind (("s-g s" . magit-status)
         )
  ;; ^ 

  :init
  (setq magit-save-repository-buffers 'dontask)

  :config
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
;;; External Packages: Formats ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :bind (:map markdown-mode-map
              ("TAB" . dabbrev-expand)
         :map gfm-mode-map
              ("TAB" . dabbrev-expand))

  :init (setq markdown-command "multimarkdown")

  :commands (markdown-mode gfm-mode))

;; ^ 
;;
;; `gfm-mode' abbreviates "GitHub-flavored markdown".
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode

  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'"  . yaml-mode)
         )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package xpm
;;   :commands (xpm-grok xpm-finish xpm-raster xpm-as-xpm xpm-put-points xpm-generate-buffer)
;;   :mode (("\\.xpm\\'" . c-mode))
;;   ; :mode (("\\.xpm\\'" . xpm-mode))
;;   ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Miscellaneous ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require which-key ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require rainbow-mode ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require volatile-highlights ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Finalization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-fonts nil t)
  (sboo-fonts-config!))

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