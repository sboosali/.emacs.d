;;; `init.el' for the `sboo' profile. -*- lexical-binding: t -*-

;;; Commentary:

;;----------------------------------------------;;
;; My `init.el' (@sboosali on github)
;;
;; Most `use-package' statements are in this file.
;;
;; 
;;
;;----------------------------------------------;;

;;; Code:

;;----------------------------------------------;;
;;; Imports: -----------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl)
(require 'pcase)

;;----------------------------------------------;;
;; Imports (Bootstrapped) ----------------------;;
;;----------------------------------------------;;

(eval-and-compile

  (let* ((EmacsDirectory     (or user-emacs-directory
                                 "~/.emacs.d/"))

         (SbooDirectory      (file-name-as-directory (concat EmacsDirectory
                                                             "sboo/")))

         (SbooDefinitionsFile (file-truename (concat SbooDirectory
                                                     "sboo-definitions.el")))

            ;TODO EnvironmentVars
         )

    (require 'sboo-definitions SbooDefinitionsFile)

  ;; (require 'sboo-conditions SbooConditionsFile)

  ;; (require 'sboo-utilities SbooUtilitiesFile)

    ()))

;; ^ Notes
;;
;; • `eval-and-compile' is like `progn' at both run-time AND compile-time.
;;
;; • 
;;

;;----------------------------------------------;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-debug ()

 "Toggle `debug-on-error'."

 (interactive)

 (if debug-on-error

     (progn
       (setq debug-on-error nil)
       (message "Disable `debug-on-error'."))

   (progn
     (setq debug-on-error t)
     (message "Enable `debug-on-error'."))))

;;----------------------------------------------;;

(defun add-to-load-path! (FilePath)

  "Register `FilePath' (a directory containing ELisp packages/features) with `load-path'.

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

;;----------------------------------------------;;

(defun add-startup-hook! (FunctionSymbol)

  "Register `FunctionSymbol' with `emacs-startup-hook'."

  (add-hook 'emacs-startup-hook FunctionSymbol))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defmacro sboo-append-to-list! (variable list)

  "Append (the value) LIST to (the variable) VARIABLE."

  `(setq ,variable (append ,list ,variable)))

;;----------------------------------------------;;

(cl-defun sboo-move-to-head-of-alist! (alist-var &key key)

  "`sboo-move-to-head-of-alist' with mutation.

Inputs:

• KEY — is one of: `symbolp', `stringp', or `numberp'.

• ALIST-VAR — is a symbol, representing an `alist' variable.
          its key-type is equal to the `type-of' KEY.

Output:

• ALIST-VAR.

Examples:

• M-: (setq sboo-xyz '((x . 1) (y . 2) (z . 3) (y . 4)))
• M-: (sboo-move-to-head-of-alist! 'sboo-xyz :key 'y)
    ⇒ '((y . 2) (x . 1) (z . 3))
• M-: sboo-xyz
    ⇒ '((y . 2) (x . 1) (z . 3))

• M-: (setq sboo-abc '((\"a1\" . 1) (\"b2\" . 2) (\"c3\" . 3)))
• M-: (sboo-move-to-head-of-alist! 'sboo-abc :key \"b2\")
    ⇒ '((\"b2\" . 2) (\"a1\" . 1) (\"c3\" . 3))
• M-: sboo-abc
    ⇒ '((\"b2\" . 2) (\"a1\" . 1) (\"c3\" . 3))

Laws:

• is idempotent."

  (let ((ASSERTION (boundp alist-var))
        )

    (if ASSERTION

        (let* ((alist          (symbol-value alist-var))

               (DEFAULT        :sboo-not-found)
               (TEST           #'equal)

               (VALUE          (alist-get key alist DEFAULT nil TEST))
               (WAS-KEY-FOUND? (not (eql VALUE DEFAULT)))
               )

          (if WAS-KEY-FOUND?

              (let* ((PREDICATE (lambda (KV)
                                  (when (consp KV)
                                    (let ((K (car KV)))
                                      (equal key K)))))
                     (ATTR      (cons key VALUE))
                     )
                (set alist-var
                     (cons ATTR (seq-remove PREDICATE alist))))

            alist))

      (format-message "[sboo-move-to-head-of-alist!] assertion failed in « sboo-move-to-head-of-alist! ALIST-VAR :key KEY »: ALIST-VAR must be a `boundp' symbol; the ALIST-VAR given was « %S »."
                      alist-var))))

;; ^ Notes
;;
;; `alist-get' doesn't invoke `symbol-value' (c.f. `add-to-list'):
;;
;; M-: (alist-get 'y 'sboo-xyz)
;; Wrong argument type, `listp': sboo-xyz »
;;
;; `seq-remove' removes all:
;;
;; M-: (seq-remove  (lambda (KV) (when (consp KV) (let ((K (car KV))) (equal 'y K))))  '((x . 1) (y . 2) (z . 3) (y . 4)))
;;  ⇒ '((x . 1) (z . 3))
;;
;; `seq-remove' doesn't mutate:
;;
;; M-: (progn  (setq sboo-xyz '((x . 1) (y . 2) (z . 3) (y . 4)))  (seq-remove  (lambda (KV) (when (consp KV) (let ((K (car KV))) (equal 'y K))))  (symbol-value 'sboo-xyz))  (symbol-value 'sboo-xyz))
;;  ⇒ '((x . 1) (y . 2) (z . 3) (y . 4))
;;
;; `add-to-list' can postpend (by default, it prepends):
;;
;; M-: (progn  (setq sboo-xyz '((x . 1) (y . 2) (z . 3) (y . 4)))  (add-to-list 'sboo-xyz '(a . 5) t)  (symbol-value 'sboo-xyz))
;;  ⇒ '((x . 1) (y . 2) (z . 3) (y . 4) (a . 5))
;;
;; 
;;
;; 
;;

;;----------------------------------------------;;
;;----------------------------------------------;;

(defun sboo-insert-open-parenthesis ()

  "Insert « \"(\" »."

  (interactive)

  (insert "("))

(defun sboo-insert-close-parenthesis ()

  "Insert « \")\" »."

  (interactive)

  (insert ")"))

;;----------------------------------------------;;

(defun sboo-insert-open-square-bracket ()

  "Insert « \"[\" »."

  (interactive)

  (insert "["))

(defun sboo-insert-close-square-bracket ()

  "Insert « \"]\" »."

  (interactive)

  (insert "]"))

;;----------------------------------------------;;

(defun sboo-insert-open-curly-brace ()

  "Insert « \"}\" »."

  (interactive)

  (insert "{"))

(defun sboo-insert-close-curly-brace ()

  "Insert « \"}\" »."

  (interactive)

  (insert "}"))

;;----------------------------------------------;;
;;----------------------------------------------;;



;;----------------------------------------------;;
;; Settings ------------------------------------;;
;;----------------------------------------------;;

(setq custom-file sboo-custom-file)

;;----------------------------------------------;;

(dolist (BINDING '( (lexical-binding . t)
                    ))
  (add-to-list 'safe-local-variable-values BINDING))

;;----------------------------------------------;;

(progn
  
  (dolist (BINDING '( (progn (dante-mode 0) (flycheck-mode 0))
                      ))
    (add-to-list 'safe-local-eval-forms BINDING))

  ;; ^ URL `http://endlessparentheses.com/a-quick-guide-to-directory-local-variables.html'

  (put 'dante-target       'safe-local-variable #'stringp)
  (put 'dante-project-root 'safe-local-variable #'stringp)

  ())

;; ^ Ensure `dante-*' variables are marked as "safe strings".
;; (NOTE `dante' does this, but haskell files may be opened before(?) `dante' is loaded.)

;;----------------------------------------------;;
;; Register LoadPaths --------------------------;;
;;----------------------------------------------;;

(add-to-load-path! sboo-root-directory)
(add-to-load-path! sboo-lisp-directory)

;;----------------------------------------------;;
;; Settings ------------------------------------;;
;;----------------------------------------------;;

(sboo-load-file! "sboo-settings.el")
(sboo-load-file! "sboo-aliases.el")
(sboo-load-file! "sboo-commands.el")
(sboo-load-file! "sboo-keybindings.el")

;;; TODO add-to-list

;;----------------------------------------------;;

(when (and (>= emacs-major-version 26)
           (require 'sboo-autosave nil :noerror)
           )

  (sboo-autosave-init!)

  (add-startup-hook! #'sboo-autosave-config!))

;;----------------------------------------------;;;

(when (require 'sboo-auto-mode nil :noerror)

  ;;------------------------;;

  (sboo-add-auto-mode-basename "LICENSE" #'text-mode)
  (sboo-add-auto-mode-basename "TODO"    #'text-mode)
  (sboo-add-auto-mode-basename "NOTES"   #'text-mode)

  (sboo-add-auto-mode-basename ".gitignore"     #'conf-mode)
  (sboo-add-auto-mode-basename ".gitattributes" #'conf-mode)

  ;;------------------------;;

  (sboo-add-auto-mode-file-extension "service" #'conf-mode)

  (sboo-add-auto-mode-file-extension "rc" #'conf-mode)

  ;; ^ Most `.rc' files (including « ~/.config/xfce4/panel/*-*.rc »),
  ;; have the `INI' format, which `conf-mode' supports.

  (sboo-add-auto-mode-file-extension "knsrc" #'conf-mode)

  ;; ^ `.knsrc' files (for KDE)
  ;; have the `INI' format, which `conf-mode' supports.

  ;;TODO any file that ends in `rc`, should we default to 'conf-mode or to 'sh-mode?
  ;;;(add-to-list 'auto-mode-alist ("rc\\'" . #'conf-mode))
  ;;;(add-to-list 'auto-mode-alist ("rc\\'" . #'sh-mode))

  ;;---------------------------;;

  (sboo-add-auto-mode-file-extension "xml" #'sgml-mode)
  ;; ^ `nxml-mode' vs `sgml-mode'.

  ;;---------------------------;;

  ())

;; ^ `auto-mode-alist' maps filepaths to major-modes.

;;----------------------------------------------;;

;; (when (and (>= emacs-major-version 24)
;;            (require 'sboo-theme nil :noerror))
;;   (add-to-list 'custom-theme-load-path sboo-theme-directory)
;;   (sboo-theme-set!)
;;   ())

;; ^ `load-theme':
;;
;; (defun load-theme (THEME &optional NO-CONFIRM NO-ENABLE)

;;----------------------------------------------;;

;;(defvar sboo-submodule-solarized-directory

(when (>= emacs-major-version 24)
  (sboo-register-submodule-packages! "solarized")
  (sboo-register-submodule-themes!   "solarized")
  (load-theme 'solarized :noconfirm)
  ())

;;----------------------------------------------;;

(when (require 'sboo-desktop nil :noerror)

  (sboo-desktop-init!)

  ;; (if (require 'sboo-xdg nil :noerror)
  ;;     (setq bookmark-default-file (sboo-xdg-cache "desktop.el"))
  ;;   (setq bookmark-default-file "desktop.el"))

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

;;----------------------------------------------;;

;; (use-package desktop
;;   :config
;;   (progn
;;     (defvar modi/no-desktop-read-at-startup nil
;;       "Set this variable to a non-nil value if you do not want to enable
;; `desktop-save-mode'.
;; This variable can be used to start emacs without reading the previously
;; saved desktop at startup:
;; > emacs --eval \"(setq modi/no-desktop-read-at-startup t)\"
;; ")
;;     ()))

;;----------------------------------------------;;

;; (use-package conf-mode

;;   :init
;;   ()

;;   :config
;;     (defun modi/conf-quote-normal ()
;;       "Enable `conf-quote-normal' for *.setup files."
;;       (when-let* ((fname (buffer-file-name))
;;                   (enable-conf-quote-normal (string-match-p "\\.setup.*" fname)))
;;         ;; Set the syntax of ' and " to punctuation.
;;         (conf-quote-normal nil)))
;;     (add-hook 'conf-space-mode-hook #'modi/conf-quote-normal)
;;   ())

;; `conf-quote-normal':
;;
;; « 0 » — Set the syntax of « ' » and « " » to punctuation.
;; « 1 » — Set the syntax of only « ' » to punctuation.
;; « 2 » — Set the syntax of only « " » to punctuation.
;;
;; 

;;----------------------------------------------;;

(use-package lisp-mode

  :init

  ()

  ;; :bind
  ;; ("[" . sboo-insert-open-parenthesis)
  ;; ("]" . sboo-insert-close-parenthesis)
  ;; ("(" . sboo-insert-open-square-bracket)
  ;; (")" . sboo-insert-close-square-bracket)

  :config

  ())

;;----------------------------------------------;;

;; (use-package compile
;;   :bind (("s-m" . compile))
;;   :config 
;;          
;;                 compilation-read-command nil
;;                 compile-command "make")
;;  ())

(when (require 'sboo-compilation nil :noerror)

  (sboo-compilation-init!)

  (add-startup-hook! #'sboo-compilation-config!))

;;----------------------------------------------;;

(use-package ansi-color

  :config

  (defun sboo-ansi-colors-apply ()

    "`ansi-color-apply-on-region' on whole buffer."

    (interactive)

    (ansi-color-apply-on-region (point-min) (point-max)))

  ())

;;----------------------------------------------;;

(use-package shell

  :config

  (push
   (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
   ;; ^
   ;;
   ;; How To Override The Default Behavior Of  "M-x shell" Opening A New Buffer In Another Window (e.g. splitting).
   ;;
   ;; see:
   ;;    https://stackoverflow.com/questions/40301732/m-x-shell-open-shell-in-other-windows
   ;;
   ;; >  The command `shell` uses `pop-to-buffer`.
   ;; > If you have the Emacs source code, you can see it for yourself by running `C-h d f shell` to open the function's documentation (and then clicking the link to the function's source).
   ;; `pop-to-buffer` can be configured via `display-buffer-alist`. 

  :bind

  ( ("<s>-s" . shell)
  
    :map shell-mode-map

    ("<kp-prior>" . comint-previous-input)
    ;; ^ <prior> is the page-up key.
    ;; `comint-previous-input` is like "press down in a terminal-emulator".
    
    ("<kp-next>" . 'comint-next-input)
    ;; ^ <next> is the page-down key.
    ;; `comint-next-input` is like "press up in a terminal-emulator".

    ))

;;----------------------------------------------;;
;;; Internal Packages --------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-server nil :noerror)
  (add-startup-hook! #'server-start-unless-running))

;;----------------------------------------------;;

(require 'sboo-widgets)
;;;  (sboo-minibuffer-config))
;;;  (sboo-config-fonts))

;;----------------------------------------------;;

(when (require 'sboo-make nil :noerror)
  (add-hook 'makefile-mode-hook #'sboo-show-trailing-whitespace))

;;----------------------------------------------;;

;; (when (require 'sboo-prog nil :noerror)
;;   (dolist (HOOK sboo-prog-mode-hooks)
;;     (add-hook 'prog-mode-hook HOOK)))

;;----------------------------------------------;;

(when (require 'sboo-shell nil :noerror)

  ;;---------------------------;;

  (defadvice term-char-mode (after term-char-mode-fixes ())

    (set (make-local-variable 'cua-mode) nil)
    ;; ^ Disable `cua-mode' to enable `?\C-x' for escaping.
    (set (make-local-variable 'transient-mark-mode) nil)
    (set (make-local-variable 'global-hl-line-mode) nil)

    (ad-activate 'term-char-mode)

    (term-set-escape-char ?\C-x))

  ;;---------------------------;;

  (add-hook 'term-mode-hook #'sboo-local-unset-tab))

;;----------------------------------------------;;

(when (require 'dired nil :noerror)

  ;;---------------------------;;

  (bind-keys :map dired-mode-map
             ("w" . wdired-change-to-wdired-mode) ;; Mnemonic: [w]dired.
             )

  ;; ^ Shadows `dired-copy-filename-as-kill':
  ;;
  ;; (define-key map "w" 'dired-copy-filename-as-kill)

  (setq wdired-allow-to-change-permissions t)

  ;; ^ Edit permission bits directly.
  ;; wdired ensures you can only enter valid permission bits.

  (setq wdired-allow-to-redirect-links t)

  ;; ^ Change symlinks.

  (setq wdired-use-dired-vertical-movement 'sometimes)

  ;; ^ `sometimes' means — Emacs will move the point to the beginning of filename, if the point is before it.

  ;;---------------------------;;

  ())

;; ^ WDired
;;
;; <https://www.masteringemacs.org/article/wdired-editable-dired-buffers>
;;
;; 
;;
;;

;;----------------------------------------------;;

(when (require 'sboo-bookmark nil :noerror)

  (sboo-bookmark-init!)

  (add-startup-hook! #'sboo-bookmark-config!))

;; ^ Some bookmarking commands:
;;
;; ‘C-x r m’ – set a bookmark at the current location (e.g. in a file)
;; ‘C-x r b’ – jump to a bookmark
;; ‘C-x r l’ – list your bookmarks
;; ‘M-x bookmark-delete’ – delete a bookmark by name
;;
;; Your personal bookmark file is defined by option ‘bookmark-default-file’, which defaults to `~/.emacs.d/bookmarks

;;----------------------------------------------;;

(when (require 'saveplace nil 'noerror)

  (setq-default save-place t)
  ;TODO; (setq save-place-file (expand-file-name "save-point-places" user-emacs-directory))

  ())

;; ^ Save point position between sessions.
;;
;; "Save the position I was in each file, i.e. no scrolling down to paragraph N or function foo when I reopen my files."
;;

;;----------------------------------------------;;

(when (require 'savehist nil 'noerror)

  (setq savehist-additional-variables '(search-ring regexp-search-ring))

  (setq savehist-file (emacs-file "savehist.el"))

  (savehist-mode t)

  ())

;; ^ Save History.
;;
;; Save mode-line history between sessions.
;;

;;----------------------------------------------;;

(when (require 'man nil 'noerror)

  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face    :bold t)
  (set-face-attribute 'Man-underline  nil :inherit font-lock-keyword-face :underline t)

  ())

;; (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "orange red")
;; (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "forest green")

;; Or to be theme agnostic:

;; (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
;; (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

;;----------------------------------------------;;
;; `package--builtins': ------------------------;;
;;----------------------------------------------;;

;; these features (below) can be configured with `use-package',
;; because they are actual packages.
;;
;; See: (describe-variable 'package--builtins)

;;----------------------------------------------;;

(use-package calendar
  :defer t
  :config (setq calendar-week-start-day 1))

;;----------------------------------------------;;

(use-package vc
  :defer t
  :config (setq vc-follow-symlinks t))

;;----------------------------------------------;;

;; no multiframe ediff please
(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;----------------------------------------------;;

;;----------------------------------------------;;
;; Internal Packages: Settings -----------------;;
;;----------------------------------------------;;

(progn

  (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

  ())

;;----------------------------------------------;;
;; Internal Packages: Utilities ----------------;;
;;----------------------------------------------;;

(when (require 'sboo-unicode nil :noerror)

  ())

;;----------------------------------------------;;

;;----------------------------------------------;;
;; External Packages: Installation -------------;;
;;----------------------------------------------;;

(pcase (sboo-install-p)

  ('submodules (progn
                 (sboo-register-submodule-packages! "use-package/")
                 (sboo-register-submodule-packages! "helm/")
                 
                 (when (< emacs-major-version 26)
                   (sboo-register-submodule-packages! "real-auto-save/"))))

  ('melpa      (progn
                 (sboo-load-file! "sboo-packages-by-installing.el")))

  ('nixpkgs    (progn))

  (_           (progn)))

;;----------------------------------------------;;
;; External Packages: `package.el' -------------;;
;;----------------------------------------------;;

(when (require 'sboo-packages nil :noerror)

  ;;------------------------;;

  (dolist (ARCHIVE sboo-package-archives)
    (add-to-list 'package-archives ARCHIVE 'append))

  ;;------------------------;;

  (when (>= emacs-major-version 26)
    (async-start #'package-refresh-contents))

  ;;------------------------;;

  (setq package-load-list sboo-all-packages)

  ;;------------------------;;

  ;;(package-initialize)
  ;;NOTE we call `package-initialize' in `init.el'.

  ;;------------------------;;

  ())

;; ^ `async-start':
;;
;;     (async-start START-FUNC &optional FINISH-FUNC)
;;
;; Execute START-FUNC (often a lambda) in a subordinate Emacs process.
;; When done, the return value is passed to FINISH-FUNC.  Example:

;;----------------------------------------------;;
;; External Packages: Core Configuration -------;;
;;----------------------------------------------;;

(sboo-load-file! "sboo-init-helm.el")
(sboo-load-file! "sboo-init-use-package.el")

(when (< emacs-major-version 26)
  (sboo-load-file! "sboo-init-real-auto-save.el"))

;;----------------------------------------------;;
;; External Packages: Miscellaneous ------------;;
;;----------------------------------------------;;

(use-package flycheck

  :defer t

  ;;^
  ;; deferred because flycheck is "more framework than application".
  ;; i.e. any "application" package will `require` it whenever needed (e.g. `dante`), and afaik, it's not useful alone.

  ;;TODO;; style « *Flycheck error messages* »

  ;;TODO;; mode of « *Flycheck errors* » (e.g. « *Flycheck errors for buffer ...* »)

  :config

  (add-hook 'flycheck-error-list-mode-hook #'visual-line-mode)

  (when (require 'sboo-flycheck nil :no-error)
    (bind-key "<kp-divide>" #'sboo-flycheck)
    (add-to-list 'display-buffer-alist sboo-flycheck-display-buffer))

  ())

;; See
;;     - « https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-flycheck.el »
;;

;;----------------------------------------------;;


;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.
;; http://jblevins.org/projects/deft
;; https://github.com/jrblevin/deft

;; (use-package deft
;;   :config

;;----------------------------------------------;;

(use-package edit-indirect

  :commands (edit-indirect-region)

  :config

  (defun sboo-edit-indirect-guess-mode (parent-buffer parent-region-begin parent-region-end)

    "Guess the major mode for an edit-indirect buffer.

Calls `set-auto-mode', which parses the « mode » file-local (special) variable 
(i.e. « -*- mode: ... -*- »)."

    (set-auto-mode t))

  (customize-set-variable 'edit-indirect-guess-mode-function
                          #'sboo-edit-indirect-guess-mode
                          "`edit-indirect-guess-mode-function' is `edit-indirect-default-guess-mode' by default.")

  ())

;;----------------------------------------------;;

;; (use-package awesome-tab

;;   :config

;;   (awesome-tab-mode t)

;;   ())
;;TODO;; helm-source-list awesome-tab-build-helm-source)

;;----------------------------------------------;;

;; ;;
;; ;;  bm
;; ;;  bookmark+ (bmkp)
;; ;;  Quickly save and restore point using registers

;; ;;; bm
;; ;; https://github.com/joodland/bm
;; (use-package bm

;;   :config
;;   (progn
;;     (setq-default bm-buffer-persistence t) ; buffer persistence on by default

;;     (when (display-graphic-p) ; Add fringe only if display is graphic (GUI)
;;       (define-fringe-bitmap 'bm-marker-left [#xF8    ; ▮ ▮ ▮ ▮ ▮ 0 0 0
;;                                              #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
;;                                              #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
;;                                              #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
;;                                              #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
;;                                              #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
;;                                              #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
;;                                              #xF8])) ; ▮ ▮ ▮ ▮ ▮ 0 0 0
;;     ()))

;;----------------------------------------------;;
;; External Packages: Haskell Configuration ----;;
;;----------------------------------------------;;

(when (require 'sboo-haskell nil :noerror)

  ;;------------------------;;

  (use-package haskell
    :demand t

    :commands    (haskell-mode)

    :interpreter (("runhaskell"  . haskell-mode)
                  ("runghc"      . haskell-mode)
                  ("cabal"       . haskell-mode)
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

      ())

    (when (require 'sboo-ghc nil :no-error)

      (dolist (EXTENSION sboo-ghc-language-extensions)
        (cl-pushnew EXTENSION
                    haskell-ghc-supported-extensions
                    :test #'equal))

      (dolist (OPTION sboo-ghc-compiler-options)
        (cl-pushnew OPTION
                    haskell-ghc-supported-options
                    :test #'equal)))

    ())

  ;;------------------------;;

  (use-package haskell-decl-scan
    :after    haskell
    
    :commands (haskell-decl-scan-mode)
    
    ;;;:hook     ((haskell-mode . haskell-decl-scan-mode))

    :init
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode))

  ;;------------------------;;

  (use-package haskell-cabal
    :after    haskell

    :commands (haskell-cabal-mode)

    :mode        (("\\.cabal\\'"      . haskell-cabal-mode)
                  ("\\.project\\'"    . haskell-cabal-mode)
                  ("\\`\\.project\\'" . haskell-cabal-mode)))

  ;;------------------------;;

  (use-package dante
    :after    haskell

    :load-path "~/.emacs.d/submodules/dante/" ;TODO 

    :commands (dante-mode dante-restart)

    :bind (:map haskell-mode-map
                (("C-c d" . sboo-dante-mode)))

;;;  :hook ((haskell-mode . flycheck-mode)
;;;         (haskell-mode . dante-mode))

    :init

    (add-hook 'haskell-mode-hook #'flycheck-mode)
    (add-hook 'haskell-mode-hook #'dante-mode)

    :config

    (dolist (KEY '(nix new-nix))
      (setq dante-methods-alist
            (assq-delete-all KEY dante-methods-alist)))

    (when sboo-dante-default-method
      (sboo-move-to-head-of-alist! 'dante-methods-alist
                                   :key sboo-dante-default-method))

    (setq dante-tap-type-time 2)

    (setq sboo-haskell-eldoc 'dante)

    ;(add-to-list 'display-buffer-alist sboo-dante-display-buffer)

    ())

  ;; ^ Configure `dante':
  ;;
  ;; * load `dante.el', which registers `dante-target' and `dante-project-root' as `safe-local-var's.
  ;; * `autoload' the `dante-mode' command, so we can run 《 M-x dante-mode 》 manually.
  ;; 
  ;; 

  ;;------------------------;;

  ())

;;----------------------------------------------;;
;; External Packages: ProgrammingLanguages -----;;
;;----------------------------------------------;;

(when (require 'sboo-nix nil :noerror)

  ;;------------------------;;

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

  ;;------------------------;;

  (use-package nix-repl)

  ;;------------------------;;

  ())

;;----------------------------------------------;;
;; External Packages: `company-*' Configurations
;;----------------------------------------------;;

(when (require 'sboo-company nil :noerror)

  ;;------------------------;;

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

  ;;------------------------;;

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

  ;;------------------------;;

  (use-package company-cabal

    :init
    (add-to-list 'company-backends #'company-cabal)

    ())

  ;;------------------------;;

  (use-package company-ghci

    :init

    (push #'company-ghci company-backends)
    
    (add-hook 'haskell-mode-hook             #'company-mode)
    (add-hook 'haskell-interactive-mode-hook #'company-mode)
    ;; ^ for completions in the REPL

    ())

  ;;------------------------;;

  ;; (use-package company-web

  ;;   :init
  ;;   (dolist (HOOK '(js-mode-hook
  ;;                   js2-mode-hook
  ;;                   js3-mode-hook
  ;;                   inferior-js-mode-hook))

  ;;     (add-hook HOOK #'sboo-company-javascript))
  ;;   ())

  ;;------------------------;;

  ;; (use-package company-anaconda
  ;;   ;
  ;;   :init
  ;;   (add-hook 'python-mode-hook #'sboo-company-python)
  ;;   ;
  ;;   ())

  ;;------------------------;;

  ;;------------------------;;

  ;;------------------------;;

  ())

;;----------------------------------------------;;

(when (require 'sboo-haskell-compilation nil :noerror)

  

  ())

;;----------------------------------------------;;

(use-package helm-gtags)

;;----------------------------------------------;;

;;----------------------------------------------;;
;; External Packages: Miscellaneous ------------;;
;;----------------------------------------------;;

(when (require 'sboo-projectile nil :noerror)

  (use-package projectile

    ;;;TODO :delight '(:eval (concat " " (projectile-project-name)))

    ;; ^
    ;; [1] Hide the mode name for projectile-mode;
    ;; [2] Show the project name instead.

    :config

    (sboo-append-to-list! projectile-globally-ignored-directories
                          sboo-excluded-directories)

    (sboo-append-to-list! projectile-globally-ignored-files
                          sboo-excluded-file-names)

    (sboo-append-to-list! projectile-globally-ignored-file-suffixes
                          sboo-excluded-file-extensions)

    ()))

;;----------------------------------------------;;

(when (require 'sboo-yasnippets nil :noerror)

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

    ()))

;;----------------------------------------------;;
;;; `magit': "eMAcs GIT".

(progn
  
  (use-package magit

    :bind (("s-g s" . magit-status)
           )
    ;; ^ 

    :init
    (setq magit-save-repository-buffers 'dontask)

    :config
    ())

  ;;--------------------------;;

  ;; (use-package magithub
  ;;FIXME crashes magit
  ;;   )

  ())

;;----------------------------------------------;;
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

;;----------------------------------------------;;
;; External Packages: Formats ------------------;;
;;----------------------------------------------;;

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

;;----------------------------------------------;;

(use-package yaml-mode

  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'"  . yaml-mode)
         )

  )

;;----------------------------------------------;;

;; (use-package xpm
;;   :commands (xpm-grok xpm-finish xpm-raster xpm-as-xpm xpm-put-points xpm-generate-buffer)
;;   :mode (("\\.xpm\\'" . c-mode))
;;   ; :mode (("\\.xpm\\'" . xpm-mode))
;;   ())

;;----------------------------------------------;;
;; External Packages: Miscellaneous ------------;;
;;----------------------------------------------;;

;;(require which-key ())

;;----------------------------------------------;;

;;(require rainbow-mode ())

;;----------------------------------------------;;

;;(require volatile-highlights ())

;;----------------------------------------------;;

(when (require 'volatile-highlights nil 'noerror)
  (volatile-highlights-mode t))

;;----------------------------------------------;;

;; (use-package which-key
;;   ;;
;;   :diminish which-key-mode
;;   ;;
;;   :init
;;   (setq which-key-idle-secondary-delay 0.5)
;;   (setq which-key-idle-delay           1.0)
;;   ;;
;;   :config
;;   (which-key-mode t))
;; ;; ^ After 1 second of an unfinished key-press,
;; ;; show the documentation of the sub-keys available in the key sequence.


;;----------------------------------------------;;
;; Builtin Features: ---------------------------;;
;;----------------------------------------------;;

;; Non-Package features must be configured more because,
;; with `with-eval-after-load', `define-key', etc.

(with-eval-after-load 'comint

  (bind-key "<up>"   'comint-previous-matching-input-from-input comint-mode-map)
  (bind-key "<down>" 'comint-next-matching-input-from-input     comint-mode-map)

  (setq comint-scroll-to-bottom-on-output 'others)
  (setq comint-scroll-to-bottom-on-input  'this)

  ())

;;----------------------------------------------;;

;;----------------------------------------------;;
;; Finalization --------------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-fonts nil :noerror)
  (sboo-fonts-config!))

;;----------------------------------------------;;
;;; Notes: -------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

;;----------------------------------------------;;
;; `wgrep' notes

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

;;----------------------------------------------;;
;; `markdown-mode' notes

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

;;----------------------------------------------;;
;; DirEd

;; (use-package ranger
;;   :defer t
;;
;;   :init
;;   (setq ranger-preview-file t)
;;   (setq ranger-show-literal nil)
;;   ())

;;----------------------------------------------;;

;; (use-package dired-sidebar
;;   :commands (dired-sidebar-toggle-sidebar))

;; ^ `dired-sidebar':
;;
;; https://github.com/jojojames/dired-sidebar/blob/master/readme.org

;;----------------------------------------------;;

;; `dired-hack':
;;
;; https://github.com/Fuco1/dired-hacks/blob/master/README.md#dired-subtree


;;----------------------------------------------;;
;;

;; (use-package vlf

;;   :config
;;   (require 'vlf-setup)

;;   )

;;----------------------------------------------;;
;;

;;----------------------------------------------;;
(provide 'sboo-init)