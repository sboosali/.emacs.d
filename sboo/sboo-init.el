;;; sboo-init.el --- sboosali's ‘init.el’ -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal initialization (for me, “sboosali”.)
;;
;; This file consolidates all my personal configurations.
;; Mostly via:
;;
;; • `use-package' declarations.
;; • `require'-ing « sboo-* » `featurep's.
;;
;; Symbol Naming:
;;
;; • « sboo-* » — most functions/variables are namespaced under ‘sboo’.
;;
;; File Naming:
;;
;; • « sboo-init-* » — execute effects, should be `load'ed.
;; • « sboo-* » — only define functions/variables, should be `require'd.
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'seq))

;;; BOOTSTRAPPING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;----------------------------------------------;;
;; Settings: Bootstrapping ---------------------;;
;;----------------------------------------------;;

;; Configure Loading-Settings before calling `load-file':

(setq load-prefer-newer t)

;; ^ never accidentally `load' outdated (byte-compiled) files.

;;----------------------------------------------;;
;; Imports: Bootstrapping ----------------------;;
;;----------------------------------------------;;

(eval-and-compile

  (let* ((EmacsDirectory     (or (bound-and-true-p user-emacs-directory)
                                 "~/.emacs.d"))

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
;;

;;----------------------------------------------;;
;; Functions: Bootstrapping --------------------;;
;;----------------------------------------------;;

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
;; Constants: Bootstrapping --------------------;;
;;----------------------------------------------;;

(defconst sboo-current-directory

  (file-name-directory (or load-file-name buffer-file-name))

  "Runtime Directory with « sboo-*.el » files.

=== Implementation ===

• variable `load-file-name'   — non-nil when evaluated under `load-file'.
• variable `buffer-file-name' — non-nil when evaluated under `eval-buffer'.

URL `http://ergoemacs.org/emacs/elisp_relative_path.html'")

;;----------------------------------------------;;
;; Settings (early) ----------------------------;;
;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s"
  (custom-set-variables

   '(enable-local-variables :safe :eager nil "set only Safe Variables (don't query for unsafe ones).")

   ;; ^ Set `enable-local-variables' early (to prevent Confirmation Prompts like « _ may not be safe. Enable it? y, n, !. »).

   '(tool-bar-style 'both :eager nil "each Icon of the Tool Bar has both Image (above) and Label (below).")

   ;; ^ Set `tool-bar-style' early (to prevent interface resizing/thrasing during Emacs).

   ))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defmacro sboo-append-to-list! (variable list)

  "Append, to (the variable) VARIABLE, (the value) LIST."

  `(setq ,variable (append ,list ,variable)))

;;----------------------------------------------;;

(eval-when-compile

  ;;--------------------------;;

  (defmacro sboo-custom-set (variable expression &optional comment)

    "`custom-set-variables' wrapper."
    
    (declare (indent 2) (doc-string 3))

    `(with-demoted-errors "[Warning] %s"
       (custom-set-variables
        (list (quote ,variable) (quote ,expression) :eager nil ,comment)))) ;TODO handle variables, not just symbols

  ;;--------------------------;;

  

  ;;--------------------------;;

  ())

;;----------------------------------------------;;

(defun sboo-use-package-init ()

  "Register and initialize `use-package'.

Links:

• URL `https://github.com/jwiegley/use-package'"

  (progn

    (sboo-register-submodule-packages! "use-package")

    (require 'use-package)

    (setq use-package-verbose t)))

;;----------------------------------------------;;

(cl-defun sboo-parse-boolean (string &key default)

  "Parse an INI-style or EnvironmentVariable-style boolean.

Inputs:

• STRING — a `stringp'.

Output:

• a `booleanp'.

Examples (true):

• M-: (sboo-parse-boolean \"1\")
    ⇒ t
• M-: (sboo-parse-boolean \"yes\")
    ⇒ t
• M-: (sboo-parse-boolean \"true\")
    ⇒ t
• M-: (sboo-parse-boolean \"y\")
    ⇒ t
• M-: (sboo-parse-boolean \"t\")
    ⇒ t
• M-: (sboo-parse-boolean \"YES\")
    ⇒ t
• M-: (sboo-parse-boolean \"True\")
    ⇒ t

Examples (false):

• M-: (sboo-parse-boolean \"0\")
    ⇒ nil
• M-: (sboo-parse-boolean \"no\")
    ⇒ nil
• M-: (sboo-parse-boolean \"false\")
    ⇒ nil
• M-: (sboo-parse-boolean \"n\")
    ⇒ nil
• M-: (sboo-parse-boolean \"f\")
    ⇒ nil
• M-: (sboo-parse-boolean \"NO\")
    ⇒ nil
• M-: (sboo-parse-boolean \"False\")
    ⇒ nil

Examples (default):

• M-: (sboo-parse-boolean \"\")
    ⇒ nil
• M-: (sboo-parse-boolean \"\" :default t)
    ⇒ t"

  (pcase string

    ("1"     t)
    ("yes"   t)
    ("true"  t)
    ("y"     t)
    ("t"     t)
    ("YES"   t)
    ("True"  t)

    ("0"      nil)
    ("no"     nil)
    ("false"  nil)
    ("n"      nil)
    ("f"      nil)
    ("NO"     nil)
    ("False"  nil)

    (_        (if default default nil))))

;;----------------------------------------------;;

(cl-defun sboo-getenv-boolean (environment-variable &key (default nil))

  "Get ENVIRONMENT-VARIABLE, parse as a boolean, default to DEFAULT.

Inputs:

• ENVIRONMENT-VARIABLE — a string.

Output:

• a `booleanp'.

Examples:

• M-: (sboo-getenv-boolean \"\")
    ⇒ t"

  (sboo-parse-boolean (getenv environment-variable)
                      :default default))

;;----------------------------------------------;;

(cl-defun sboo-getenv-number (environment-variable &key (default nil))

  "Get ENVIRONMENT-VARIABLE, parse as a number, default to DEFAULT.

Inputs:

• ENVIRONMENT-VARIABLE — a string.

Output:

• a `numberp'.

Examples:

• M-: (sboo-getenv-number \"EUID\")
    ⇒ 1001"

  (let ((VALUE (getenv environment-variable))
        )

    (if (equal nil VALUE)
        default

      (string-to-number VALUE))))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-desktop-enable

  (condition-case nil
      (sboo-getenv-boolean "SBOO_EMACS_DESKTOP_ENABLE" :default t)
    (error t))

  "Whether to `desktop-read'.

Related:

• `sboo-desktop-config!'")

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;
;; Import `defmacro's:

(eval-when-compile

  ;; this expression uses only Emacs Builtins.

  (let* ((USE-PACKAGE-DYNAMIC (getenv "SBOO_EMACS_USEPACKAGE"))
         (USE-PACKAGE-STATIC  "~/.emacs.d/submodules/use-package/")
         (USE-PACKAGE         (file-name-directory
                               (expand-file-name
                                (if (not (null USE-PACKAGE-DYNAMIC))
                                    USE-PACKAGE-DYNAMIC
                                  USE-PACKAGE-STATIC))))
         )

  (add-to-list 'load-path USE-PACKAGE)
  (require 'use-package (concat USE-PACKAGE "use-package.el"))
  (setq use-package-verbose t)

  ()))

;;(sboo-use-package-init)

;;----------------------------------------------;;
;; Import `defun's:

(progn
  (require 'bind-key nil :no-error)
  (require 'delight  nil :no-error))

;;----------------------------------------------;;
;; Settings ------------------------------------;;
;;----------------------------------------------;;
;; Variable Safety:

(dolist (BINDING '((lexical-binding . t)))
  (add-to-list 'safe-local-variable-values BINDING))

;;----------------------------------------------;;

(progn
  
  (dolist (BINDING '( (progn (dante-mode 0) (flycheck-mode 0))
                      ))
    (add-to-list 'safe-local-eval-forms BINDING))

  ;; ^ URL `http://endlessparentheses.com/a-quick-guide-to-directory-local-variables.html'

  (put 'dante-target       'safe-local-variable #'stringp)
  (put 'dante-project-root 'safe-local-variable #'stringp)

  (put 'firestarter 'safe-local-variable #'symbolp)

  ;; ^ `bnf-mode' has a File-Local Variable « firestarter: ert-run-tests-interactively ».

  ())

;; ^ Ensure `dante-*' variables are marked as "safe strings".
;; (NOTE `dante' does this, but haskell files may be opened before(?) `dante' is loaded.)

;; ^ e.g the `safe-local-variable' properly of the `compile-command' symbol:
;;
;;   M-: (get 'compile-command 'safe-local-variable)
;;     ⇒ '(lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command)) compilation-read-command)))
;;
;;     ⇒ '(lambda (x) (and (stringp x) (or t compilation-read-command)))
;;     ⇒ '(lambda (x) (and (stringp x) compilation-read-command))
;;     ⇒ '(lambda (x) (and (stringp x) t))
;;     ⇒ '(lambda (x) (stringp x))
;;     ⇒ 'stringp
;;

;;----------------------------------------------;;
;; `eval' Safety

;;TODO add all « pure » (and/or « side-effect-free ») builtins as `safe-local-variable' predicates...

;;(add-to-list 'safe-local-eval-forms '())
;;(add-to-list 'safe-local-variable '())

;; ^ e.g the `side-effect-free' properly of the `+' symbol:
;;
;;   M-: (get '+ 'side-effect-free)
;;     ⇒ t
;;   M-: (get '+ 'pure)
;;     ⇒ nil
;;

;; See:
;;
;; • Info node `(elisp) Standard Properties'
;; • URL `'
;;
;; • property `safe-function'
;; • property `safe-local-eval-function'
;; • property `safe-local-variable'
;;
;; • property `pure'
;; • property `side-effect-free'
;;
;; • property `variable-documentation'
;;

;;----------------------------------------------;;
;; Register ------------------------------------;;
;;----------------------------------------------;;

;; Register Code (c.f. `provide'):

(progn

  (let* ((APPEND    nil)
         (DIRECTORY (if (bound-and-true-p sboo-root-directory)
                        sboo-root-directory
                      (expand-file-name "~/.emacs.d/sboo")))
         )
    (add-to-list 'load-path DIRECTORY APPEND))

  ;; ^ prepend (to the start of `load-path'):
  ;; 
  ;;   • `sboo-root-directory' holds « sboo-*.el » `featurep's.
  ;; 

  (let* ((APPEND    t)
         (DIRECTORY (if (bound-and-true-p sboo-lisp-directory)
                        sboo-lisp-directory
                      (expand-file-name "~/.emacs.d/sboo/lisp")))
         )
    (add-to-list 'load-path DIRECTORY APPEND))

  ;; ^ append (to the end of `load-path'):
  ;; 
  ;;   • `sboo-lisp-directory' holds Vendored Packages,
  ;;     which have lower priority than Installed Packages
  ;;     (whether installed via program `nix' or via `package.el'.)
  ;;

  load-path)

;;----------------------------------------------;;
;;; Groups -------------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defgroup sboo nil

  "‹sboosali›'s customization."

  :group 'local

  ;; ^ Group `local' means: your personal (“site-local”) configuration.

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/.emacs.d#readme"))

;;----------------------------------------------;;
;;; Settings -----------------------------------;;
;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s"
  (setq custom-file sboo-custom-file))

;;----------------------------------------------;;

(require 'sboo-commands nil :no-error)

;; ^ My `commandp's.

;;----------------------------------------------;;

(require 'sboo-keymaps nil :no-error)

;; ^ My `keymapp's.

;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s" (sboo-load-file! "sboo-init-settings.el"))
(with-demoted-errors "[Warning] %s" (sboo-load-file! "sboo-init-keybindings.el"))
(with-demoted-errors "[Warning] %s" (sboo-load-file! "sboo-init-aliases.el"))

;; ^ NOTE `load' these files (rather than `require' them)
;;        because they execute statements
;;        (rather than `provide' definitions).
;;
;;        Later configuration (i.e. `sboo-*' features) musn't depend on them
;;        (whether explicitly via `require' or implicitly via reference).
;;        Instead, « sboo-utilities.el » has any general-purpose dependencies.
;;        Hence, we `ignore-errors' (rather than aborting the configuration).
;;

;;----------------------------------------------;;

(require 'sboo-xah nil :no-error)

;; ^ `commandp's by Xah Lee.

;;----------------------------------------------;;

(unless (require 'sboo-private "~/.emacs.d/private/sboo-private" :no-error)
  (require 'sboo-private nil :no-error))

;; ^ NOTE if the "true" `sboo-private' can't be loaded
;;        (e.g. doesn't exist yet, file doesn't parse, etc),
;;        load a "fake" one which is always available
;;        (because it's version-controlled,
;;        but doesn't contain any sensitive information.)
;;

;;==============================================;;
;;; Themes -------------------------------------;;
;;----------------------------------------------;;

;; Register Themes (c.f. `provide-theme'):

(with-demoted-errors "[Warning] %s"

  (defvar sboo-initital-theme 'solarized
    "Theme Name to initially load (via `load-theme').

  a `symbolp'.")

  (when (>= emacs-major-version 24)

    (add-to-theme-path! sboo-theme-directory)
    (add-to-theme-path! (emacs-subdir "themes"))

    (progn
      (sboo-register-submodule-packages! "solarized")
      (sboo-register-submodule-themes!   "solarized")
      (load-theme sboo-initital-theme :no-confirm))

    ()))

;;----------------------------------------------;;

;; Register Icons:

(with-demoted-errors "[Warning] %s"

  (add-to-icon-path! sboo-icon-directory)
  (add-to-icon-path! (emacs-subdir "icons"))

  ())

;;; INTERNAL PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;==============================================;;
;;; Builtin Packages:
;;----------------------------------------------;;

(use-package emacs

  :no-require t

  :delight

  (visual-line-mode " VL")
  ;; ^ Shorten `visual-line-mode'.

  (auto-fill-function " AF")
  ;; ^ Shorten `auto-fill-mode'.

  (buffer-face-mode)
  ;; ^ Hide `buffer-face-mode'.

  :config

  ())

;;----------------------------------------------;;

(when (require 'sboo-auto-mode nil :no-error)

  ;;------------------------;;

  (add-to-list 'auto-mode-alist (cons (rx bos "TODO" eos) #'text-mode))

  (sboo-add-auto-mode-basename "LICENSE"         #'text-mode)
  (sboo-add-auto-mode-basename "NOTES"           #'text-mode)

  (sboo-add-auto-mode-basename "^Procfile\\'"    #'conf-mode)

  (sboo-add-auto-mode-basename "^\\.jshintrc\\'" #'json-mode)

  (sboo-add-auto-mode-basename "^Portfile\\'"    #'tcl-mode)

  ;;------------------------;;

  (add-to-list 'auto-mode-alist (cons "\\.xpm\\'" #'c-mode))

  (sboo-add-auto-mode-file-extension "xml" #'nxml-mode)

  ;; ^ `nxml-mode' vs `sgml-mode'?

  ;;---------------------------;;

  'sboo-auto-mode)

;; ^ NOTES:
;;
;;  • `auto-mode-alist' maps filepaths to `major-mode's.
;;

;;----------------------------------------------;;

;; (when (and (>= emacs-major-version 24)
;;            (require 'sboo-theme nil :no-error))
;;   (add-to-list 'custom-theme-load-path sboo-theme-directory)
;;   (sboo-theme-set!)
;;   ())

;; ^ NOTES:
;;
;;   • `load-theme''s signature:
;;
;;       (defun load-theme (THEME &optional NO-CONFIRM NO-ENABLE)
;;

;;==============================================;;
;;; Feature: Autosaving:
;;----------------------------------------------;;

(when (require 'sboo-autosave nil :no-error)

  (if (>= emacs-major-version 26)

      ;; `auto-save-visited-mode':

      (progn
        (sboo-autosave/auto-save-visited/init!)
        (sboo-autosave/auto-save-visited/config!))

    ;; `real-auto-save-mode':

    (progn
      (eval-after-load 'real-auto-save
        `(progn
           (sboo-autosave/real-auto-save/init!)
           (sboo-autosave/real-auto-save/config!)))
      (require 'real-auto-save nil :no-error))))

;;==============================================;;
;;; Feature: Sessions:
;;----------------------------------------------;;

(when (require 'sboo-desktop nil :no-error)

  (sboo-desktop-init!)

  ;; (if (require 'sboo-xdg nil :no-error)
  ;;     (setq bookmark-default-file (sboo-xdg-cache "desktop.el"))
  ;;   (setq bookmark-default-file "desktop.el"))

  (add-startup-hook! #'sboo-desktop-config!)

;;(sboo-desktop-mode)

  ())

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

;;TODO:
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

;;==============================================;;
;;; Feature: Server (for ‘emacsclient’)
;;----------------------------------------------;;

(when (require 'sboo-server nil :no-error)
  (add-startup-hook! #'server-start-unless-running))

;;==============================================;;
;;; Builtin Packages: Widgets (Menubar/Toolbar):
;;----------------------------------------------;;

(when (require 'sboo-toolbar nil :no-error)
  (sboo-toolbar-setup))

;;----------------------------------------------;;

(when (require 'sboo-menubar nil :no-error)
  (sboo-menubar-setup))

;;----------------------------------------------;;

(require 'sboo-widgets nil :no-error)
;;;  (sboo-minibuffer-config))
;;;  (sboo-config-fonts))

;;----------------------------------------------;;
;;; Builtin Packages: Text Modes ---------------;;
;;----------------------------------------------;;

(progn

  (defun modi-conf-quote-normal ()
    "Enable `conf-quote-normal' for *.setup files."
    (when-let* ((fname (buffer-file-name))
                (enable-conf-quote-normal (string-match-p "\\.setup.*" fname)))
      ;; Set the syntax of ' and " to punctuation.
      (conf-quote-normal nil)))

  (add-hook 'conf-space-mode-hook #'modi-conf-quote-normal))

;;----------------------------------------------;;

(use-package conf-mode

  :commands (conf-mode)

  :config

  (when (require 'sboo-text nil :no-error)
    (add-to-list 'conf-space-keywords-alist (cons (rx (? ".") "aspell.conf")
                                                  (sboo-aspell-conf-keywords-regexp))))

  ;; ^ `conf-space-keywords-alist':
  ;;
  ;; • file-name-based ‘conf-space-keywords’.
  ;; • e.g. entry: « '("/mod\\(?:ules\\|probe\\)\\.conf" . "alias\\|in\\(?:clude\\|stall\\)\\|options\\|remove") ».
  ;; 

  (when (require 'sboo-auto-mode nil :no-error)

    (sboo-add-auto-mode-basename ".gitignore"       #'conf-mode)
    (sboo-add-auto-mode-basename ".gitattributes"   #'conf-mode)

    ;; ^ for program `git'.

    (sboo-add-auto-mode-basename "terminalrc"       #'conf-mode)

    ;; ^ for program `xfce4-terminal'.

    (sboo-add-auto-mode-basename ".xbindkeysrc"     #'conf-mode)

    ;; ^ for program `xbindkeys'.

    (sboo-add-auto-mode-file-extension "rc"         #'conf-mode :suffix t)

    ;; ^ `.../xfce4/**.rc' files configure Xfce4 (program `xfce4-*').
    ;;
    ;;   e.g. all « ~/.config/xfce4/panel/*-*.rc » e Config-Files.

    (sboo-add-auto-mode-file-extension "knsrc"      #'conf-mode)

    ;; ^ `.knsrc' files configure KDE (program `kde*').

    (sboo-add-auto-mode-file-extension "service"    #'conf-mode) ; e.g. « /etc/services »
    (sboo-add-auto-mode-file-extension "interfaces" #'conf-mode) ; e.g. « /etc/network/interfaces »

    auto-mode-alist)

  ;; ^ Most `.rc' files are in the INI Format (which `conf-mode' supports).

  (when (require 'sboo-text nil :no-error)
    (dolist (HOOK sboo-conf-hooks)
      (dolist (FUNCTION sboo-conf-functions)
        (add-hook HOOK FUNCTION))))

  ())

;; ^ NOTES
;;
;; • `conf-quote-normal':
;;
;;     • « 0 » — Set the syntax of « ' » and « " » to punctuation.
;;     • « 1 » — Set the syntax of only « ' » to punctuation.
;;     • « 2 » — Set the syntax of only « " » to punctuation.
;;
;; • 

;;TODO: any file that ends in `rc`, should we default to 'conf-mode or to 'sh-mode?
;;
;; (add-to-list 'auto-mode-alist ("rc\\'" . #'conf-mode))
;; (add-to-list 'auto-mode-alist ("rc\\'" . #'sh-mode))

;;==============================================;;

(use-package help

  :bind (:map help-map
              ("C-v" . find-variable)
              ("C-k" . find-function-on-key)
              ("C-f" . find-function)
              ("C-l" . find-library)
         :map help-mode-map
              ("/" . isearch-forward)
              ("g" . sboo-revert-buffer-never-confirm)
         )

  :preface

  (defun sboo-revert-buffer-never-confirm (&optional ignore-auto)
    "`revert-buffer' without asking."
    (interactive (list (not current-prefix-arg)))
    (revert-buffer ignore-auto t nil))

  :config ())

;; describe-*:
;;
;; • `describe-bindings'
;; • `describe-categories'
;; • `describe-char'
;; • `describe-char-eldoc'
;; • `describe-character-set'
;; • `describe-coding-system'
;; • `describe-current-display-table'
;; • `describe-display-table'
;; • `describe-distribution'
;; • `describe-face'
;; • `describe-font'
;; • `describe-fontset'
;; • `describe-function'
;; • `describe-input-method'
;; • `describe-key'
;; • `describe-key-briefly'
;; • `describe-minor-mode'
;; • `describe-mode'
;; • `describe-package'
;; • `describe-symbol'
;; • `describe-syntax'
;; • `describe-text-properties'
;; • `describe-theme'
;; • `describe-variable'
;; • `describe-vector'
;;

;;==============================================;;

(use-package view

  :bind (:map view-mode-map
              ("/" . isearch-forward)
              ("n" . sboo-narrow-dwim) ; “[N]arrow”.
              ("N" . widen)            ; Mnemonic: “N” (upper-case) inverts “n” (lower-case).
         )

  :preface

  (when (fboundp #'defun-dwim)
    (defun-dwim sboo-narrow-dwim narrow-to-region narrow-to-defun))

  :config ())

;; TODO `flycheck' warns “reference to free variable” with custom macro.

;;==============================================;;

(use-package diff-mode

  :commands (diff-mode)

 ;;TODO? :mode (rx "." (or "diff" "patch"))

  :config

  ())

;;==============================================;;

(use-package nroff-mode
  :defer t

  :commands (nroff-mode)

  :config

  ())

;; ^ « nroff » is a format for writing « manpage » files.

;;==============================================;;

(use-package rst

    :mode (("\\.rst$" . rst-mode)
           ("\\.rest$" . rst-mode)
           )

    :config ())

;; ^ Links:
;;
;;   • URL `https://www.emacswiki.org/emacs/reStructuredText'
;;

;;----------------------------------------------;;

(use-package image-file

  :defer 5

  :config

  (auto-image-file-mode +1)

  (add-hook 'image-mode-hook #'image-transform-reset)
  (add-hook 'image-mode-hook #'auto-revert-mode)

  ;; ^ Images are Read-Only, hence Auto-Revert them.

 ())

;; ^ Links:
;;
;;   • URL `http://ergoemacs.org/emacs/emacs_view_images.html'
;;

;;----------------------------------------------;;
;;; Builtin Packages: Prog Modes ---------------;;
;;----------------------------------------------;;

(use-package prog-mode

    :commands (prettify-symbols-mode)

    :custom

    (prettify-symbols-unprettify-at-point 'right-edge "“Unprettify” a symbol if ‘point’ is ① at it or ② just after it.")

    :config

    (when (require 'sboo-prog nil :no-error)

      (dolist (HOOK sboo-prog-mode-hooks)
        (add-hook 'prog-mode-hook HOOK))

      ())

    ())

;;==============================================;;

(when (require 'sboo-lisp nil :no-error)

  (dolist (MODE sboo-lisp-modes)
    (font-lock-add-keywords MODE sboo-lisp-keywords))

  ())

;;----------------------------------------------;;

(use-package lisp-mode

    :commands (check-parens)

    :config

    (dolist (MODE '(lisp-mode
                    emacs-lisp-mode
                    inferior-emacs-lisp-mode
                    inferior-lisp-mode
                    lisp-interaction-mode))

      (font-lock-add-keywords MODE
                              '(("(\\(lambda\\)\\>"
                                 (0 (ignore
                                     (compose-region (match-beginning 1) (match-end 1) ?λ))))
                                )))

    ())

;; M-: (setq emacs-lisp-mode-hook '(highlight-cl-add-font-lock-keywords sboo-company-elisp-setup eldoc-mode superword-mode sboo-set-run-key-to-eval-buffer))

;;----------------------------------------------;;

(use-package elisp-mode

  :delight (emacs-lisp-mode "Elisp")

  ;; ^ Shorten « Emacs-Lisp » to « elisp ».
  ;;
  ;; NOTE the `major-mode', unlike any `minor-mode's,
  ;;      starts the Modeline, and thus shouldn't have leading whitespace.
  ;;

  :hook ((emacs-lisp-mode . superword-mode)
         )

  :config

  (when (require 'sboo-prog nil :no-error)
    ())

  ())

;;----------------------------------------------;;

(use-package sh-script

    ;;--------------------------;;

    :hook (sh-mode . flycheck-mode)

    ;; ^ ShellCheck (a Bash Linter) is a builtin FlyCheck checker.

    :config ())

;;==============================================;;

(use-package checkdoc

  :custom

  (checkdoc-arguments-in-order-flag t "ensure that the order of arguments coincides between (1) a function's Argument List ans (2) its docstring's Header Line.")

  :init

  (setq checkdoc-spellcheck-documentation-flag t)

  :config ())

;;----------------------------------------------;;

(use-package elint

  :commands (elint-initialize elint-current-buffer)

  ;; mnemonic:
  ;; • "s-m" — personal keymap for MODE-specific stuff.
  ;; • "l" — LINTING.

  :bind (("s-q l" . sboo-elint-buffer)
         )

  :preface

  (defun sboo-elint-buffer ()
    "`elint-current-buffer'"
    (interactive)
    (when (derived-mode-p 'emacs-lisp-mode major-mode)
      (elint-initialize)
      (elint-current-buffer)))
                
  :config       

  (dolist (SYMBOL '(current-prefix-arg
                    command-line-args-left
                    buffer-file-coding-system
                    emacs-major-version window-system
                    ))
    (add-to-list 'elint-standard-variables SYMBOL))

  ())

;; ^ Links:
;;
;;   • Info Node `(emacs) Elint'
;;

;;==============================================;;
;; Builtin Packages: Editing:
;;----------------------------------------------;;

(use-package kmacro

    :commands (kmacro-start-macro-or-insert-counter  ; <f3>
               kmacro-end-or-call-macro              ; <f4>
               kmacro-name-last-macro                ; C-x C-k n
               )

    :config
    
    (when (require 'sboo-kmacro nil :no-error)

      ())

    ())

;;----------------------------------------------;;

(use-package macros

    :commands (apply-macro-to-region-lines insert-kbd-macro)

    :config
    
    (when (require 'sboo-kmacro nil :no-error)

      ())

    ())

;;----------------------------------------------;;

(use-package align

  :commands (align align-regexp)

  ;;--------------------------;;

  ;; mnemonic:
  ;; • "s-e" — personal keymap for EDITING stuff.
  ;; • "[" — the Open-Square-Bracket looks vertical, which implies Vertical-Alignment (?)

  :bind (:map sboo-edit-keymap
              ("[ c" . sboo-align-code)
              ("[ r" . align-regexp)
              )

  ;;--------------------------;;

  :config

  (when (require 'sboo-align nil :no-error)
    (sboo-align/setup))

  ())

;;==============================================;;
;;; Builtin Packages: Viewing:
;;----------------------------------------------;;

(use-package hl-line

  :commands (hl-line-mode)

  :hook (prog-mode . hl-line-mode)

  :config

  ())

;; "hl-line" abbreviates "[H]igh[L]ight [LINE]".

;;==============================================;;

(use-package hi-lock

  :commands (highlight-regexp highlight-phrase highlight-lines-matching-regexp)

  :bind (("C-@ r" . highlight-regexp)
         ("C-@ p" . highlight-phrase)
         ("C-@ l" . highlight-lines-matching-regexp)
         )

  :preface (unbind-key "C-@")

  :config ())

;; ^ "hi-lock" abbreviates "[HI]ghlight [LOCK

;;==============================================;;
;;; Builtin Packages: Searching:
;;----------------------------------------------;;

(use-package isearch
  :no-require t

  :bind (:map isearch-mode-map
              ("C-c" . isearch-toggle-case-fold)
              ("C-t" . isearch-toggle-regexp)
              ("C-^" . isearch-edit-string)
              ("C-i" . isearch-complete)
        )

  :config

  ())

;;----------------------------------------------;;

(use-package grep

  :commands (grep
             grep-find
             find-grep-dired
             find-name-dired
             )

  ;;--------------------------;;

  ;; mnemonic:
  ;; • "s-r" — personal keymap for RUNNING stuff.
  ;; • "g"   — [G]rep.
  ;; • "g g" — [G]rep (the doubled character meaning the default).

  :bind (("s-r g f" . find-grep)
         ("s-r g d" . find-grep-dired)
         ("s-r g n" . find-name-dired)
         ("s-r g g" . grep))

  ;;--------------------------;;

  :hook (grep-mode . sboo-grep-config)

  ;;--------------------------;;

  :preface

  (defun sboo-grep-config ()

    "Hook for `grep-mode'."

    (toggle-truncate-lines +1))

  ;;--------------------------;;

  :config

  (setq grep-files-aliases (append '(("hs" . "*.hs")
                                ("md" . "*.md")
                                )
                              grep-files-aliases))

  (setq grep-find-ignored-directories (append '("tmp" "old" "stdout"
                                          "dist" "dist-newstyle" "dist-dante" ".stack-work"
                                          "elpa" ".cask"
                                          "node_modules" ".bundle"
                                          )
                                         grep-find-ignored-directories))
  ;;--------------------------;;

  ())

;; ^ NOTES:
;;
;;  • `grep-files-aliases', by default, holds:
;;
;;        (("all" . "* .[!.]* ..?*")
;;         ("el"  . "*.el")
;;         ("ch"  . "*.[ch]")
;;         ...)
;;
;;

;;==============================================;;

(when (require 'sboo-compilation nil :no-error)

  (sboo-compilation-init!)

  (add-startup-hook! #'sboo-compilation-config!))

;;----------------------------------------------;;

(when (require 'sboo-make nil :no-error)
  (add-hook 'makefile-mode-hook #'sboo-show-trailing-whitespace))

;;----------------------------------------------;;

;;(add-hook 'text-mode-hook #'sboo-set-input-method-TeX)

;; (when (require 'sboo-text-mode nil :no-error)
;;   (dolist (HOOK sboo-text-mode-hooks)
;;     (add-hook 'text-mode-hook HOOK)))

;;==============================================;;
;;; Builtin Packages: Buffers
;;----------------------------------------------;;

(use-package autorevert

  :commands (auto-revert-mode)

  :delight (auto-revert-mode " 🗘")
  ;; ^ Shorten `auto-revert-modee'.

  :config

  ())

;;==============================================;;

(use-package uniquify

  :custom

  (uniquify-buffer-name-style 'forward
                              "distinguish Synonyms Buffers (when two buffers are open with the same name, this makes it easier to tell them apart).")

  :config ())

;;==============================================;;
;;; Builtin Packages: Windows
;;----------------------------------------------;;

;;==============================================;;
;;; Builtin Packages: Filesystem
;;----------------------------------------------;;

(use-package dired

  ;;---------------------------;;

  :bind (:map dired-mode-map
              ("/" . dired-isearch-filenames-regexp) ; Mnemonic: like « $ less ».
              )

  ;;---------------------------;;

  :custom

  (dired-recursive-deletes 'top "`top' means: ask for each directory at the TOP level, but delete subdirectories without asking.")

  (dired-auto-revert-buffer t)

  ;; ^ revert Dired buffers automatically.

  (dired-listing-switches "-l --recursive --almost-all --ignore-backups --human-readable --group-directories-first")

  ;; ^ Program ‘ls’ options (must include ‘-l’).
  ;;
  ;; * `--recursive': list subdirectories recursively.
  ;; * `--almost-all': omit « ./ » and « ../ »
  ;; * `--ignore-backups': omit « ~ »-suffixed files.
  ;; * `--human-readable': print sizes like 1K, 234M, 2G, etc (needs `-l' and `-s').
  ;;

  :config

  (setq-default dired-dwim-target t)
  
  ;; ^ if `dired-dwim-target' is non-nil,
  ;;   `dired-mode' guesses targets for commands like copy and move (guesses the default, but the user still confirms).
  ;;
  ;; e.g. if you have your Frame split into two Dired Windows,
  ;;      then Dired will assume that you want to copy/move the file from the one into the other.
  ;;
  ;; 

  (define-key dired-mode-map [mouse-2] #'dired-find-file)

  ())

;; ^ "Dired" abbreviates "[DIR]ectory [ED]itor".

;; ^ Dired Keybindings include:
;;
;; « ( » binds `dired-hide-details-mode'
;;

;; ^ Links
;;
;; • URL `http://ergoemacs.org/emacs/emacs_dired_tips.html' 

;;----------------------------------------------;;

(use-package wdired

  :commands (wdired-change-to-wdired-mode)

  ;;---------------------------;;

  :bind (:map dired-mode-map
              ("w" . wdired-change-to-wdired-mode)   ; Mnemonic: [W]dired.
              )

  ;; ^ Shadows `dired-copy-filename-as-kill':
  ;;
  ;; (define-key map "w" 'dired-copy-filename-as-kill)

  ;;---------------------------;;

  :custom

  (wdired-allow-to-change-permissions t "edit Permission-Bits directly (`wdired' ensures you can only enter valid ones), by pressing « w » or « x » or « r ».")

  (wdired-allow-to-redirect-links t "edit Symbolic Links (adding or removing), by pressing « s » or deleting it.")

  (wdired-use-dired-vertical-movement 'sometimes
                                      "`sometimes' means — upon any Vertical Movement, emacs will move `point' to the Beginning of a Filename (if `point' is to the left of it).")

  ;;---------------------------;;

  :config

  ())

;; ^ "WDired" abbreviates "[W]riteable [DIR]ectory [ED]itor".

;; ^ Links
;;
;; • URL `https://www.masteringemacs.org/article/wdired-editable-dired-buffers' 

;;----------------------------------------------;;

(use-package find-dired

    :commands (find-dired
               find-grep-dired
               find-name-dired)

    :custom

    (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld") "“By default Emacs will pass -exec to find and that makes it very slow. It is better to collate the matches and then use xargs to run the command.”")

    ;; :custom (find-dired- "" "only Files.")

    :config ())

;; ^ Links
;;
;; • URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-and-Find.html'
;; • URL `https://www.masteringemacs.org/article/working-multiple-files-dired'
;;

;;----------------------------------------------;;

(use-package image-dired

  :commands (image-dired)

  :config ())

;;==============================================;;
;;; Feature: Completion:
;;----------------------------------------------;;

(use-package minibuffer

  :config

  (add-to-list 'completion-styles 'substring nil)

  (dolist (COMPLETER '(elisp-completion-at-point comint-dynamic-complete-filename))
    (add-to-list 'completion-at-point-functions COMPLETER nil))

  ())

;; ^ Notes:
;;
;; • « C-M-i » binds `complete-symbol', which tries
;;   `completion-at-point-functions'.
;;   « C-M-i » is the same as « M-C-i », which under « emacs -nw » is
;;   the same as « M-<tab> ».
;;

;;==============================================;;

(use-package dabbrev

    :delight (abbrev-mode " 👆")

    ;;----------------------------;;

    :custom

    (abbrev-file-name sboo-abbrev-file "Personal `dabbrev' config.") ; Default: « "~/.emacs.d/abbrev_defs" ».

    ;;----------------------------;;

    :preface

    (defvar sboo-abbrev-file

      (condition-case _
          (sboo-file "dabbrev/abbrev_defs.el")
        ((void-function void-variable)
         "~/.emacs.d/sboo/dabbrev/abbrev_defs.el"))

      "Personal (version-controlled) `abbrev-file-name'.")

    ;;----------------------------;;

    :init

    (let* ((DIRECTORY (file-name-directory sboo-abbrev-file))
           )
      (when (not (file-directory-p DIRECTORY))
        (make-directory DIRECTORY :make-parent-directories)))

    ;;----------------------------;;

    :config

    (add-hook 'text-mode-hook #'abbrev-mode)

    (when (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

    ())

;; ^ "`dabbrev'" abbreviates "Dynamic ABBREViation".

;; ^ Links:
;;
;;   • URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Abbrev-Files.html'
;;   • URL `http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html'
;;   • URL `https://www.emacswiki.org/emacs/AbbrevMode'
;;

;;==============================================;;
;;; Builtin Packages: Shells / Terminals:
;;----------------------------------------------;;

(use-package comint

  ;;--------------------------;;

  :bind (:map comint-mode-map
              ("<up>"   . comint-previous-matching-input-from-input)
              ("<down>" . 'comint-next-matching-input-from-input)
              )

  ;;--------------------------;;

  :hook (comint-mode . goto-address-mode)

  ;;--------------------------;;

  :custom

  (comint-scroll-to-bottom-on-output 'others "‘others’ means — “move ‘point’ down to track STDOUT only in ‘other-window’s (not in the ‘selected-window’).”")
  (comint-scroll-to-bottom-on-input  'this   "‘this’ means — “move ‘point’ down if you type into the ‘selected-window’.”")

  (comint-buffer-maximum-size 65536 "increase to « 2^16.")

  ;;--------------------------;;

  :config

  (setq comint-scroll-to-bottom-on-input t)

  ;; ^ non-`nil' = insertion and yank commands scroll the selected window to the bottom before inserting.

  (setq comint-input-ignoredups t)

  ;; ^ whether successive identical inputs are stored in the input history.

  (setq comint-completion-addsuffix t) 

  ;; ^ whether completion inserts a space or a slash after a fully-completed file or directory (respectively).

  (when (require 'sboo-shell nil :no-error)

    (add-hook 'comint-exec-hook #'sboo-process-disable-query-on-exit)
    ;; ^ Quit Comint buffers without confirmation.

    ())

  ())

;; ^ NOTES
;;
;;   • `comint' is a Non-Package (?) Feature.
;;

;;----------------------------------------------;;

(use-package shell

    ;;--------------------------;;

    :bind

  ( ("<s>-s" . shell)
    
    :map shell-mode-map

    ("<kp-prior>" . comint-previous-input)
    ;; ^ <prior> is the page-up key.
    ;; `comint-previous-input` is like "press down in a terminal-emulator".
    
    ("<kp-next>" . 'comint-next-input)
    ;; ^ <next> is the page-down key.
    ;; `comint-next-input` is like "press up in a terminal-emulator".

    )

  ;;--------------------------;;

  :custom

  (explicit-shell-file-name "/bin/bash")

  ;;--------------------------;;

  :config

  (add-hook 'shell-mode-hook #'dirtrack-mode)

  ;; ^ `dirtrack' tracks the working directory by parsing the command prompt.

  (setq shell-completion-fignore '("~" "#" "%"))

  ;; ^ filename extensions to ignore during completion.

  (push (cons (rx "*shell*") display-buffer--same-window-action)
        display-buffer-alist)

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

  (when (require 'sboo-shell nil :no-error)

    (dolist (HOOK sboo-shell-hooks-list)
      (add-hook 'shell-mode-hook HOOK))

    ())

  ())

;;----------------------------------------------;;

(use-package term

    :commands (ansi-term)

    ;;--------------------------;;

    :bind (:map term-mode-map             ; `term-line-mode':
                ("C-j" . term-char-mode)
                ("C-v" . nil)
           :map term-raw-map              ; `term-char-mode':
                ("C-j" . term-line-mode)
                ("C-v" . nil)
           )

    ;;--------------------------;;

    :hook (term-mode . goto-address-mode)

    ;;--------------------------;;

    :preface

    (defun sboo-ansi-term ()
      "Call `ansi-term' with program `bash'."
      (interactive)
      (ansi-term "/bin/bash"))

    ;;--------------------------;;

    :config

    (push (cons (rx bos "*" (or "ansi-term" "terminal") "*" eos) display-buffer--same-window-action)
          display-buffer-alist)

    ;; (defadvice term-char-mode (after term-char-mode-fixes ())
    ;;   (set (make-local-variable 'cua-mode) nil)
    ;;   ;; ^ Disable `cua-mode' to enable `?\C-x' for escaping.
    ;;   (set (make-local-variable 'transient-mark-mode) nil)
    ;;   (set (make-local-variable 'global-hl-line-mode) nil)
    ;;   (ad-activate 'term-char-mode)
    ;;   (term-set-escape-char ?\C-x))

    (when (require 'sboo-shell nil :no-error)

      (dolist (HOOK sboo-term-hooks-list)
        (add-hook 'term-mode-hook HOOK))

      (add-hook 'term-exec-hook #'sboo-process-disable-query-on-exit)

      ;; ^ Quit Comint buffers without confirmation.

      ())

    ())

;; ^ « (kbd "<S-insert>") » pastes into Terminal-Emulators (like `ansi-term').
;;   TODO `key-translation-map'? `raw-mode'?

;; ^ Switching between Input Modes.
;;
;; • URL `https://stackoverflow.com/questions/14485858/multi-term-understanding-keyboard-bindings/14492124'
;; • URL `https://stackoverflow.com/questions/14484454/running-emacs-commands-from-ansi-term-in-character-mode/14491568'

;; ^ Links:
;;
;;   • URL `https://oremacs.com/2015/01/01/three-ansi-term-tips/'
;;

;;----------------------------------------------;;

(use-package ansi-color

  ;;--------------------------;;

  :config

  (defun sboo-ansi-colors-apply ()

    "`ansi-color-apply-on-region' on whole buffer."

    (interactive)

    (ansi-color-apply-on-region (point-min) (point-max)))

  ;;--------------------------;;

  ())

;;----------------------------------------------;;

(use-package dirtrack

  ;;--------------------------;;

  :config

  (when (require 'sboo-shell nil :no-error)

    (setq dirtrack-list (list sboo-prompt-regexp 0))

    ())

  ())

;;----------------------------------------------;;

(use-package eshell

  :commands (eshell eshell-command)

  :preface

  (defvar eshell-isearch-map
    (let ((KEYMAP (copy-keymap isearch-mode-map)))
      (define-key KEYMAP [(control ?m)] #'eshell-isearch-return)
      (define-key KEYMAP [return]       #'eshell-isearch-return)
      (define-key KEYMAP [(control ?r)] #'eshell-isearch-repeat-backward)
      (define-key KEYMAP [(control ?s)] #'eshell-isearch-repeat-forward)
      (define-key KEYMAP [(control ?g)] #'eshell-isearch-abort)
      (define-key KEYMAP [backspace]    #'eshell-isearch-delete-char)
      (define-key KEYMAP [delete]       #'eshell-isearch-delete-char)
      KEYMAP)
    "Keymap for `isearch' in Eshell.")

  :config

  ())

;;==============================================;;
;;; Builtin Packages: Recent Files/Places:
;;----------------------------------------------;;

(defvar sboo-recentf-file

  (condition-case _
      (sboo-xdg-data "emacs/recentf/recentf.el")
    ((void-function void-variable)
     "~/.local/share/emacs/recentf/recentf.el"))

  "XDG-conformant `recentf-save-file'.

Notes:

• `recentf-save-file' persists `recentf-list'.")

;;----------------------------;;

(use-package recentf

  :commands (recentf-mode recentf-open-files)

  :delight (recentf-mode " ⏲")

  :init

  (let* ((DIRECTORY (file-name-directory sboo-recentf-file))
         )
    (when (not (file-directory-p DIRECTORY))
      (make-directory DIRECTORY :make-parent-directories)))

  :custom

  (recentf-save-file sboo-recentf-file "XDG-conformant `recentf' data.") ; Default: « "~/.emacs.d/recentf" ».

  (recentf-max-saved-items 1024 "Remember more files.")
  (recentf-max-menu-items  15  "Display more files.")

  :config

  (recentf-mode +1))

;; ^ "`recentf'" abbreviates "[RECENT] [F]iles".

;; ^ Links:
;;
;;   • URL `https://www.masteringemacs.org/article/find-files-faster-recent-files-package'
;;   • URL `'
;;

;;==============================================;;

(use-package saveplace

  :commands (save-place-mode)

;;:custom

  :config

  (save-place-mode +1))

;; ^ `saveplace'
;;
;; • remembers your Last Position for Re-Opened Files.
;;

;; ^ Save point position between sessions.
;;
;; "Save the position I was in each file, i.e. no scrolling down to paragraph N or function foo when I reopen my files."
;;

;TODO; (setq save-place-file (expand-file-name "save-point-places" user-emacs-directory))

;;----------------------------------------------;;

(when (require 'savehist nil :no-error)

  (setq savehist-additional-variables '(search-ring regexp-search-ring))

  (setq savehist-file (emacs-file "savehist.el"))

  (savehist-mode t)

  ())

;; ^ Save History.
;;
;; Save mode-line history between sessions.
;;

;;==============================================;;
;;; Builtin Packages: Bookmarks:
;;----------------------------------------------;;

(when (require 'sboo-bookmark nil :no-error)

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
;;; Builtin Packages: Web:
;;----------------------------------------------;;

(use-package browse-url

    :commands (browse-url)

    :custom

    (browse-url-browser-function #'browse-url-chrome)

    :config ())

;; ^ `browse-url-browser-function':
;;
;; • ‘eww-browse-url’        — via ‘eww’.
;; • ‘browse-url-firefox’    — via "Mozilla Firefox".
;; • ‘browse-url-chrome’     — via "Google Chrome".
;; • ‘browse-url-text-emacs’ — via a Text Browser in an Emacs Window.
;;

;;----------------------------------------------;;

(use-package eww

  :commands (eww eww-open-file eww-browse-with-external-browser)

  :bind (:map sboo-launch-keymap
              ("w" . eww)
              )

  :custom

  (url-configuration-directory (sboo-xdg-data "eww" :subdir "emacs") "XDG-conformant")

  :config ())

;; ^ `eww' is the Emacs Web Browser.
;;
;; "eww" abbreviates "[E]macs [W]eb Bro[W]ser".
;;
;; Keybindings include:
;;
;; • 【&】 `eww-browse-with-external-browser'
;; • 【R】 `eww-readable'
;; • 【d】 `eww-download'
;; • 【g】 `eww-reload'
;; • 【p】 `eww-previous-url'
;; • 【q】 `eww-quit'
;; • 【r】 `eww-forward-url'
;; • 【w】 `eww-copy-page-url'
;;

;; ^ Links:
;;
;;   • URL `'
;;   • URL `http://ergoemacs.org/emacs/emacs_eww_web_browser.html'
;;

;;==============================================;;
;;; Feature: Spell-Checking:
;;----------------------------------------------;;

(defvar sboo-spelling-aspell-p

  (if (or (executable-find "aspell")
          (getenv "SBOO_EMACS_ASPELL"))
      t
    nil)

  "Personal spell-checker.

i.e. use `aspell' over `ispell'.")

;;----------------------------------------------;;

(defvar sboo-spelling-personal-dictionary

  (if sboo-spelling-aspell-p
      "~/configuration/data/aspell/en.pws"
    "~/configuration/data/ispell/")

  "Personal dictionary for spell-checkers.")

;;==============================================;;

(defun sboo-flyspell-check-next-word ()

  "Spell-Check the next misspelled word."

  (interactive)

  (flyspell-goto-next-error)
  (ispell-word))

;;----------------------------------------------;;

;; (defun sboo-flyspell-check-prior-word ()

;;   "Spell-Check the previous misspelled word."

;;   (interactive)

;;   (flyspell-goto-previous-error)
;;   (ispell-word))

;;----------------------------------------------;;

(defun sboo-flyspell-check-first-word ()

  "Spell-Check the previous misspelled word."

  (interactive)

  (beginning-of-buffer)
  (flyspell-goto-next-error)
  (ispell-word))

;;----------------------------------------------;;

(defun sboo-flyspell-buffer-after-pdict-save (&rest _)

  "Restart `flyspell-buffer' after extending personal dictionary.

Motivation:

>If I add a word during a flyspell session, it’s still marked up as
>misspelled. And flyspell-correct-previous-word tells me that it’s
>spelling is correct. How do I run ‘flyspell-buffer’ on the buffer every
>time the dictionary is modified?

Links:

• URL `https://www.emacswiki.org/emacs/FlySpell'
• URL `https://www.reddit.com/r/emacs/comments/4oc7pg/spellcheck_flyspellmode_underlines_disappear_when/'"

  (interactive)

  (flyspell-buffer))

;;==============================================;;

(use-package ispell

  :commands (ispell-word ispell-region ispell-buffer)

  :custom

  (ispell-program-name "aspell" "« aspell »")
  (ispell-really-aspell t       "« aspell »")

  (ispell-list-command "--list"
                       "Because the “-l” option, which means “--list” in program `ispell', means “--lang” in program `aspell'.")

  (ispell-silently-savep t "don't ask")

  (ispell-personal-dictionary sboo-spelling-personal-dictionary
                              "XDG-conformant (defaults to « ~/.aspell.en.pws »).")

;;:custom-face (TODO '(:color "red" :style wave))

  :config

  (setq-default ispell-program-name "aspell")

  (add-to-list 'ispell-skip-region-alist '("^```" . "^```"))
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

  ;; ^ don't spell-check code-blocks,
  ;;   in `markdown-mode' and `org-mode'.

  (let* ((DIRECTORY (file-name-directory sboo-spelling-personal-dictionary))
         )
    (when (not (file-directory-p DIRECTORY))
      (make-directory DIRECTORY :make-parent-directories)))

  ;; ^ « mkdir -p ».

  ())

;;----------------------------------------------;;

(use-package flyspell

  :delight (flyspell-mode " 🔤")

  ;;--------------------------;;

  :commands (flyspell-mode flyspell-prog-mode flyspell-auto-correct-word flyspell-goto-next-error flyspell-check-next-highlighted-word)

  ;;--------------------------;;

  :bind (("M-<f9>"    . sboo-flyspell-prior-error)
         ("C-<f9>"    . sboo-flyspell-next-error))

  :bind (:map text-mode-map
              ("<kp-up>"   . sboo-flyspell-check-first-word)
              ("<kp-down>" . sboo-flyspell-check-next-word))

  ;;--------------------------;;

  ;; :hook ((text-mode     . flyspell-mode)
  ;;        (markdown-mode . flyspell-mode)
  ;;        )

  ;;--------------------------;;

  :preface

  (defun sboo-turn-on-flyspell-mode ()
    "Turn on either ‘flyspell-mode’ or ‘flyspell-prog-mode’."
    (interactive)
    (unless flyspell-mode
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (turn-on-flyspell)))
    )

  (defun sboo-flyspell-prior-error ()
    "Launch Flyspell and goto the prior Spelling Error."
    (interactive)
    (sboo-turn-on-flyspell-mode)
    ;; (flyspell-buffer)
    (flyspell-goto-previous-error))

  (defun sboo-flyspell-next-error ()
    "Launch Flyspell and goto the next Spelling Error."
    (interactive)
    (sboo-turn-on-flyspell-mode)
    ;; (flyspell-buffer)
    (flyspell-goto-next-error))

  ;;--------------------------;;

  :config

  (advice-add 'ispell-pdict-save :after #'sboo-flyspell-buffer-after-pdict-save)

  ())

;; ^ NOTES
;;
;;   • `flyspell-prog-mode' spell-checks comments.
;;

;; ^ Links:
;;
;;   • URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html'
;;   • URL `https://www.emacswiki.org/emacs/FlySpell'
;;   • URL `https://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary'
;;

;;----------------------------------------------;;
;;; Builtin Packages: Help:
;;----------------------------------------------;;

(use-package re-builder

  :commands (re-builder)

  :bind (:map reb-mode-map 
              ("C-c C-k" . reb-quit)
              ;; ^ more idiomatic quit binding.
              )

;;:custom (reb-re-syntax 'rx) (reb-re-syntax 'string)

  :config ())

;; ^ Links:
;;
;;   • URL `https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder'
;;

;;==============================================;;
;;; Builtin Packages: Miscellanea:
;;----------------------------------------------;;

(use-package man

    :commands (man)

    :config

    (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face    :bold      t)
    (set-face-attribute 'Man-underline  nil :inherit font-lock-keyword-face :underline t)

    ())

;; (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "orange red")
;; (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "forest green")

;; Or to be theme agnostic:

;; (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
;; (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

;;----------------------------------------------;;

(use-package ediff

  :commands (ediff-buffers ediff-current-file)

  ;; mnemonic:
  ;; • "s-r" — personal keymap for RUNNING stuff.
  ;; • "=" — “whether two things are EQUAL.”

  :bind (("s-r = B" . ediff-buffers3)
         ("s-r = F" . ediff-files3)
         ("s-r = P" . ediff-patch-buffer)
         ("s-r = b" . ediff-buffers)
         ("s-r = c" . compare-windows)
         ("s-r = f" . ediff-files)
         ("s-r = l" . ediff-regions-linewise)
         ("s-r = m" . count-matches)
         ("s-r = p" . ediff-patch-file)
         ("s-r = r" . ediff-revision)
         ("s-r = w" . ediff-regions-wordwise)
         ("s-r = =" . ediff-files))

  :custom

  (ediff-window-setup-function 'ediff-setup-windows-plain "“Plain” means “no multiframe ediff”.")

  :config

  ())

;;----------------------------------------------;;

(use-package eldoc

  :commands (eldoc-mode)

  :delight (eldoc-mode " 👇")

  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode)

;;:custom

  :config

  ())

;;----------------------------------------------;;

(use-package sql

  :commands (sql-postgres)

  :config

  (when (require 'sboo-sql nil :no-error)
    ())

  ())

;;----------------------------------------------;;

(use-package vc
  :defer t

  :custom

  (vc-follow-symlinks t "don't ask when visiting a symbolic link to a version-controlled file (but do warn in the echo area).")

  :config

  ())

;;----------------------------------------------;;

(use-package outline
  :defer t

  :config ())

;;----------------------------------------------;;

(use-package calendar
  :defer t

  :commands (calendar)

  :custom

  (calendar-week-start-day 0 "“0 means Sunday, 1 means Monday, etc”")

  :config

  ())

;;----------------------------------------------;;
;;; Builtin Packages: Utilities:
;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s"

  (when (require 'sboo-unicode nil :no-error)

    ;;(add-startup-hook! #'sboo-unicode-init)

    (sboo-unicode-init)))

;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s"

  (when (require 'sboo-comment nil :no-error)

    ()))

;;----------------------------------------------;;
;;; Builtin Effects:
;;----------------------------------------------;;

(progn

  (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

  ())

;;; EXTERNAL PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;----------------------------------------------;;
;;; External Packages: Installation ------------;;
;;----------------------------------------------;;

(pcase (sboo-get-installation-method)

  ('submodules (progn
                 (sboo-register-submodule-packages! "use-package/")
                 (sboo-register-submodule-packages! "helm/")

                 (when (< emacs-major-version 26)
                   (sboo-register-submodule-packages! "real-auto-save/"))))

  ;;--------------------------;;

  ('melpa      (progn
                 (sboo-load-file! "sboo-packages-by-installing.el")

                 (when (require 'sboo-packages nil :no-error)

                   (dolist (ARCHIVE sboo-package-archives)
                     (add-to-list 'package-archives ARCHIVE :append))

                   (when (>= emacs-major-version 26)
                     (async-start #'package-refresh-contents))
                   (setq package-load-list sboo-all-packages)

                   ;;(package-initialize)
                   ;;NOTE we call `package-initialize' in `init.el'.

                   ())))
  ;;--------------------------;;

  ('nixpkgs    (progn))

  (_           (progn)))

;; ^ NOTES `async-start':
;;
;;     (async-start START-FUNC &optional FINISH-FUNC)
;;
;; Execute START-FUNC (often a lambda) in a subordinate Emacs process.
;; When done, the return value is passed to FINISH-FUNC.  Example:
;;

;;----------------------------------------------;;
;;; External Packages: Prioritized Packages ----;;
;;----------------------------------------------;;

;;(sboo-load-file! "sboo-init-use-package.el")

;; ^ Links:
;;
;;   • URL `https://github.com/jwiegley/use-package'
;;

;;----------------------------------------------;;

;;(sboo-load-file! "sboo-init-helm.el")

;;----------------------------------------------;;
;; External Packages: Libraries ----------------;;
;;----------------------------------------------;;

(require 'dash nil :no-error)

;; ^ URL `https://github.com/magnars/dash.el'

;;----------------------------------------------;;

(require 's nil :no-error)

;; ^ URL `https://github.com/magnars/s.el'

;;----------------------------------------------;;

(require 'f nil :no-error)

;; ^ URL `https://github.com/rejeep/f.el'

;;----------------------------------------------;;

(require 'ht nil :no-error)

;; ^ URL `https://github.com/Wilfred/ht.el'

;;----------------------------------------------;;

(require 'ov nil :no-error)

;; ^ URL `https://github.com/ShingoFukuyama/ov.el'

;;----------------------------------------------;;

(require 'popup nil :no-error)

;; ^ URL `https://github.com/auto-complete/popup-el'

;;----------------------------------------------;;

(require 'list-utils nil :no-error)

;; ^ URL `https://github.com/rolandwalker/list-utils'

;;----------------------------------------------;;

(require 'loop nil :no-error)

;; ^ URL `https://github.com/Wilfred/loop.el'

;;----------------------------------------------;;

(require 'memoize nil :no-error)

;; ^ URL `https://github.com/skeeto/emacs-memoize'

;;----------------------------------------------;;

(require 'html-to-markdown nil :no-error)

;; ^ `html-to-markdown'…
;;
;; Provides:
;;
;; • `html-to-markdown'
;; • `html-to-markdown-string'
;;
;; URL `https://github.com/Malabarba/html-to-markdown#html-to-markdown-converter-written-in-emacs-lisp'
;;

;;----------------------------------------------;;
;;; External Packages: Completion (Helm) -------;;
;;----------------------------------------------;;

;; Primary Helm Packages...

;;----------------------------------------------;;

(use-package helm-config
    :demand t

    :load-path "vendor/helm"
    ;; :load-path ("submodules/helm" "submodules/async")

    ;;------------------------;;

    :custom

    (helm-command-prefix-key "<f9>"
      "the Default (« C-x c ») is too similar to `kill-emacs's keybinding.")

    ;; ^  NOTE `helm-command-prefix-key' becomes immutable once `helm-config' is `load'ed.

    ;;   :bind (:map helm-map
    ;;               ("<tab>" . helm-execute-persistent-action)
    ;;               ("C-i"   . helm-execute-persistent-action)
    ;;               ("C-z"   . helm-select-action)
    ;;               ("A-v"   . helm-previous-page))

    :config

    ;; Remap keybindings:

    (define-key global-map [remap execute-extended-command] #'helm-M-x)
    (define-key global-map [remap list-buffers]             #'helm-buffers-list)
    (define-key global-map [remap find-file]                #'helm-find-files) ; Includes the « `<tool-bar>' `<new-file>' ».
    (define-key global-map [remap find-file-existing]       #'helm-find-files) ; Includes the « `<tool-bar>' `<open-file>' »?
    (define-key global-map [remap occur]                    #'helm-occur)

    (define-key global-map [remap menu-find-file-existing]  #'helm-find-files) ; The `toolbar's `<open-file>'.

    ())

;;----------------------------------------------;;

(use-package helm
    :load-path "vendor/helm"

    :after helm-config

    ;;------------------------;;

  :delight (helm-mode " ⎈")
  :custom

  (helm-allow-mouse t "Enable mouse (doesn't enable selection-by-clicking, only marking-by-clicking).")

  ;; ^ `helm-allow-mouse'. the mouse is gratuitously disabled by default.
  ;;   this enables, for example, clicking on a helm candidate to activate it,
  ;;   rather than navigating it with several arrow and/or character keypresses.

  ;; ^ `helm-boring-buffer-regexp-list'. by default, it's:
  ;; 
  ;;     '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")
  ;;

  (helm-mode-fuzzy-match                 t " ")
  (helm-completion-in-region-fuzzy-match t " ")

  (helm-register-max-offset              10000 "Increase (Helm's) Maximum Clipboard Size.")

  (helm-split-window-in-side-p           t   "open the Helm Buffer inside the currently-`selected-window' (don't occupy the whole `other-window').")
  (helm-move-to-line-cycle-in-source     nil "Don't cycle (i.e. move to the end or to the beginning of the prior or of the next source) when reaching top-or-bottom of a source.")
  (helm-scroll-amount                    8   "Scroll 8 lines (« M-<next> » / « M-<prior> » )")

  (helm-echo-input-in-header-line        t " ")
  (helm-full-frame                       t " ")

  (helm-autoresize-min-height 20 "Minimum Height (in lines?) for Helm Windows.")
  (helm-autoresize-max-height 60 "Maximum Height (in lines?) for Helm Windows.")

  :config

  ;; Helm keybindings:

  (define-key helm-map (kbd "<f12>")         #'helm-select-action)
  (define-key helm-map sboo-key/keyboard-quit #'keyboard-quit) ; (if Overrides the “Run Ninth Helm Action” command.

  ;; ^ Mnemonic: 
  ;;
  ;; • Global ‹f12› executes an Emacs Command.
  ;; • Helm-Local ‹f12› executes a Helm Action
  ;;   (i.e. the subset of Emacs Commands which can work upon the selection(s)).
  ;; 

  ;; Helm config:

  (when (executable-find "curl")
    (setq helm-net-prefer-curl’ t))

  ;; ^ website `google' via program `curl'.

  (progn
    ;; Helm and Ido (their `minor-mode's) are mutually-exclusive:
    (helm-autoresize-mode +1)
    (ido-mode            -1))

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/emacs-helm/helm'
;;   • URL `https://emacs-helm.github.io/helm/'
;;   • URL `https://github.com/emacs-helm/helm/wiki'
;;   • URL `https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm.el'
;; 

;; ^ `helm-mode' vs `helm-autoresize-mode': TODO.

;;----------------------------------------------;;

;; (use-package helm-mode
;;   :config
;;   (helm-mode +1))

;;----------------------------------------------;;

(use-package helm-buffers
  :load-path "vendor/helm"

  :commands (helm-buffers-list)

  :custom

  (helm-buffers-fuzzy-matching t "Fuzzily-Match buffer-names (for `helm-mini' when listing buffers).")

  (ido-use-virtual-buffers t "`helm-buffers-list' wants this.")

  :config

  (dolist (MODE '(picture-mode artist-mode))
    (add-to-list 'helm-buffers-favorite-modes MODE))

  ())

;; ^ ‘helm-buffers’:
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-files
  :load-path "vendor/helm"

  :commands (helm-find-files)

  :custom

  (helm-ff-file-name-history-use-recentf t "use `recentf'.")
  (helm-ff-search-library-in-sexp        t "search for library in `require' and `declare-function' sexp.")

  (helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
                                "hide these files from the Helm Buffer.")

  )

;; ^ ‘helm-files’:
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-font
    :load-path "vendor/helm"

    :commands (helm-ucs helm-select-xfont)

    :custom

    (helm-ucs-recent-size 999 "‘helm-ucs’ — remember more previously-chosen characters")

    :config ())

;; ^ ‘helm-font’:
;;
;; • ‘helm-ucs’ — *Helm* ‘insert-char’ (“:source”'s the ‘ucs-names’).
;;
;; • ‘helm-select-xfont’ — *Helm TUI* for *Fonts* (“:source”'s the ‘x-list-fonts’).
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-ring
    :load-path "vendor/helm"

    :commands (helm-show-kill-ring helm-all-mark-rings)

    :custom

    (helm-kill-ring-threshold 4 "‘helm-show-kill-ring’ — ignoores fewer short clipboard-entries")

    :config ())

;; ^ ‘helm-ring’:
;;
;; • ‘helm-show-kill-ring’ — *Helm TUI* for *Clipboard History* (“:source”'s the ‘kill-ring’).
;;
;; • ‘helm-all-mark-rings’ — *Helm TUI* for *Position History* (“:source”'s the ‘mark-ring’).
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-grep
    :load-path "vendor/helm"

    :commands (helm-do-grep-ag)

    :config ())

;; ^ ‘helm-grep’:
;;
;; • ‘helm-grep’ — 
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-find 
    :load-path "vendor/helm"

    :commands (helm-find)

    :config ())

;; ^ ‘helm-find’:
;;
;; • ‘helm-find’ — 
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-dabbrev
  :load-path "vendor/helm"

  :commands (helm-dabbrev)

  )

;; ^ ‘helm-dabbrev’:
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;----------------------------------------------;;

(use-package helm-sys
  :load-path "vendor/helm"

  :commands (helm-top)

  )

;; ^ ‘helm-sys’:
;;
;; URL `https://github.com/emacs-helm/helm'
;;

;;==============================================;;

;;; Secondary Helm Packages...

;;----------------------------------------------;;

(use-package helm-descbinds
    :demand t

    :commands (helm-descbinds-mode)

    :config

    (helm-descbinds-mode +1)

    ())

;; ^ `helm-descbinds':
;;
;; • is a Helm TUI for `describe-bindings'.
;;
;; • its Helm Actions include:
;;
;;     • Execute the command.
;;     • Describe the command.
;;     • Find the command.
;;
;; URL `https://github.com/emacs-helm/helm-descbinds'
;;

;;----------------------------------------------;;

(use-package helm-describe-modes
    :demand t

    :commands (helm-describe-modes)

    :config

    (global-set-key [remap describe-mode] #'helm-describe-modes)

    ())

;; ^ `helm-describe-modes':
;;
;; • is a Helm TUI for `describe-mode'.
;;
;; • its Helm Sources include:
;;
;;     • Current `major-mode'. 
;;     • Currently-Active `minor-mode's.
;;     • Inactive `minor-mode's.
;;
;; URL `https://github.com/emacs-helm/helm-describe-modes'
;;

;;----------------------------------------------;;

(use-package helm-navi

    :commands (helm-navi helm-navi-headings)

    :delight (helm-navi-mode)

    ;;------------------------;;

    :preface (require 'outline)

    :config

    (setf (cdr (assoc :ALL (cdr (assoc "emacs-lisp" navi-keywords))))
          "^[[:space:]]*(\\(use-package\\|\\(cl-\\)\\{0,1\\}def[a-z]+\\)\\*? ")

    ;; ^ Add ‘use-package’ lines to `navi-keywords':
    ;;
    ;; Quickly navigate to individual Package Configurations with ‘helm-navi’.
    ;;

    :config ())

;; ^ `helm-navi' provides:
;;
;; • `helm-navi' — for all ‘navi-mode’ keywords and all ‘outshine’ headings.
;;
;; • `helm-navi-headings' — for all ‘outshine’ headings.
;;
;;
;; URL `https://github.com/emacs-helm/helm-navi'
;;

;;----------------------------------------------;;

(use-package helm-wikipedia

    :commands (helm-wikipedia-suggest)

    :config ())

;; ^ `helm-wikipedia':
;;
;; URL `https://github.com/emacs-helm/helm-wikipedia'
;;

;;----------------------------------------------;;

(use-package helm-eww

    :commands (helm-eww
               helm-eww-buffers
               helm-eww-bookmarks
               helm-eww-history)

    :config ())

;; ^ `helm-eww' merges:
;;
;; • `helm-eww-buffers'
;; • `helm-eww-bookmarks'
;; • `helm-eww-history'
;;
;; URL `https://github.com/emacs-helm/helm-eww'
;;

;;----------------------------------------------;;

(use-package helm-system-packages

  :commands (helm-system-packages)

  :custom

  (helm-system-packages-candidate-limit 0 "‹0› means: Unlimited.")
  (helm-system-packages-use-symbol-at-point-p t "Guess input from `symbol-at-point'.")
;; (helm-system-packages-force-package-manager 'dpkg)

  :config

  ())

;; URL `https://github.com/emacs-helm/helm-system-packages'

;;----------------------------------------------;;

(use-package helm-dictionary
    :demand t

    :commands (helm-dictionary)

    :config

    ;;(setq helm-dictionary-database "")

    ())

;; ^ `helm-dictionary':
;;
;; • is a Helm TUI for Natural-Language Dictionaries, including Translation Dictionaries.
;;
;; • searches via Regexps. e.g.:
;;
;;     • « \b » — to search for complete words, denote Word Boundaries by surrounding with « \b »’s. e.g. « \bhouse\b » includes phrases like « White House », but excludes words like « household » and « workhouse ».
;;
;; URL `https://github.com/emacs-helm/helm-dictionary'
;;

;;----------------------------------------------;;

(use-package helm-pass

    :commands (helm-pass)

    :config ())

;; ^ `helm-pass':
;;
;; • is a Helm TUI for program ‘pass’.
;;
;; URL `https://github.com/emacs-helm/helm-pass'
;;

;;----------------------------------------------;;

(use-package helm-firefox

    :commands (helm-firefox)

    :config ())

;; ^ `helm-firefox':
;;
;; • is a Helm TUI for *Browser Bookmarks* (of program ‘firefox’).
;;
;; URL `https://github.com/emacs-helm/helm-firefox'
;;

;;==============================================;;

;;; Tertiary Helm Packages...

;;----------------------------------------------;;

(use-package helm-swoop

  :commands (helm-swoop helm-multi-swoop)

  :bind (("<f2>"   . helm-swoop)
         ("S-<f2>" . helm-multi-swoop)
         )

  :custom

  (helm-swoop-speed-or-color nil "« nil » means: boost the Invoke-Speed (slightly), lose any Text-Color.")

  (helm-swoop-use-fuzzy-match t "Fuzzily-Match.")

  ;; ^ fuzzy matching means: TODO.

  (helm-swoop-split-direction 'split-window-horizontally "Horizontally or Vertically.")

  ;;TODO:
  ;; helm-swoop-pre-input-function #'symbol-at-point-or-helm-swoop-pattern
  ;; ;; ^ if there is no symbol at the cursor, use the last used words instead.
  ;; ;; `helm-swoop-pattern' holds the last used words.

  ())

;; ^ ‘helm-swoop’:
;;
;; • ‘helm-swoop’ is like ‘helm-occur’.
;;
;; URL `https://github.com/ShingoFukuyama/helm-swoop'
;;

;;----------------------------------------------;;

(use-package helm-google

    :commands (helm-google)

    :custom

    (helm-google-default-engine 'google)

    :config ())

;; ^ ‘helm-google’:
;;
;; URL `https://framagit.org/steckerhalter/helm-google'
;;

;;----------------------------------------------;;

(use-package helm-flyspell

  :commands (helm-flyspell-correct)

  :bind (:map flyspell-mode-map
              ("s-SPC" . helm-flyspell-correct)
              ("<f9>"  . helm-flyspell-correct)
         )

  :config ())

;; ^ ‘helm-flyspell’:
;;
;; URL `https://github.com/pronobis/helm-flyspell'
;;

;;----------------------------------------------;;
;;; External Packages: Completion (Company) ----;;
;;----------------------------------------------;;

;; Primary Company Packages...

(when (require 'sboo-company nil :no-error)

  ;;------------------------;;

  (use-package company
    :load-path "vendor/company-mode"

    ;;------------------------;;

    :commands (company-complete-common-or-cycle
               company-diag
               company-mode)

    ;;------------------------;;

    :bind (("<backtab>" . company-complete-common-or-cycle)
           )

    ;;------------------------;;

    :hook ((prog-mode       . sboo-company-prog-setup)
           (emacs-lisp-mode . sboo-company-elisp-setup)
           (haskell-mode    . sboo-company-haskell-setup)
           (nix-mode        . sboo-company-nix-setup)
           (python-mode     . sboo-company-python-setup)
           (javascript-mode . sboo-company-javascript-setup)
           (text-mode       . sboo-company-text-setup))

    ;;------------------------;;

    :delight (company-mode " ©")

    ;;------------------------;;

    :custom

    (company-show-numbers               t   "")
    (company-minimum-prefix-length      1   "minimum Prefix Length for Idle Completion.")
    (company-tooltip-align-annotations  t   "")
    (company-dabbrev-downcase           nil "")

    ;;------------------------;;

    :config

    (sboo-company-register-backends)
    (sboo-company-register-frontends)

    ;;------------------------;;

    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    ;; ^ use Company for Emacs's Builtin Completion.

    ;; ^ NOTE why not `:bind'?
    ;;   Because `:bind''s syntax is « (`kbd' ...) » only, no « [`remap' ...] ».

    ;;------------------------;;

    ;; `company-active-map':

    (define-key company-active-map (kbd "TAB")       #'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<backtab>") #'sboo-company-complete-common-or-previous-cycle)

    (bind-keys :map company-active-map

               ("!"           . sboo-company-complete-1)
               ("<kp-1>"      . sboo-company-complete-1)
               ("<kp-end>"    . sboo-company-complete-1)

               ("@"           . sboo-company-complete-2)
               ("<kp-2>"      . sboo-company-complete-2)
               ("<kp-down>"   . sboo-company-complete-2)

               ("#"           . sboo-company-complete-3)
               ("<kp-3>"      . sboo-company-complete-3)
               ("<kp-next>"   . sboo-company-complete-3)

               ("$"           . sboo-company-complete-4)
               ("<kp-4>"      . sboo-company-complete-4)
               ("<kp-left>"   . sboo-company-complete-4)

               ("%"           . sboo-company-complete-5)
               ("<kp-5>"      . sboo-company-complete-5)
               ("<kp-begin>"  . sboo-company-complete-5)

               ("^"           . sboo-company-complete-6)
               ("<kp-6>"      . sboo-company-complete-6)
               ("<kp-right>"  . sboo-company-complete-6)

               ("&"           . sboo-company-complete-7)
               ("<kp-7>"      . sboo-company-complete-7)
               ("<kp-home>"   . sboo-company-complete-7)

               ("*"           . sboo-company-complete-8)
               ("<kp-8>"      . sboo-company-complete-8)
               ("<kp-up>"     . sboo-company-complete-8)

               ("("           . sboo-company-complete-9)
               ("<kp-9>"      . sboo-company-complete-9)
               ("<kp-prior>"  . sboo-company-complete-9)

               (")"           . sboo-company-complete-10)
               ("<kp-0>"      . sboo-company-complete-10)
               ("<kp-insert>" . sboo-company-complete-10)

               )

    ;; ^ FYI candidate selection via `M-{number}'
    ;;  (i.e. `M-1', `M-2', ..., `M-9').

    ;;------------------------;;

    (global-company-mode +1)))

;;----------------------------------------------;;

(use-package company-ispell

  :custom

  (company-ispell-dictionary (sboo-data-file "english-words.txt"))

  :config

  ())

;; ^ ‘company-ispell-dictionary’ defaults to ‘ispell-complete-word-dict’.

;; ^ Links:
;;
;;   • URL `http://blog.binchen.org/posts/emacs-auto-completion-for-non-programmers.html'
;;   • URL `http://blog.binchen.org/posts/how-to-spell-check-functionvariable-in-emacs.html' 

;;==============================================;;

;;----------------------------------------------;;
;; External Packages: Templates ----------------;;
;;----------------------------------------------;;

(when (require 'sboo-yass nil :no-error)

  (use-package yasnippet

    :demand

    :commands (snippet-mode yas-insert-snippet yas-next-field-or-maybe-expand)

    :delight (yas-minor-mode " Y")

    ;;------------------------;;

    :mode ("\\.yasnippet\\'" . snippet-mode)

    ;;------------------------;;

    :bind (("<kp-home>" . yas-next-field-or-maybe-expand)
           )

    ;;------------------------;;

    :custom

    (yas-wrap-around-region t
                            "Enables setting « $0 » field to `region' (by default).")

    (yas-indent-line 'fixed
                     "Indent the snippet to the current column (of the snippet, not the file into which the snippet is being inserted).")

    (yas-snippet-dirs `(,sboo-snippets-directory)
                      "Register personal snippets.")

    (yas-trigger-symbol "↣"
                       "Unicode-ify.")

    (yas-new-snippet-default "\
# -*- mode: snippet -*-
#
# key         : $1
# name        : [sboo] a « $2 ».
#
# type        : snippet
# condition   : (let ((KEY "$1")) (condition-case nil (sboo-yas-column-p :key KEY :indentation 6) (void-function (= (current-column) (string-width KEY)))))
# expand-env  : ((yas-indent-line 'fixed) (yas-wrap-around-region 'nil))
#
# commentary  : 
# contributor : Spiros Boosalis <samboosalis@gmail.com> 
#
# --
$0")

    :custom-face

    (yas-field-highlight-face ((t (:inherit 'region :slant italic))))

    :init

    (setq yas-alias-to-yas/prefix-p nil)

    ;; ^ `setq' vs `add-to-list': remove the default.

    ;;------------------------;;

    :config

    (defun sboo-yas-reload (&optional force)

      "Recompile and reload all « .yasnippet » files."

      (interactive "P")

      (yas-recompile-all)
      (yas-reload-all force)
      (load "sboo-yass" :no-error :no-message))

    ;;------------------------;;

    (defalias '/y #'yas-insert-snippet)

    ;;------------------------;;

    ;;(add-to-list 'yas-snippet-dirs "~/.emacs.d/submodules/yasnippet-snippets/snippets")

    ;; ^ Links:
    ;;
    ;;   • URL `https://github.com/AndreaCrotti/yasnippet-snippets'
    ;;   • URL `http://andreacrotti.github.io/yasnippet-snippets/snippets.html'
    ;;

    ;;------------------------;;

    (add-hook 'emacs-startup-hook #'sboo-yas-reload)

    (yas-global-mode +1)

    ;;------------------------;;

    ()))

;; ^ Links:
;;
;;   • URL `https://github.com/joaotavora/yasnippet'
;;   • URL `http://joaotavora.github.io/yasnippet/snippet-development.html'
;;   • URL `https://joaotavora.github.io/yasnippet/snippet-organization.html'
;;   • URL `https://joaotavora.github.io/yasnippet/snippet-expansion.html'
;;   • URL `https://github.com/haskell/haskell-snippets'
;;

;;==============================================;;

;;; Secondary Company Packages...

;;----------------------------------------------;;

;;==============================================;;

;;; Tertiary Company Packages...

;;----------------------------------------------;;

;;----------------------------------------------;;
;; External Packages: Programming --------------;;
;;----------------------------------------------;;

(use-package flycheck
  :defer t

  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error
             )

  :delight (flycheck-mode " 🛸")

  ;; :hook ((emacs-lisp-mode . flycheck-mode))

  :bind (
         :map emacs-lisp-mode-map
              (("<kp-prior>" . flycheck-previous-error)
               ("<kp-next>"  . flycheck-next-error))
         :map emacs-lisp-mode-map
              (("M-<left>"   . flycheck-previous-error)
               ("M-<right>"  . flycheck-next-error))
         ;; :map js-mode-map
         ;;      (("<kp-prior>" . flycheck-previous-error)
         ;;       ("<kp-next>"  . flycheck-next-error))
         ;; :map c-mode-base-map
         ;;      (("<kp-prior>" . flycheck-previous-error)
         ;;       ("<kp-next>"  . flycheck-next-error))
         ;; :map haskell-mode-map
         ;;      (("<kp-prior>" . flycheck-previous-error)
         ;;       ("<kp-next>"  . flycheck-next-error))
         )

  :config

  (add-hook 'flycheck-error-list-mode-hook #'visual-line-mode)

  (when (require 'sboo-flycheck nil :no-error)
    (bind-key "<kp-divide>" #'sboo-flycheck)
    (add-to-list 'display-buffer-alist sboo-flycheck-display-buffer))

  ())

;; ^ Links:
;;
;;   • URL `https://www.flycheck.org/'
;;   • URL `https://github.com/flycheck/flycheck'
;;   • URL `https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-flycheck.el'
;;

;; ^ NOTES
;;
;; • `flycheck' is “«:defer»red” because flycheck is more a Framework than an Application;
;;    i.e. any "application" package will `require' it whenever needed (e.g. `dante').
;;

;;TODO;; style « *Flycheck error messages* »

;;TODO;; mode of « *Flycheck errors* » (e.g. « *Flycheck errors for buffer ...* »)

;;----------------------------------------------;;

(when (require 'sboo-projectile nil :no-error)

  (use-package projectile

    :commands (projectile-mode)

    :delight '(:eval (concat " " (projectile-project-name)))

    ;; ^ ❶ Hide the mode name for projectile-mode.
    ;;   ❷ Show the project name instead.

    :bind-keymap ("C-c p" . projectile-command-map)

    :preface

    (defun sboo-projectile-invalidate-cache (&rest _)
      "(`projectile-invalidate-cache' which ignores the args to `magit-checkout'.)"
      (projectile-invalidate-cache nil))

    :config

    (sboo-append-to-list! projectile-globally-ignored-directories
                          sboo-projectile-excluded-directories)

    (sboo-append-to-list! projectile-globally-ignored-files
                          sboo-projectile-excluded-file-names)

    (sboo-append-to-list! projectile-globally-ignored-file-suffixes
                          sboo-projectile-excluded-file-extensions)

    (with-eval-after-load 'magit-branch
      (advice-add 'magit-checkout
                  :after #'sboo-projectile-invalidate-cache)
      (advice-add 'magit-branch-and-checkout
                  :after #'sboo-projectile-invalidate-cache))

    ()))

;;----------------------------------------------;;
;;; `magit': "eMAcs GIT".

(progn

  ;;--------------------------;;

  (use-package magit

 ;; :load-path ("vendored/magit")
    :load-path ("submodules/magit/lisp")

    :commands (magit-status)

    :bind (("s-g s" . magit-status)
           ("s-g S" . sboo-magit-status-with-prefix)
           )
    :bind (:map magit-mode-map
                (("U" . magit-unstage-all) 
                 ("M-h") ("M-s") ("M-m") ("M-w")))
    :bind (:map magit-file-section-map ("<C-return>"))
    :bind (:map magit-hunk-section-map ("<C-return>"))

    :hook ((magit-mode . hl-line-mode)
           )

    :custom

    (magit-save-repository-buffers 'dontask "don't ask (just save).")

  :preface

  (defun sboo-magit-status-with-prefix ()
    "`magit-status' with `current-prefix-arg' of `4'."
    (interactive)                   
    (let* ((current-prefix-arg '(4))
          )
      (call-interactively #'magit-status)))

  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+?\\)\\(\\.git\\)?\\'" "\\1"
              (magit-get "remote" (magit-get-remote) "url"))
             (magit-get-current-branch))))

  :config

  ;;TODO (define-key magit-mode-map "G" #'endless/visit-pull-request-url)

;;  (eval-after-load 'magit-pull
;;    '(transient-insert-suffix 'magit-pull "p"
;;       '("F" "default" magit-fetch-from-upstream)))

;;  (eval-after-load 'magit-push
;;    '(transient-insert-suffix 'magit-push "p"
;;       '("P" "default" magit-push-current-to-upstream))))

    ())

  ;;--------------------------;;

;;  (use-package magit-commit
;;    :after (magit)
;;
;;    :config
;;
;;    (use-package git-commit)
;;
;;    ())

  ;;--------------------------;;

 (use-package magit-files
   :after (magit)

   :commands (magit-diff
              magit-log
              magit-stage-file
              magit-unstage-file
              magit-commit
              )

   :config

   (global-magit-file-mode +1)

   ())

  ;;--------------------------;;

  ;; (use-package magithub) ;FIXME" magithub crashes magit.

  ;;--------------------------;;

  ())

;;----------------------------------------------;;

(use-package gitignore-mode

  :commands (gitignore-mode)

  :mode ((rx "/" ".gitignore" eos) . gitignore-mode)

  :config
  
  ())

;; ^ Links:
;;
;;   • URL `'
;;

;;----------------------------------------------;;

(use-package gitconfig-mode

  :commands (gitconfig-mode)

  :config
  
  ())

;; ^ Links:
;;
;;   • URL `'
;;

;;----------------------------------------------;;

(use-package gitattributes-mode

  :commands (gitattributes-mode)

  :config
  
  ())

;; ^ Links:
;;
;;   • URL `'
;;

;;----------------------------------------------;;
;; External Packages: Lisp ---------------------;;
;;----------------------------------------------;;

(use-package package-lint

  :commands (package-lint-current-buffer)

  :config ())

;; ^ `package-lint' provides a Linter (via `package-lint-current-buffer') 
;;    for Emacs-LISP, checking a « *.el » file's Library Headers.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/purcell/package-lint'
;;
  
;;----------------------------------------------;;

(use-package flycheck-package
  :demand t

  :config

  (with-eval-after-load 'flycheck
    (flycheck-package-setup))

  ())

;; ^ `flycheck-package' provides feedback (via `flycheck') about issues with an elisp file's package metadata a.k.a. its library headers (activated only if a Package-Requires or Package-Version header is present).
;;

;; ^ Links:
;;
;;   • URL `https://github.com/purcell/flycheck-package'
;;

;;----------------------------------------------;;

(require 'use-package-lint nil :no-error)

;; ^ Provides: ⟨ M-x `use-package-lint' ⟩.

;;----------------------------------------------;;
;; External Packages: Haskell ------------------;;
;;----------------------------------------------;;

(when (require 'sboo-haskell nil :no-error)

  ;;------------------------;;

  (use-package haskell
    :demand t
    :load-path "submodules/haskell-mode"

    :commands (haskell-mode)

    ;;------------------------;;

    :interpreter (("runhaskell"  . haskell-mode)
                  ("runghc"      . haskell-mode)
                  ("cabal"       . haskell-mode)
                  ("stack"       . haskell-mode)
                  )

    :mode        (("\\.hs\\'"      . haskell-mode)
                  ("\\.chs\\'"     . haskell-mode)
                  ("\\.hsig\\'"    . haskell-mode)
                  ("\\.hsc\\'"     . haskell-mode)
                  ("\\.hs-boot\\'" . haskell-mode)
                  ("\\.lhs\\'"     . literate-haskell-mode)
                  )

    ;;------------------------;;

    ;; mnemonic:
    ;; • "s-i" — personal keymap for INSERTING stuff.

    ;;------------------------;;

    :bind (:map haskell-mode-map
                ("s-i u" . sboo-haskell-insert-undefined))

    ;;------------------------;;

    :hook        ((haskell-mode . interactive-haskell-mode))

    ;;------------------------;;

    :custom
    (haskell-tags-on-save                         t                              "Continuously update « TAGS » file via « hasktags ».")
    (haskell-process-type                         'cabal-new-repl                "« cabal new-repl »")
    (haskell-process-args-cabal-repl             '("--ghc-option=-ferror-spans") "")
    (haskell-process-log                          t                              "")
    (haskell-process-suggest-remove-import-lines  t                              "")
    (haskell-process-auto-import-loaded-modules   t                              "")
    (haskell-process-suggest-hoogle-imports       t                              "")
    (haskell-stylish-on-save                      t                              "")
    (haskell-ask-also-kill-buffers              nil                              "")
    (haskell-interactive-popup-errors           nil                              "")

       ;;'(haskell-process-path-ghci        "cabal")
       ;; '(haskell-process-type             (quote stack-ghci))
       ;; '(haskell-process-path-ghci        "stack")


;; (custom-set-variables
;;  '(haskell-ask-also-kill-buffers nil)
;;  '(haskell-interactive-popup-errors nil)
;;  '(haskell-process-args-cabal-repl (list "--ghc-option=-ferror-spans"))
    ;;  '(haskell-process-type '(cabal-new-repl))
    ;; )

    ;;------------------------;;

    :preface

    (defun sboo-haskell-insert-undefined ()
      "`insert' « undefined » (e.g. during development.)"
      (interactive)
      (insert "undefined"))

    ;;------------------------;;

    :init

    (setq haskell-doc-current-info #'sboo-haskell-doc-current-info)

    ;; (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    ;;
    ;; ^ `haskell-process' repeatedly spams errors for working projects,
    ;; stealing focus from the current buffer.

    (remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

    ;; ^ See « https://wiki.haskell.org/Emacs/Inferior_Haskell_processes ».

    ;;------------------------;;

    :config
    
    (dolist (HOOK sboo-haskell-hooks-list)
      (add-hook 'haskell-mode-hook HOOK))

    (dolist (QQ sboo-haskell-quasi-quote-alist)
      (add-to-list 'haskell-font-lock-quasi-quote-modes QQ))

    (when (require 'sboo-ghc nil :no-error)

      (dolist (EXTENSION sboo-ghc-language-extensions)
        (cl-pushnew EXTENSION
                    haskell-ghc-supported-extensions
                    :test #'equal))

      (dolist (OPTION sboo-ghc-compiler-options)
        (cl-pushnew OPTION
                    haskell-ghc-supported-options
                    :test #'equal)))

    (sboo-haskell-modify-syntax-entries)

    ())

  ;;------------------------;;

  (use-package haskell-decl-scan
    :after    haskell
    
    :commands (haskell-decl-scan-mode)
    
    ;;;:hook     ((haskell-mode . haskell-decl-scan-mode))

    :init

    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)

    :config

    ())

  ;;------------------------;;

  (use-package haskell-cabal
    :after (haskell)

    :commands (haskell-cabal-mode)

    :mode (("\\.cabal\\'"      . haskell-cabal-mode)
           ("\\.project\\'"    . haskell-cabal-mode)
           ("\\`\\.project\\'" . haskell-cabal-mode))

    :config

    ())

  ;;------------------------;;

  (use-package dante
    :after (haskell)

    :load-path "submodules/dante"

    :commands (dante-mode dante-restart)

    :bind (:map haskell-mode-map
                (("C-c d" . sboo-dante-mode)))

;;;  :hook ((haskell-mode . flycheck-mode)
;;;         (haskell-mode . dante-mode))

    :init

    (add-hook 'haskell-mode-hook #'flycheck-mode)

    ;; (if (commandp #'sboo-dante-mode)
    ;;     (add-hook 'haskell-mode-hook #'sboo-dante-mode)
    ;;   (add-hook 'haskell-mode-hook #'dante-mode))

    :config

    (dolist (KEY '(nix new-nix))
      (setq dante-methods-alist
            (assq-delete-all KEY dante-methods-alist)))

    (when (bound-and-true-p sboo-dante-methods)
      (setq dante-methods sboo-dante-methods))

    (setq dante-tap-type-time 2)

    (setq sboo-haskell-eldoc 'dante)

    (add-to-list 'display-buffer-alist sboo-dante-display-buffer)

    ;; ^ Hide `dante-mode' popup (via `display-buffer-alist').

    ())

  ;; ^ Configure `dante':
  ;;
  ;; * load `dante.el', which registers `dante-target' and `dante-project-root' as `safe-local-var's.
  ;; * `autoload' the `dante-mode' command, so we can run 《 M-x dante-mode 》 manually.
  ;; 
  ;; 

  ;;------------------------;;

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/jyp/dante'
;;

;;----------------------------------------------;;
;; External Packages: Programming Languages ----;;
;;----------------------------------------------;;

(when (require 'sboo-nix nil :no-error)

  ;;------------------------;;

  (use-package nix-mode

    :load-path ("vendor/nix-mode")
    ;; :load-path ("vendor/nix-mode")
    ;; :load-path ("submodules/nix-mode")

    :interpreter (("nix"       . nix-mode)
                  ("nix-build" . nix-mode)
                  ("nix-shell" . nix-mode)
                  ("nix-env"   . nix-mode)
                  )

    :mode        (("\\.nix\\'" . nix-mode)
                  )

    :init

    (dolist (HOOK sboo-nix-hooks-list)
      (add-hook 'nix-mode-hook HOOK))

    :config ())

  ;;------------------------;;

  (use-package nix-repl

    :load-path ("vendor/nix-mode")
 
    :config ())

  ;;------------------------;;

  ())

;;----------------------------------------------;;

(when (require 'sboo-scheme nil :no-error)

  ;;------------------------;;

  (use-package quack

    :commands ()

    :interpreter (("guile"     . scheme-mode)
                  )

    :mode        (("\\.scm\\'" . scheme-mode)
                  )

    ;;:hook (scheme-mode-hook . scheme-mode-quack-fontify)

    :init

    (setq scheme-program-name "guile")

    ;; ^ interpreter (binary).
    ;;
    ;;   e.g.: guile, racket, ...

    (dolist (HOOK sboo-scheme-hooks-list)
      (add-hook 'scheme-mode-hook HOOK))

;; (defun scheme-mode-quack-fontify ()
;;   (require 'quack)
;;   (setq quack-fontify-style 'emacs))

    ())

  ;;------------------------;;

  ())

;;----------------------------------------------;;

(when (require 'sboo-haskell-compilation nil :no-error)

  ()

  ())

;;----------------------------------------------;;
;;; External Packages: `company-*' Configurations

(when (require 'sboo-company nil :no-error)


  ;;------------------------;;

  (use-package company-quickhelp
    :after (company)

    :config

    (company-quickhelp-mode +1)

    ())

  ;; ^ Links:
  ;;
  ;;   • URL `https://github.com/expez/company-quickhelp'
  ;;

  ;;------------------------;;

  (use-package company-cabal
    :after (company)

    :config

    (add-to-list 'company-backends 'company-cabal)

    ())

  ;; ^ Links:
  ;;
  ;;   • URL `https://github.com/iquiw/company-cabal'
  ;;

  ;;------------------------;;

  (use-package company-ghci
    :after (company)

    :config

    (add-hook 'haskell-mode-hook             #'company-mode)
    (add-hook 'haskell-interactive-mode-hook #'company-mode)
    ;; ^ for completions in the REPL

    (add-to-list 'company-backends 'company-ghci)

    ())

  ;; ^ Links:
  ;;
  ;;   • URL `https://github.com/horellana/company-ghci'
  ;;


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

(use-package llvm-mode

  :mode "\\.ll\\'"

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/llvm-mode.el'
;;   • URL `'
;;

;;----------------------------------------------;;

(use-package lua-mode

  :mode "\\.lua\\'"
  :interpreter "lua"

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/immerrr/lua-mode'
;;   • URL `https://immerrr.github.io/lua-mode/'
;;

;;----------------------------------------------;;
;;; External Packages: Formats -----------------;;
;;----------------------------------------------;;

(use-package markdown-mode

  :commands (markdown-mode gfm-mode markdown-edit-code-block)

  ;;--------------------------;;

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         )

  ;;--------------------------;;

  :bind (:map markdown-mode-map
              ("TAB" . dabbrev-expand)
              ("C-c !" . sboo-markdown-mark-h1)  ; Mnemonic: « C-c » is the Personal Keymap; « ! » shares a key with « 1 », referencing « <h1> ».
         :map gfm-mode-map
              ("TAB" . dabbrev-expand)
         )

  ;;--------------------------;;

  :custom

  (markdown-command "multimarkdown" "")  ;; TODO `pandoc'
  (imenu-auto-rescan nil "non-`nil' means: Imenu always rescans the (file-)buffer.")

  ;;--------------------------;;

  :config

  (when (require 'sboo-markdown nil :no-error)

    ())

  (dolist (ASSOCIATION '(("lisp"   . lisp-mode)
                         ("scheme" . scheme-mode)
                         ("guile"  . scheme-mode)

                         ("sh"    . shell-script-mode)
                         ("shell" . shell-script-mode)
                         ("bash"  . shell-script-mode)

                         ("sql"        . sql-mode)
                         ("postgresql" . sql-mode)

                         ("haskell" . haskell-mode)
                         ("cabal"   . haskell-cabal-mode)

                         ("xml"        . xml-mode)
                         ("html"       . html-mode)
                         ("css"        . css-mode)
                         ("js"         . javascript-mode)
                         ("javascript" . javascript-mode)
                         ("json"       . json-mode)
                         ("markdown"   . markdown-mode)

                         ("dot" . graphviz-dot-mode)))

    (add-to-list 'markdown-code-lang-modes ASSOCIATION))

  (if (require 'sboo-html nil :no-error)
    (dolist (HOOK (sboo-markdown-hooks))
      (dolist (FUNCTION sboo-markdown-functions)
        (add-hook HOOK FUNCTION)))
    ;; ^ Register custom commands and with Mode-Inheritance.
    (progn
      (add-hook 'markdown-mode-hook #'imenu-add-menubar-index)
      ;; ^ [Gracefully-Degrade] Register builtin functions.
      ()))

  (when (require 'sboo-html nil :no-error)
    (dolist (HOOK sboo-html-hooks)
      (dolist (FUNCTION sboo-html-functions)
        (add-hook HOOK FUNCTION))))

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/jrblevin/markdown-mode'
;;   • URL `https://jblevins.org/log/markdown-imenu'
;;

;; ^ NOTE:
;;
;; `gfm-mode' abbreviates "GitHub-flavored markdown".
;;
;; 

;;----------------------------------------------;;

(use-package json-mode

  ;; :disabled

  :mode (("\\.json\\'" . json-mode)
         )

  :config

  ;;--------------------------;;

  (add-hook 'json-mode-hook #'flycheck-mode)
  (add-hook 'json-mode-hook #'sboo-set-font-to-iosevka)

  ;; ^ FlyCheck builds-in a « jsonlint » checker
  ;; ^ « jsonlint » is a JSON Linter.

  ;;--------------------------;;

  (when (>= emacs-major-version 25)     ; for `cl-delete-if'.

    (defun sboo/alist/json-mode-value-p (KV)
      "e.g. « (sboo/alist/json-mode-value-p '(\"^[{[]$\" . json-mode)) » is `t'."
      (let* ((V (cdr KV)) (B (eq 'json-mode V))) B))

    (cl-delete-if #'sboo/alist/json-mode-value-p magic-mode-alist) ;TODO
    (cl-delete-if #'sboo/alist/json-mode-value-p magic-fallback-mode-alist)

    ;; ^ Remove `json-mode' from `magic-mode-alist'.

    ())

  ;;--------------------------;;

  ())

;; ^ `json-mode' registers (via ‘autoload’):
;;
;;     ;; Well formatted JSON files almost always begin with “{” or “[”.
;;     (add-to-list 'magic-fallback-mode-alist '("^[{[]$" . json-mode))
;;

;;----------------------------------------------;;

(use-package yaml-mode

  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'"  . yaml-mode)
         )

  :config

  (add-hook 'yaml-mode-hook #'flycheck-mode)
  (add-hook 'yaml-mode-hook #'sboo-set-font-to-iosevka)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/yoshiki/yaml-mode'
;;

;;----------------------------------------------;;

(use-package makefile-runner

  :commands (makefile-runner)

  :bind ((("s-<Scroll_Lock>" . makefile-runner)
          )
         :map sboo-launch-keymap
              (("m" . makefile-runner)
               )
         )

  ;; ^ mnemonics:
  ;;
  ;; • "<Scroll_Lock>" — personal keybinding for `compile-command'.
  ;;
  ;; • "s-r" — personal keymap for [R]UNNING stuff.
  ;; • "m" — run « [m]ake ».
  ;;

  :config ())

;; ^ Command `makefile-runner':
;;
;; ❶ locates a « Makefile ».
;; ❷ parses the Makefile Targets (for `completing-read').
;;

;; ^ Links:
;;
;;   • URL `https://github.com/danamlund/emacs-makefile-runner'
;;   • URL `http://danamlund.dk/emacs/make-runner.html'
;;

;;----------------------------------------------;;

(use-package bnf-mode

  :load-path "~/.emacs.d/submodules/bnf-mode"

;;  :load-path (sboo-submodule-directory "bnf-mode")

  :commands (bnf-mode)

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/sergeyklay/bnf-mode'
;;

;;----------------------------------------------;;

(when (require 'sboo-xmodmap nil :no-error)

  (use-package xmodmap-mode

    :mode        "\\.xmodmap\\'"
    :interpreter "xmodmap"

    :init

    ()

    :config

    (dolist (HOOK sboo-xmodmap-hooks-list)
      (add-hook 'xmodmap-mode-hook HOOK))

    ()))

;; ^ Links:
;;
;;   • URL `https://github.com/dominikh/dotfiles/blob/master/emacs.d/contrib/xmodmap-mode.el'
;;

;;----------------------------------------------;;

(use-package csv-mode

  :commands (csv-mode)

  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)

  :custom

  (csv-separators '("," ";" "|" " "))

  :config ())

;; ^ “CSV” abbreviates “[C]haracter-[S]eparated [V]alues”.

;; ^ Links:
;;
;;   • URL `https://www.emacswiki.org/emacs/CsvMode'
;;   • URL `https://elpa.gnu.org/packages/csv-mode.html'
;;

;;----------------------------------------------;;

(use-package graphviz-dot-mode

  :commands (graphviz-dot-mode)

  :mode "\\.dot\\'"

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/ppareit/graphviz-dot-mode'
;;   • URL `'
;;

;;----------------------------------------------;;

;; (use-package xpm
;;   :commands (xpm-grok xpm-finish xpm-raster xpm-as-xpm xpm-put-points xpm-generate-buffer)
;;   :mode (("\\.xpm\\'" . c-mode))
;;   ; :mode (("\\.xpm\\'" . xpm-mode))
;;   ())


;;----------------------------------------------;;

(use-package cask-mode

  :commands (cask-mode)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/Wilfred/cask-mode'
;;

;;----------------------------------------------;;

(use-package mediawiki

  :commands (mediawiki-site mediawiki-open)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/hexmode/mediawiki-el'
;;

;;----------------------------------------------;;

(use-package dockerfile-mode

  :mode "Dockerfile[a-zA-Z.-]*\\'" ; (rx bos "Dockerfile" (0+ (char ".-" alpha)) eos)

  :config

  ())

;;----------------------------------------------;;

(use-package nsis-mode

  :commands (nsis-mode nsis-compile-and-run nsis-execute-buffer nsis-run-file)

  :mode ("\.[Nn][Ss][HhIi]\'" . nsis-mode)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/mattfidler/nsis-mode'
;;   • URL `www.nsi.org'
;;

;;----------------------------------------------;;
;;; External Packages: Tools -------------------;;
;;----------------------------------------------;;

(use-package jq-mode

  :commands (jq-mode jq-interactively)

  :mode "\\.jq\\'"

  :config

  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "s-m m j q") #'jq-interactively))

  ())

;; « M-x `jq-interactively' »:
;;
;; • runs the query (which was entered into the minibuffer) iteratively over the JSON buffer.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/ljos/jq-mode'
;;

;;----------------------------------------------;;

(use-package pandoc-mode

  :commands (pandoc-mode)

  :hook ((pandoc-mode   . pandoc-load-default-settings)
         (markdown-mode . pandoc-mode)
         )

  :config ())

;; ^ `pandoc-mode' is a Minor Mode interacting with Pandoc:
;;
;;   • set the various options that Pandoc accepts.
;;   • run Pandoc on the `current-buffer' (as input filw).
;;
;;    `pandoc-mode' can be activated alongside the Major Mode for any of Pandoc's supported input formats
;;   (e.g. `markdown-mode').
;;

;; ^ Links:
;;
;;   • URL `https://github.com/joostkremers/pandoc-mode'
;;   • URL `http://joostkremers.github.io/pandoc-mode/'
;;

;;----------------------------------------------;;

(use-package pass

  :commands (pass pass-view-mode)

  :mode (".*\\.gpg\\'" . pass-view-mode)

  :preface

  (defun sboo-insert-password ()
    (interactive)
    (shell-command "apg -m24 -x24 -a1 -n1" t))

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/NicolasPetton/pass'
;;

;;==============================================;;

(use-package gmail-message-mode

  :commands (gmail-message-mode)

  :init (setq gmm/auto-mode-list t)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/Malabarba/gmail-mode/'
;;   • URL `http://www.emacswiki.org/emacs/Edit_with_Emacs'
;;

;;----------------------------------------------;;

(use-package ham-mode

  :mode (".*email\\'" . ham-mode)

  :config ())

;; "HAM" abbreviates "Html As Markdown".
;;
;; URL `https://github.com/Malabarba/ham-mode/'

;;----------------------------------------------;;
;;; External Packages: Editing -----------------;;
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

(use-package fdlcap

  :commands (fdlcap-change-case-region
             fdlcap-change-case-current-word
             sboo-fdlcap-dwim)

  :bind (("S-<f3>" . sboo-fdlcap-dwim) ; like Microsoft Word.
         )

  :preface

  (when (fboundp #'defun-dwim)
    (defun-dwim sboo-fdlcap-dwim fdlcap-change-case-region fdlcap-change-case-current-word "Cycle through `lowercase' → `capitalized' → `uppercase' → …"))

  :config ())

;; ^ Links:
;;
;;   • URL `http://www.emacswiki.org/elisp/fdlcap.el'
;;

;;----------------------------------------------;;

(use-package string-inflection

  ;;--------------------------;;

  :commands (string-inflection-all-cycle)

  ;;--------------------------;;

  :bind ((("C--" . string-inflection-all-cycle)
          )
         :map sboo-edit-keymap
              ("-" . string-inflection-all-cycle)
         :map java-mode-map
              ("C--" . string-inflection-java-style-cycle)
         :map python-mode-map
              ("C--" . string-inflection-python-style-cycle)
         :map ruby-mode-map
              ("C--" . string-inflection-ruby-style-cycle)
         :map haskell-mode-map
              ("C--" . sboo-string-inflection-haskell-style-cycle)
        )

  ;;--------------------------;;

  :preface

  (defun sboo-string-inflection-haskell-style-cycle-function (word)
    "Cycle WORD between « fooBar ⇒ FooBar ⇒ foo_bar ⇒ FOO_BAR ⇒ ... ».

Inputs:

• WORD — a `stringp'.

Output:

• a `stringp'.
  defaults to camel-casing (i.e. `string-inflection-lower-camelcase-function').

Laws:

• “4-potent” — the fourth invocation is equivalent to no invocation,
  modulo `point' (see « Effects »).

Effects:

• `point' — may move `point' to `end-of-word'.

Notes (naming conventions in the Haskell programming language):

• camel-case  (e.g. « fooBar »)  — Haskell functions (conventional).
• class-case  (e.g. « FooBar »)  — Haskell types and constructors (conventional).
• snake-case  (e.g. « foo_bar ») — Haskell « foregin import »s (unconventional).
• const-case  (e.g. « FOO_BAR ») — Haskell constants (unconventional).

Related:

• « * ⇒ fooBar »  — via `string-inflection-lower-camelcase'
• « * ⇒ FooBar »  — via `string-inflection-camelcase'
• « * ⇒ foo_bar » — via `string-inflection-underscore'
• « * ⇒ FOO_BAR » — via `string-inflection-underscore' + `string-inflection-upcase'
• « * ⇒ Foo_Bar » — via `string-inflection-capital-underscore'

Links:

• URL `https://wiki.haskell.org/Programming_guidelines#Naming_Conventions'
• URL `https://en.wikipedia.org/wiki/Naming_convention_(programming)'"

    (cond

     ;; « fooBar ⇒ FooBar »:

     ((string-inflection-camelcase-p word)
      (string-inflection-pascal-case-function word))

     ;; « FooBar ⇒ foo_bar »:

     ((string-inflection-pascal-case-p word)
      (string-inflection-underscore-function word))

     ;; « foo_bar ⇒ FOO_BAR »:

     ((string-inflection-underscore-p    word)
      (string-inflection-upcase-function word))

     ;; « * ⇒ fooBar »:

     (t
      (string-inflection-lower-camelcase-function word))))

  (defun sboo-string-inflection-haskell-style-cycle ()
    "`string-inflection-all-cycle' for Haskell (identifiers & constructors).

Related:

• Calls `sboo-string-inflection-haskell-style-cycle-function'"

    (interactive)
    (string-inflection-insert
     (sboo-string-inflection-haskell-style-cycle-function
      (string-inflection-get-current-word))))

  ;;--------------------------;;

  :config

  ())

;; ^ `string-inflection' converts between Variable Casings,
;;   like « under_score » ← « UPCASE » ← « CamelCase ».
;;
;; e.g. repeatedly invoking `string-inflection-all-cycle',
;;      when `point' is at “two-words”:
;;
;;   • “two-words” — 0x
;;   • “Two_Words” — once
;;   • “two_words” — twice
;;   • “TWO_WORDS” — thrice
;;   • “TwoWords”  — 4x
;;   • “twoWords”  — 5x
;;   • “two-words” — 6x (same as 0x)
;;

;; ^ Links:
;;
;;   • URL `https://github.com/akicho8/string-inflection'
;;

;;----------------------------------------------;;
;;; `rg': "Rust Grep".

(use-package rg

  :commands (rg rg-dwim rg-project rg-literal)

  :config

  (rg-enable-default-bindings)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/dajva/rg.el'
;;

;;----------------------------------------------;;

(use-package selected
  :demand t

  :commands (selected-minor-mode selected-global-mode)

  :delight (selected-minor-mode "")

  ;;--------------------------;;

  ;; NOTE! `selected' and `wrap-region':
  ;;
  ;;       many punctuation characters (in particular, bracket characters)
  ;;       are keybindings for `wrap-region' (in most Major Modes).
  ;;

  :bind (:map selected-keymap

              ;; Symbols:

           ;;   ("`" . typo-)
              ("!" . shell-command-on-region)      ; mnemonic is « M-! ».
           ;; ("@" . )
              ("#" . comment-or-uncomment-region)
           ;; ("$" . )
              ("%" . query-replace)                ; mnemonic is « M-% ».
           ;; ("^" . )
           ;; ("&" . )
           ;; ("*" . )
           ;; ("(" . )
           ;; (")" . )
              ("-" . string-inflection-all-cycle)
           ;; ("=" . )
           ;; ("[" . )
           ;; ("]" . )
              (":" . comment-or-uncomment-region)
           ;; (""" . )
           ;; ("," . )
           ;; ("." . )
           ;; ("/" . )
           ;; ("\\" . )

              ;; Letters:

              ("a" . sboo-register-append-region)
           ;; ("b" . )
              ("c" . capitalize-region)
              ("d" . downcase-region)              ; Mnemonic is “[D]owncase”.
              ("e" . sboo-edit-indirect-dwim)      ; from `edit-indirect' (via `sboo-commands').
              ("f" . fill-region)                  ; Mnemonic is “[F]ill”.
              ("F" . unfill-region)                ; ‹F› inverts ‹f›.
              ("g" . google-this-region)           ; from `google-this'.
           ;; ("h" . )
              ("i" . indent-region)                ; Mnemonic is “[I]ndent”.
           ;; ("j" . )
           ;; ("k" . )
              ("l" . align-regexp)                 ; Mnemonic is “a[L]ign”.
              ("m" . apply-macro-to-region-lines)  ; Mnemonic is “[M]acro apply”.
              ("n" . move-text-down)               ; from `move-text'. Mnemonic is “[N]ext line”, and the ‹p› key is on the top row of (of letters).
           ;; ("o" . )
              ("p" . move-text-up)                 ; from `move-text'. Mnemonic is “[P]rior line”, and the ‹n› key is on the bottom row (of letters).
              ("q" . selected-off)                 ; (from `selected' itself.)
              ("r" . query-replace-regexp)         ; Mnemonic is “[R]eplace”.
              ("s" . sort-lines)                   ; Mnemonic is “[S]ort”.
              ("S" . reverse-region)               ; Mnemonic is “rever[S]e”.
           ;; ("t" . )
              ("u" . upcase-region)                ; Mnemonic is “[U]pcase”.
              ("v" . eval-region)                  ; Mnemonic is “e[V]al”.
              ("w" . delete-trailing-whitespace)   ; Mnemonic is “[W]hitespace”.
              ("x" . cua-cut-region)               ; Mnemonic is ‹C-x›.
           ;; ("y" . )
           ;; ("z" . undo)

              ;; Non-Graphical Characters:

           ;;   ("<left>"  . sboo-extend-selection-leftward)
           ;;   ("<right>" . sboo-extend-selection-rightward)

           ;; ("<home>"  . )
           ;; ("<end>"   . )
              ("<prior>" . move-text-up)           ; from `move-text'.
              ("<next>"  . move-text-down)         ; from `move-text'.

              )

  ;;--------------------------;;

  :hook ((prog-mode . sboo-selected-mode)
         (text-mode . sboo-selected-mode)
         )

  ;;--------------------------;;

  :preface

  (defun sboo-selected-mode (&optional argument)

    "Conditional `selected-minor-mode'.

Conditions:

• buffer must be Read-Only.
• buffer must be a File-Buffer.

Inputs:

• ARGUMENT — a `booleanp' or `integerp'.
  whether to enable `selected-minor-mode' (nil or `positivep')
  or to disable it (0 or `negativep')."

    (interactive "P")

    (let* ((BUFFER-READ-ONLY-P (and buffer-file-read-only buffer-file-name))
           )

      (unless BUFFER-READ-ONLY-P
        (selected-minor-mode argument))))

  ;;--------------------------;;

  :config

  ;; (eval-after-load 'sboo-html
  ;;   (let ((MAP selected-keymap))
  ;;     (cl-dolist ((KEYSTRING . COMMAND) '((",ab" . sboo-yas-insert/element-abbrv)))
  ;;       (define-key MAP (kbd KEYSTRING) COMMAND))))
  ;; ;; ^ Mnemonic: « , » shares a key with « < », which opens an HTML Element.

  ())

;; ^ When `selected-minor-mode' is active, the keybindings in `selected-keymap'
;;   are enabled as long as the region is active (`use-region-p').
;;
;;   These conditionally-concise keybindings are useful for commands
;;   that operates on the region.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/Kungsgeten/selected.el'
;;

;;----------------------------------------------;;

(use-package wrap-region

  :commands (wrap-region-mode)

  :delight (wrap-region-mode " 🎁")

  ;;--------------------------;;

  :hook ((text-mode . wrap-region-mode)
         (prog-mode . wrap-region-mode)
         )

  ;;--------------------------;;

  :config

  (dolist (MODE '(magit-status-mode))
    (add-to-list 'wrap-region-except-modes MODE))

  ;; ^ TUIs

  (require 'sboo-html nil :no-error)

  (let* ((MARKDOWN-MODES (or (bound-and-true-p sboo-markdown-modes-list)
                             '(markdown-mode gfm-mode)))
         (HTML-MODES     (or (bound-and-true-p sboo-html-modes-list)
                             (append '(html-mode mhtml-mode) MARKDOWN-MODES)))
         (ORG-MODES      '(org-mode))
         )

    (wrap-region-add-wrappers

     `(

       ;; Ascii Characters...

       ("`" "`" nil ,(append MARKDOWN-MODES '(haskell-mode haddock-mode)))

       ;; ^ Syntax for code blocks (e.g. « `pandoc` »), in Markdown and Haddocks.
       ;;   Syntax for operators (e.g. « `fmap` »), in Haskell.

       ("‘" "’" "`" (emacs-lisp-mode))

       ;; ^ Syntax for hyperlinks, in Elisp (docstrings).
       ;;   « `...' » is the ASCII-Analogue of « ‘...’ » .

       ("*" "*" nil ,MARKDOWN-MODES)

       ;; ^ Syntax for emphasis, in Markdown.
       ;;   i.e. « * » for « <em> », and « ** » for « <strong> ».

       ("_" "_" nil ,MARKDOWN-MODES)

       ;; ^ Syntax for emphasis, in Markdown.
       ;;   i.e. « _ » for « <em> », and « __ » for « <strong> ».

       ("__" "__" "_" (haskell-mode haddock-mode))

       ;; ^ Syntax for emphasis, in Haddocks.
       ;;   i.e. « __ » for « <strong> »
       ;;   (no single-underscore, i.e. no « _ »).

       ("/" "/" nil (haskell-mode haddock-mode))

       ;; ^ Syntax for for emphasis, in Haddocks.

       ("~" "~" nil ,MARKDOWN-MODES)

       ;; ^ Syntax for strike-through, in Markdown.

       ("<" ">" "<" ,(append MARKDOWN-MODES '(haskell-mode haddock-mode)))
       ("<" ">" "," ,(append MARKDOWN-MODES '(haskell-mode haddock-mode)))

       ;; ^ Syntax for hyperlinks, in both Markdown and Haddocks.
       ;;   « , » because it shares a key with « < » (unshifted).

       ("<<" ">>" ">" (haskell-mode haddock-mode))

       ;; ^ Syntax for image hyperlinks, in Haddocks.
       ;;   « > » (the closing pair) because « < » (the opening pair) is already taken.

       ("@" "@" nil (haskell-mode haddock-mode))

       ;; ^ Syntax for code blocks, in Haddock.

       ("=" "=" nil ,ORG-MODES)
       ("+" "+" nil ,ORG-MODES)
       
       ;; ^ See `org-emphasis-alist':
       ;;
       ;;   •  « =verbatim= »
       ;;   •  « +strike-through+ »s
       ;;

       ("%" "%" nil (bat-mode))

       ;; ^ Environment-Variable syntax (e.g. « %APPDATA% »), in Batch (a.k.a. « BAT »).

       ("{-" "-}"    ":" (haskell-mode))

       ("/* " " */"  ":" (nix-mode c-mode c++-mode javascript-mode css-mode java-mode))

       ("<!--" "-->" ":" ,HTML-MODES)

       ("#|" "|#"    ":" (scheme-mode))

       ;; ^ Mode-Specific, Multi-Line Comments:
       ;;
       ;;   • « ; », i.e. the semicolon-character,
       ;;     shares a key with the colon-character (unshifted),
       ;;     is our universal trigger-key for commenting a region;
       ;;     c.f. « M-; » runs `comment-dwim' across langauges.
       ;;

       ;; Unicode Characters...

       ("“" "”" "“" ())
       ("‘" "’" "‘" ())
       ("«" "»" "«" ())
       ("‹" "›" "‹" ())
       ("「" "」" "「" ())

       ("¿" "?" "?" (text-mode))
       ("¡" "!" "!" (text-mode))

       ;; ^ Spanish-language Inverted Question/Exclamation Marks.
       ;;   URL `https://en.wikipedia.org/wiki/Inverted_question_and_exclamation_marks'

       ;;SKELETON ("" "" "" ())

       )))

  (when (bound-and-true-p sboo-html-wrap-region-table)
    (wrap-region-add-wrappers (sboo-html-wrap-region-table)))

  ;;TODO...
  ;; (when (bound-and-true-p sboo-markdown-wrap-region-table)
  ;;   (wrap-region-add-wrappers (sboo-markdown-wrap-region-table)))

  (turn-on-wrap-region-mode)

  ;; ^ `wrap-region-global-mode' is a `define-globalized-minor-mode'.

  ())

;; ^ Links
;;
;;   • URL `https://github.com/rejeep/wrap-region.el'
;;   • URL `http://pragmaticemacs.com/emacs/wrap-text-in-custom-characters/'
;;   • URL `https://www.youtube.com/watch?v=9SWAKPF0fHE'
;;   • URL `https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet'
;;   • URL `https://www.haskell.org/haddock/doc/html/ch03s08.html' 
;;   • URL `https://orgmode.org/manual/Markup.html' 
;;

;; ^ Notes about `wrap-region'...
;;
;; • `wrap-region-table', by default, holds ① quotation characters, and ② matching bracket characters:
;;
;;       '(("\"" "\"")
;;         ("'"  "'")
;;         ("("  ")")
;;         ("{"  "}")
;;         ("["  "]")
;;         ("<"  ">"))
;;
;;    and can be inspected with:
;;
;;        M-: (hash-table-keys wrap-region-table)
;;          ⇒ '(";" "`" "%" "\"" "'" "(" "{" "[" "<")
;;

;;----------------------------------------------;;

(use-package expand-region

  :delight (expand-region-mode "")

  :bind (:map sboo-mark-keymap
              ("e" . expand-region)
              )

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/magnars/expand-region.el'
;;

;;----------------------------------------------;;

(use-package sed-mode
  
  :commands (sed-mode)

  :delight (sed-mode " ")

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/emacsfodder/sed-mode'
;;   • URL `http://elpa.gnu.org/packages/sed-mode.html'
;;   • URL `https://www.gnu.org/software/sed/manual/sed.html'
;;

;;----------------------------------------------;;

(use-package edit-indirect

  :commands (edit-indirect-region)

  :delight (edit-indirect-mode)

  :custom

  (edit-indirect-guess-mode-function #'sboo-edit-indirect-guess-mode "Override `edit-indirect-default-guess-mode'.")

  ;;--------------------------;;

  :preface

  (defun sboo-edit-indirect-guess-mode (parent-buffer parent-region-begin parent-region-end)

    "Guess the major mode for an edit-indirect buffer.

Calls `set-auto-mode', which parses the « mode » file-local (special) variable 
(i.e. « -*- mode: ... -*- »).

Within `markdown-mode' (including Derived Modes like `gfm-mode'),
search (upwards) for a named Code-Block. For example, 

    \`\`\` elisp
    (list 1 t ?3)
    \`\`\`

❶ extract “elisp”, and
❷ associate it with `emacs-lisp-mode' (via `TODO')."

    (if (derived-mode-p 'markdown-mode)

        (set-auto-mode t)               ;TODO 

      (set-auto-mode t)))

  ;;--------------------------;;

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/Fanael/edit-indirect'
;;

;;----------------------------------------------;;

(use-package undo-propose

  :commands (undo-propose)

  :bind ("s-z" . undo-propose)

  :config ())

;; ^ `undo-propose' provides navigating through your undo history in a temporary buffer.
;;
;; Features include:
;;
;; • If you get lost, you can cancel the whole series of undo’s, without modifying the original buffer or undo history.
;; • You can search through your undo history for old snippets, copy and paste them back in manually, then discard the rest of the undo’s.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/jackkamm/undo-propose-el'
;;

;;----------------------------------------------;;

(use-package move-text

  :commands (move-text-up move-text-down)

  :bind (:map sboo-edit-keymap
              ("<up>"   . move-text-up)
              ("<down>" . move-text-down)
         )

  :config ())

;; ^ Links:
;;
;;   • URL `https://www.emacswiki.org/emacs/move-text.el'
;;

;;----------------------------------------------;;

(use-package typo
    :disabled t

    :commands (typo-mode)

    :delight (typo-mode)

    ;;:hook (text-mode . typo-mode)

    :config

    (modify-syntax-entry ?\» "(«")
    (modify-syntax-entry ?\« ")»")

    ;;------------------------;;

    (defun sboo-typo-cycle ()
      "`typo-mode' at `point'."
      (interactive)

      (let* ((CHAR-AT-POINT (point))
             (ASCII-CHAR    (identity CHAR-AT-POINT))
             )

        (pcase ASCII-CHAR

          (?\"  (typo-insert-quotation-mark))
          (?\'  (typo-cycle-right-single-quotation-mark))
          (?\-  (typo-cycle-dashes))
          (?\.  (typo-cycle-ellipsis))
          (?\<  (typo-cycle-left-angle-brackets))
          (?\>  (typo-cycle-right-angle-brackets))
          (?\`  (typo-cycle-left-single-quotation-mark))
          (_    ))))

    ;;------------------------;;

    ;; (typo-global-mode +1)

    ;;------------------------;;

    ())

;; ^ `typo-mode' binds keys of *printable characters* (e.g. « e » or « - ») to insert related *typographic unicode characters" (e.g. « é » or « — »).
;; 
;; its keybindings include:
;; 
;; • « . » is « . » (single-press) or « … » (triple-press).
;; • « - » is « - » (single-press) or « – » (double-press) or « — » (triple-press).
;; • « ' » is « ' » or « ‘ » or « ’ ».
;; • « " » is « " » or « “ » or « ” ».
;; 

;; ^ Links:
;;
;;   • URL `https://github.com/jorgenschaefer/typoel'
;;

;;----------------------------------------------;;

(use-package sort-words

  :commands (sort-words)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/dotemacs/sort-words.el'
;;

;;----------------------------------------------;;

(use-package string-edit

  :bind ("C-c C-'" . string-edit-at-point)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/magnars/string-edit.el'
;;

;;----------------------------------------------;;

(use-package elisp-docstring-mode

  :commands (elisp-docstring-mode)

  :config

  ;;TODO sboo-elisp-docstring-edit-indirect

  ())

;; `major-mode' for editing Emacs Lisp Docstrings.
;;
;; Provides:
;;
;; • Highlighting — via `font-lock'.
;; • Escaping — of quotes and backslashes.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/Fuco1/elisp-docstring-mode'
;;

;;----------------------------------------------;;

(use-package edit-var

  :commands (edit-variable) 

  :bind ("C-c e v" . edit-variable)

  :config ())

;;----------------------------------------------;;

(use-package edit-rectangle

  :bind ("C-x r e" . edit-rectangle)

  :config ())

;;----------------------------------------------;;

(use-package operate-on-number

  :bind (:map sboo-edit-keymap
              ("n" . operate-on-number-at-point)
              )

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/knu/operate-on-number.el'
;;

;;----------------------------------------------;;
;;; External Packages: Navigation --------------;;
;;----------------------------------------------;;

(use-package back-button
  :disabled

  :commands (back-button-mode)

  :custom

  (back-button-show-toolbar-buttons t)

  :config

  (back-button-mode +1))

;; ^ `back-button' usage:
;;
;;   • press the « Push Mark » (a.k.a. plus sign) Button in the toolbar to create a mark.
;;   • press the « Back/Forward by Mark » (a.k.a. left/right arrow) Button in the toolbar to navigate marks.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/rolandwalker/back-button'
;;

;;----------------------------------------------;;

(use-package avy)

;; ^ Links:
;;
;;   • URL `https://github.com/abo-abo/avy'
;;

;;----------------------------------------------;;

(use-package goto-line-preview

  :commands (goto-line-preview)

  :config

  (global-set-key [remap goto-line] #'goto-line-preview)

  ())

;; Inspired by Visual Studio Code, by « goto line »'s preset behavior.

;; ^ Links:
;;
;;   • URL `https://github.com/elpa-host/goto-line-preview'
;;

;;----------------------------------------------;;

(use-package dumb-jump

  :hook ((haskell-mode) . dumb-jump-mode)

  :bind (("s-g p" . dumb-jump-go)
         ("s-g P" . dumb-jump-go-prompt)
         )

  :custom

  (dumb-jump-selector        'helm
                             "disambiguate Multiple Candidates via `helm' (not `popup').")
  (dumb-jump-default-project "~/haskell"
                             "the Default Project if none is found (defaults to « ~» ).")

  :config

  ())

;; “Dumb Jump uses The Silver Searcher (program `ag'), ripgrep (program `rg'), or program `grep' to find potential definitions of a function or variable under `point'.”

;; ^ Links:
;;
;;   • URL `https://github.com/jacktasia/dumb-jump'
;;

;;----------------------------------------------;;

(use-package smartscan
  :defer 5

  :commands (smartscan-mode
             global-smartscan-mode
             smartscan-symbol-go-backward
             smartscan-symbol-go-forward
             smartscan-symbol-replace
             )

  :bind (("M-p" . smartscan-symbol-go-backward)
         ("M-n" . smartscan-symbol-go-forward)
         :map smartscan-map
         )

  :config

  (global-smartscan-mode +1))

;; ^ Bind « M-n » and « M-p » to look for `symbol-at-point'.
;;

;; ^ Links:
;;
;;   • URL `https://github.com/mickeynp/smart-scan'
;;   • URL `https://www.masteringemacs.org/article/effective-editing-movement' 
;;   • URL `https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el'
;;

;;----------------------------------------------;;

(use-package goto-last-change

  :load-path "sboo/lisp"

  :commands (goto-last-change)

  ;;--------------------------;;

  :bind (("<C-left>"  . goto-last-change)
         ;; ("<C-right>" . sboo-goto-last-change)
         )

  ;;--------------------------;;

  :config

  (defun sboo-goto-last-change ()
    "Invert `goto-last-change'."        ;TODO
    (interactive "P")
    (goto-last-change -1))

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/camdez/goto-last-change.el'
;;

;;----------------------------------------------;;

(use-package imenu-list

  :commands (imenu-list-minor-mode imenu-list-smart-toggle)

  :bind (:map sboo-navigate-keymap
              ("`" . imenu-list-smart-toggle) ; opens the `imenu-list-major-mode' window or closes it.
              )

  :custom

  (imenu-list-position               'left
                                     "where the ‘imenu-list-major-mode’ window opens; either ‘left’ or ‘right’ maximize vertical space.")
  (imenu-list-focus-after-activation t
                                     "automatically focus on the ‘imenu-list-major-mode’ window.")
  (imenu-list-auto-resize            nil
                                     "don't autoresize.")

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/bmag/imenu-list'
;;

;;----------------------------------------------;;

(use-package link-hint

    :after (avy)

    :bind (("s-n l o" . link-hint-open-link)
           ("s-n l c" . link-hint-copy-link)
           )

    :config

    (add-hook 'eww-mode-hook (lambda ()
                               (bind-key "f" #'link-hint-open-link eww-mode-map)))
    (add-hook 'w3m-mode-hook (lambda ()
                               (bind-key "f" #'link-hint-open-link w3m-mode-map)))

    ())

;; ^ Links:
;;
;;   • URL `https://github.com/noctuid/link-hint.el'
;;

;;----------------------------------------------;;

(use-package google-this

  :commands (google-this google-this-search google-this-word google-this-symbol google-this-line google-this-region google-this-noconfirm)

  ;; ^ with Prefix Argument, any `google-*' command is wrapped in quotes
  ;;  (see `google-wrap-in-quotes').

  :delight (google-this-mode)

  :bind (:map google-this-mode-submap
              ("g" . google-this)
              ;; ^ e.g. press « s-s g g »
         )

  :custom

  (google-this-browse-url-function #'sboo-browse-uri-chrome "open with Google Chrome.")

  ;; ^ Options: `browse-url', `browse-url-generic',`browse-url-emacs', `eww-browse-url'."

  :config

  (define-key sboo-search-keymap (kbd "g") #'google-this-mode-submap)

  (google-this-mode +1))

;; ^ `google-this' provides commands for googling Text Objects.
;;
;; Features include:
;;

;; ^ Links:
;;
;;   • URL `https://github.com/Malabarba/emacs-google-this'
;;

;;----------------------------------------------;;

(use-package google-translate
  :disabled t

  :init
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "gr")

  :config ())

;;----------------------------------------------;;

(use-package xah-lookup

  :commands (xah-lookup-word-on-internet
             xah-lookup-web
             xah-lookup-wikipedia
             xah-lookup-word-definition
             xah-lookup-word-dict-org
             xah-lookup-wiktionary
             xah-lookup-etymology
             )

  :init

  (setq xah-lookup-browser-function #'eww) 

  ;; ^ `xah-lookup-browser-function' must be set *before* loading `xah-lookup'.

  :config ())

;; ^ Xah Lookup provides “lookup commands” via:
;;
;; • Google
;; • Wikipedia
;; • Dictionaries
;; • programing reference sites
;;

;; ^ Links:
;;
;;   • URL `https://github.com/xahlee/lookup-word-on-internet'
;;   • URL `http://ergoemacs.org/emacs/xah-lookup.html'
;;

;;----------------------------------------------;;

(use-package engine-mode

  :commands (engine-mode)

  :delight (engine-mode)

  :bind  (:map sboo-search-keymap
               ("/" . engine-mode-prefixed-map)
          )

  :config

  (defengine google "https://www.google.com/search?q=%s"
    :browser    #'sboo-browse-uri-chrome
    :keybinding "/")

  ;; ^ e.g. press « s-s / / » for `engine/search-google'.

  (engine-mode t))

;; ^ Links:
;;
;;   • URL `https://github.com/hrs/engine-mode'
;;

;;----------------------------------------------;;

(use-package outshine
  :disabled t)

;; ^ Links:
;;
;;   • URL `https://github.com/alphapapa/outshine'
;;

;;----------------------------------------------;;

(use-package navi

  :commands (navi-mode)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/alphapapa/navi'
;;

;;----------------------------------------------;;
;; External Packages: Prose --------------------;;
;;----------------------------------------------;;

(use-package wc-mode

  :commands (wc-mode)

  :bind ("C-c \"" . wc-mode)

  :config ())

;; ^ “wc-mode” abbreviates “[W]ord-[C]ount [MODE]”
;;
;; Configuration:
;;
;; ** Modline string
;;
;;    The default string displayed in the mode line can be changed to
;;    suit your needs. It can be defined through the variable
;;    customizaton interface.
;;
;;    The setting itself is simply a string with a few special characters
;;    to represent the available statistics. These character strings are
;;    listed in the follow table.
;;
;;    | Format String | Meaning                      |
;;    |---------------+------------------------------|
;;    | %C            | Original character count     |
;;    | %W            | Original word count          |
;;    | %L            | Original line count          |
;;    | %c            | Change (delta) in characters |
;;    | %w            | Change (delta) in words      |
;;    | %l            | change (delta) in lines      |
;;    | %gc           | Character change goal        |
;;    | %gw           | Word change goal             |
;;    | %gl           | Line change goal             |
;;    | %tc           | Total number of characters   |
;;    | %tw           | Total number of words        |
;;    | %tl           | Total number of lines        |
;;
;; 

;; ^ Links:
;;
;;   • URL `https://www.emacswiki.org/emacs/WordCount'
;;   • URL `http://bnbeckwith.com/code/word-count-mode.html'
;;

;;----------------------------------------------;;
;;; External Packages: Filesystem --------------;;
;;----------------------------------------------;;

(use-package peep-dired

  :custom

  (peep-dired-ignored-extensions '("mkv" "iso" "mp4") "don't preview binary files.")

  :bind (:map peep-dired-mode-map 
         ("SPC" . nil)
         ("<backspace>" . nil))

  :config

  ())

;; ^ URL `https://github.com/asok/peep-dired'
;;
;;

;;----------------------------------------------;;

(use-package neotree
  :disabled

  :config ())

;; ^  URL `https://github.com/jaypei/emacs-neotree'

;;----------------------------------------------;;
;;; External Packages: Web ---------------------;;
;;----------------------------------------------;;

(use-package w3m
  :disabled

  :commands (w3m-browse-url w3m-find-file)

  :config ())

;; ^ 

;; ^ Links:
;;
;;   • URL `https://github.com/emacs-w3m/emacs-w3m'
;;

;;----------------------------------------------;;
;;; External Packages: Highlighting ------------;;
;;----------------------------------------------;;

(use-package volatile-highlights

  :commands (volatile-highlights-mode)

  :delight (volatile-highlights-mode " ⚡")

  :config

  (volatile-highlights-mode +1)

  ())

;; ^ `volatile-highlights' temporarily-highlights changes
;;    (e.g. from `yank'ing (and pasting), `insert'ing, `undo'ing, etc).

;; ^ Links:
;;
;;   • URL `https://github.com/k-talo/volatile-highlights.el'
;;   • URL `http://pragmaticemacs.com/emacs/volatile-highlights/'
;;

;;----------------------------------------------;;

(use-package rainbow-mode

  :commands (rainbow-mode)

  :delight (rainbow-mode " 🌈")

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://jblevins.org/log/rainbow-mode'
;;

;;----------------------------------------------;;

(use-package rainbow-delimiters

  :commands (rainbow-delimiters-mode)

  :delight (rainbow-delimiters-mode " 🌈")

  ;;TODO:
  ;; :custom-face

  ;; (rainbow-delimiters-max-face-count 6 "fewer faces, higher-contrast.")

  ;; (rainbow-delimiters-depth-1-face )
  ;; (rainbow-delimiters-depth-2-face )
  ;; (rainbow-delimiters-depth-3-face )
  ;; (rainbow-delimiters-depth-4-face )
  ;; (rainbow-delimiters-depth-5-face )
  ;; (rainbow-delimiters-depth-6-face )

  ;; (rainbow-delimiters-unmatched-face )
  ;; (rainbow-delimiters-mismatched-face )

  :config

  (let* ((LISP-HOOKS
          (if (require 'sboo-lisp nil :no-error)
              sboo-lisp-hooks
            '(emacs-lisp-mode-hook)))
         )

    (dolist (HOOK LISP-HOOKS)
      (add-hook HOOK #'rainbow-delimiters-mode)))

  ())

;; ^ URL `https://github.com/Fanael/rainbow-delimiters'

;;----------------------------------------------;;

(use-package rainbow-identifiers

  :commands (rainbow-identifiers-mode)

  :delight (rainbow-identifiers-mode " 🌈")

  :hook (prog-mode . rainbow-identifiers-mode)

  :config

  ())

;; ^ URL `https://github.com/Fanael/rainbow-identifiers'

;;----------------------------------------------;;

(use-package rainbow-blocks

  :commands (rainbow-blocks-mode)

  :delight (rainbow-blocks-mode " 🌈")

  :delight

  ;; :hook (prog-mode text-mode)

  :config

  ())

;; ^ URL `https://github.com/istib/rainbow-blocks'

;;----------------------------------------------;;

(use-package highlight-numbers

  :commands (highlight-numbers-mode)

  :delight (highlight-numbers-mode " 🗱")

  :hook (prog-mode . highlight-numbers-mode)

  :config

  ())

;; ^ `highlight-numbers': highlight numbers (in any language).

;; ^ URL `https://github.com/Fanael/highlight-numbers'

;;----------------------------------------------;;

(use-package highlight-quoted

  :commands (highlight-quoted-mode)

  :delight (highlight-quoted-mode " 🗱")

  :hook (prog-mode . highlight-quoted-mode)

  :config

  ())

;; ^ `highlight-quoted': highlight *Lisp Symbols* (e.g. `'foo`).

;; ^ URL `https://github.com/Fanael/highlight-quoted'

;;----------------------------------------------;;

(use-package highlight-defined

  :commands (highlight-defined-mode)

  :delight (highlight-defined-mode " 🗱")

;;:hook (emacs-lisp-mode . highlight-defined-mode)

  :config

  ())

;; ^ `highlight-defined': highlight DEFINED *Elisp Symbols*.
;;   Thus, contrasting UNDEFINED symbols.

;; ^ URL `https://github.com/Fanael/highlight-defined'

;;----------------------------------------------;;

(use-package highlight-escape-sequences

  :commands (highlight-escape-sequences-mode)

  :delight (highlight-escape-sequences-mode " 🗱")

  :hook (prog-mode . turn-on-hes-mode)

  ;;:custom (hes-mode-alist (append hes-mode-alist) "register Haskell Escape-Sequences.")  ;TODO

  :config

  ())

;; ^ URL `https://github.com/dgutov/highlight-escape-sequences'

;; ^ `highlight-escape-sequences': highlight *Escape Sequences* (e.g. `"\n"`).

;;----------------------------------------------;;

(use-package highlight-blocks

  :commands (highlight-blocks-mode)

  :delight (highlight-blocks-mode " 🗱")

  :config

  (let* ((LISP-HOOKS
          (if (require 'sboo-lisp nil :no-error)
              sboo-lisp-hooks
            '(emacs-lisp-mode-hook)))
         )

    (dolist (HOOK LISP-HOOKS)
      (add-hook HOOK #'highlight-blocks-mode)))

  ())

;; ^ `highlight-blocks': highlights *block* at-`point' (i.e. innermost parenthetical grouping(s)).

;; ^ URL `https://github.com/Fanael/highlight-blocks'

;;----------------------------------------------;;

(use-package highlight-cl

  :commands (highlight-cl-add-font-lock-keywords)

  :hook ((emacs-lisp-mode . highlight-cl-add-font-lock-keywords)
         )

  :config ())

;; ^ `highlight-cl': highlights `cl-*' (i.e. Common Lisp) symbols.

;; ^ Links:
;;
;;   • URL `https://github.com/emacsmirror/highlight-cl'
;;

;;----------------------------------------------;;
;;; External Packages: Windows/Buffers ---------;;
;;----------------------------------------------;;

(use-package centaur-tabs
    :disabled
l    
    :demand t

    ;;------------------------;;

    :commands (centaur-tabs-mode)
    
    :bind  (("C-<kp-left>"  . centaur-tabs-backward)
            ("C-<kp-right>" . centaur-tabs-forward))

    ;;------------------------;;

    :custom

    (centaur-tabs-style "bar"
      "")

    (centaur-tabs-cycle-scope 'groups
      "‘centaur-tabs-forward’ cycles through Tab Groups, not Visible Tags.")

    (centaur-tabs-common-group-name "?"
      "default name for Tab Groups.")

    (centaur-tabs-height 22
      "Height of each Tab.")

    (centaur-tabs-bar-height (+ 6 22)
      "Height of the Tab-bar.")

    (centaur-tabs-set-close-button t
      "enable Close-Buttons on each tab.")

    (centaur-tabs-close-button (string ?❌)
      "each Close-Button is « ❌ » (a Unicode Cross-Mark).")

    ;;------------------------;;

    :preface

    (defvar sboo-disable/centaur-tabs nil
      "Gates `centaur-tabs-mode' (a `booleanp').")

    (defun sboo-centaur-tabs-toggle (&optional force)
      "Toggle `centaur-tabs-mode', unless `sboo-disable/centaur-tabs'."
      (interactive "P")

      (let ((FORCE (and (not (null force))
                        (> (prefix-numeric-value force) 0))))
        (if FORCE
            (centaur-tabs-mode FORCE)
          (unless (or (bound-and-true-p centaur-tabs-mode)
                      (bound-and-true-p sboo-disable/centaur-tabs))
            (centaur-tabs-mode +1)))))

    (defun sboo-centaur-tabs-reset-background-color (&optional theme)
      "Set `centaur-tabs-background-color' for THEME, unless `sboo-disable/centaur-tabs'."

      (when (featurep 'centaur-tabs)
        (setq centaur-tabs-background-color (face-background 'default))))

    ;;------------------------;;

    :config

    ;; Launch the Tab-bar, later:

    (add-hook 'emacs-startup-hook #'sboo-centaur-tabs-toggle)

    ;; Reset the Tab-bar Color, whenever the Current Theme is changed via `sboo-load-theme':

    (add-hook 'sboo-theme-post-init-hook #'sboo-centaur-tabs-reset-background-color)

    ())

;; ^ `centaur-tabs' provides a Tab-bar.
;;
;; forked off `awesome-tab`.
;;
;;

;; ^ `centaur-tabs-style' defaults to "bar", and can be:
;;
;; - "alternate"
;; - "bar"
;; - "box"
;; - "chamfer"
;; - "rounded"
;; - "slant"
;; - "wave"
;; - "zigzag"
;; 

;; ^ Links:
;;
;; URL `https://github.com/ema2159/centaur-tabs'

;;----------------------------------------------;;

(use-package awesome-tab
    ;;:disabled

    :demand t

    ;;------------------------;;

    :commands (awesome-tab-mode)

    :custom

    (awesome-tab-style "alternate" "Rectilinear Tabs (default are Rounded).")

    (awesome-tab-label-fixed-length 14 "FixedWidth Tabs: all Tab Labels share the same length (unit is number-of-characters).")

    ;;(awesome-tab-buffer-groups-function #' "")

    (awesome-tab-background-color "#fdf6e3")  ; the Background-Color of Solarized-Theme.
    ;;(awesome-tab-selected         "#fdf6e3")
    ;;(awesome-tab-unselected       "#fdf6e3")

    :config

    ;; Launch the Tab-bar, later:

    (add-hook 'emacs-startup-hook #'awesome-tab-mode)

    ())

;; ^ Links:
;;
;;   • URL `https://github.com/manateelazycat/awesome-tab'
;;

;;TODO;; (add-to-list helm-source-list awesome-tab-build-helm-source)

;;----------------------------------------------;;
;;; External Packages: Terminal ----------------;;
;;----------------------------------------------;;

(use-package shell-pop
    :disabled

    ;;--------------------------;;

    :commands (shell-pop)

    ;;--------------------------;;

    :bind (("s-x s" . shell-pop)
           )

    ;;--------------------------;;

    :custom

    (shell-pop-term-shell "/bin/bash" "Bash")
    (shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (sboo-shell-pop-launch))) "use `ansi-term' (not `shell').")

    ;; ^ NOTE `shell-pop' doesn't handle function-symbols correctly (i.e. « #'sboo-shell-pop-launch »).

    ;;--------------------------;;

    :preface

    (defun sboo-shell-pop-launch ()
      "Launch `ansi-term' for function `shell-pop'."
      (ansi-term shell-pop-term-shell))

    ;;--------------------------;;

    :config

    (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)

    ())

;; ^ Links:
;;
;;   • URL `http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/'
;;   • URL `https://github.com/kyagi/shell-pop-el'
;;

;;----------------------------------------------;;
;;; External Packages: Appearence --------------;;
;;----------------------------------------------;;

(use-package diredfl

  :commands (diredfl-global-mode)

  :hook (dired-mode . diredfl-mode)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/purcell/diredfl'
;;

;;----------------------------------------------;;

(use-package telephone-line
  :disabled t

  :custom

  (telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))

  (telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

  :config

  (telephone-line-mode -1)
  ;; ^ Activation (i.e. « (`*-mode' +1) ») must follow Initialization (i.e. « (`setq' *-* ...) »).

  ())

;;----------------------------------------------;;

(use-package unicode-fonts
  :disabled
  :load-path "submodules/unicode-fonts"

  :config

  (unicode-fonts-setup)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/rolandwalker/unicode-fonts'
;;

;;----------------------------------------------;;

(use-package all-the-icons
    :disabled t
    :load-path "submodules/all-the-icons"

    :config ())

;; Installation:
;;
;; M-: (all-the-icons-install-fonts)

;; Usage:
;;
;; M-: (insert (all-the-icons-icon-for-file "foo.hs"))
;; M-: (all-the-icons-insert-icons-for 'faicon 10 0.5)   ; height=10px, delay=500ms.
;;

;; Troubleshooting:
;;
;; URL `https://github.com/domtronn/all-the-icons.el/blob/master/README.md#troubleshooting'
;;
;; M-: (set-fontset-font t 'unicode (font-spec :name "Symbola") nil 'append)
;;
;; “Symbola is messing with your fontset, since it's a universal fallback font.”
;;
;; M-: (insert (all-the-icons-wicon "tornado")
;;
;; PROBLEM segfaults.
;; SOLUTION? (nope)
;;
;;     $ ulimit -S -s unlimited; make emacs-run &
;;
;;
;;
;;

;; Notes:
;;
;; e.g. fontsets: 'faicon 'octicon 'alltheicon

;;----------------------------------------------;;

(use-package all-the-icons-dired
    :disabled t
    :load-path "submodules/all-the-icons-dired"

    :after (:any all-the-icons icons-in-terminal)

    ;;--------------------------;;

    :commands (all-the-icons-dired-mode)

    :hook (dired-mode . all-the-icons-dired-mode)

    :config

    ())

;; ^ Links:
;;
;;   • URL `https://github.com/jtbm37/all-the-icons-dired'
;;

;;----------------------------------------------;;

(use-package doom-modeline
  :disabled
  :load-path "submodules/doom-modeline"

  :hook (after-init . doom-modeline-mode)

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/seagle0128/doom-modeline'
;;

;;----------------------------------------------;;
;;; External Packages: Clipboard ---------------;;
;;----------------------------------------------;;

(use-package simpleclip
  :disabled                             ;FIXME

  :config

  (simpleclip-mode +1)

  ())

;; ^ Links:
;;
;; ^ URL `https://github.com/rolandwalker/simpleclip'
;;

;;----------------------------------------------;;

(use-package highlight2clipboard :defer t)

;; ^ Links:
;;
;;   • URL `https://github.com/Lindydancer/highlight2clipboard'
;;

;;----------------------------------------------;;
;;; External Packages: Media -------------------;;
;;----------------------------------------------;;

(use-package eimp

  :if window-system

  :commands (eimp-mode)

  :hook
  (image-mode eimp-mode)

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://www.emacswiki.org/emacs/eimp.el'
;;

;;----------------------------------------------;;

(use-package pdf-tools
  :disabled
  :if window-system
;;:load-path "site-lisp/pdf-tools/lisp"

  :commands (pdf-view-mode)

  :magic ("%PDF" . pdf-view-mode)

  :config

  (dolist (PACKAGE '(pdf-annot
                     pdf-cache
                     pdf-dev
                     pdf-history
                     pdf-info
                     pdf-isearch
                     pdf-links
                     pdf-misc
                     pdf-occur
                     pdf-outline
                     pdf-sync
                     pdf-util
                     pdf-view
                     pdf-virtual
                     ))
    (let ((debug-on-error nil))
      (with-demoted-errors "[Error ‘pdf-tools’] %S"
        (require PACKAGE))))

  (pdf-tools-install))

;; ^ Links:
;;
;;   • URL `https://github.com/politza/pdf-tools'
;;

;;----------------------------------------------;;

(use-package vlf

  :commands (vlf)

  :preface

  (defun sboo-vlf-ffap (&optional file)
    "Find a large file at `point' or FILE (with `vlf')."
    (interactive "fFile (large): ")
    (let* ((FILE (or file (ffap-file-at-point)))
           )
      (unless (file-exists-p FILE)
        (error "File does not exist: %s" FILE))
      (vlf FILE)))

  :config

  (require 'vlf-setup)

  ())

;; ^ « vlf » abbreviates « View Large Files ».
;;

;; ^ Links:
;;
;;   • URL `https://github.com/m00natic/vlfi'
;;

;;----------------------------------------------;;
;;; External Packages: Spell-Checking:
;;----------------------------------------------;;

(use-package synosaurus

  :commands (synosaurus-lookup synosaurus-choose-and-replace synosaurus-choose-and-insert)

  :custom

  (synosaurus-choose-method 'popup "choose alternatives via ‘popup.el’.")
;;(synosaurus-backend #'synosaurus-backend-wordnet "program ‘wn’ works offline".)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/hpdeifel/synosaurus'
;;

;;----------------------------------------------;;
;;; External Packages: Help --------------------;;
;;----------------------------------------------;;

;; (use-package helpful

;;   :config

;;   ())

;; ^ URL `https://github.com/Wilfred/helpful'

;;----------------------------------------------;;

(use-package which-key

  :delight (which-key-mode)

  :custom

  (which-key-idle-delay           1.000 "after « 1s » (one second) of pressing an unfinished keysequence, show the documentation of the sub-keys available in the keysequence.")
  (which-key-idle-secondary-delay 0.250 "")

  :config

  (which-key-setup-side-window-right-bottom)
  (which-key-mode +1)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/justbur/emacs-which-key'
;;

;;----------------------------------------------;;

(use-package know-your-http-well

  :commands (http-header
             http-method
             http-relation
             http-status-code
             media-type)

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/for-GET/know-your-http-well'
;;

;;----------------------------------------------;;

(use-package x86-lookup

  :bind ("C-h X" . x86-lookup)

  :config ())

;;TODO
;; pdftotext command line program from Poppler. On Linux, this program is probably already installed.
;; Intel 64 and IA-32 Architecture Software Developer Manual. Any PDF that contains the full instruction set reference will work, though volume 2 is the best choice for x86-lookup.

;; ^ Links:
;;
;;   • URL `https://github.com/skeeto/x86-lookup'
;;

;;----------------------------------------------;;
;;; External Packages: Miscellaneous -----------;;
;;----------------------------------------------;;

(use-package htmlize

  :commands (htmlize-region htmlize-buffer htmlize-file)

  :config ())

;; ^ `htmlize-buffer':
;;
;; “To use it, just switch to the buffer you want HTML-ized and type M-x htmlize-buffer. You will be switched to a new buffer that contains the resulting HTML code. You can edit and inspect this buffer, or you can just save it with C-x C-w. M-x htmlize-file will find a file, fontify it, and save the HTML version in FILE.html, without any additional intervention.”
;;

;; ^ Links:
;;
;;   • URL `https://github.com/hniksic/emacs-htmlize'
;;

;;----------------------------------------------;;

(use-package refine

  :commands (refine)

  :config ())

;; ^ `refine' — a TUI for editing Elisp variables
;;   (particularly Lists, like Hooks).
;;
;; e.g. call `refine' on `prog-mode-hook':
;;
;;      M-: (refine 'prog-mode-hook)
;;
;;

;; ^ Links:
;;
;;   • URL `https://github.com/Wilfred/refine'
;;

;;----------------------------------------------;;

(use-package desktop-environment

  :commands (desktop-environment-toggle-mute
             desktop-environment-toggle-microphone-mute
             desktop-environment-screenshot-part
             desktop-environment-volume-decrement
             desktop-environment-volume-increment
             )

  :init

  ;; See « desktop-environment.el »

  ())

;; ^ the `desktop-environment' `featurep'
;;   provides `desktop-environment-keyboard-backlight-set'.
;;   e.g.:
;;
;;       (defun desktop-environment-keyboard-backlight-set (value)
;; "Set keyboard backlight to VALUE."
;; (dbus-call-method :system
;;                   "org.freedesktop.UPower"
;;                   "/org/freedesktop/UPower/KbdBacklight"
;;                   "org.freedesktop.UPower.KbdBacklight"
;;                   "SetBrightness"
;;                   :int32 value)
;; (message "New keyboard value: %s%%" (desktop-environment-keyboard-backlight-percent)))

;;----------------------------------------------;;

(use-package bm

  :commands (bm-toggle bm-next bm-previous)

  :custom

  (bm-buffer-persistence t "Do save bookmarks (i.e. enable buffer persistence).")

  :config

  (when (display-graphic-p) ; Add fringe only if display is graphic (GUI)
    (define-fringe-bitmap 'bm-marker-left [#xF8    ; ▮ ▮ ▮ ▮ ▮ 0 0 0
                                           #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                           #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                           #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
                                           #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
                                           #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                           #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                           #xF8])) ; ▮ ▮ ▮ ▮ ▮ 0 0 0

  ())

;; ^ `bm' (a `featurep').
;;
;; > Quickly save and restore point using registers
;; 
;;
;; `bmkp' a.k.a bookmark+

;; ^ Links:
;;
;;   • URL `https://github.com/joodland/bm'
;;

;;----------------------------------------------;;

(use-package deft

  :commands (deft deft-new-file deft-new-file-named)

  :bind (:map sboo-launch-keymap
              ("," . deft)
              )

  :config ())

;; ^ Deft is an Emacs mode for quickly browsing, filtering, and editing
;;   directories of plain text notes, inspired by Notational Velocity.

;; ^ Links:
;;
;;   • URL `http://jblevins.org/projects/deft'
;;   • URL `https://github.com/jrblevin/deft'
;;

;;----------------------------------------------;;

(use-package olivetti

  :commands (olivetti-mode)

  ;; :custom (olivetti- t "")

  :config ())

;; ^ Links:
;;
;;   • URL `https://github.com/rnkn/olivetti'
;;

;;----------------------------------------------;;

;; ;; Major Mode for Git Commits.
;; (use-package git-commit)

;;----------------------------------------------;;

;; (use-package web-mode
;;   :mode (("\\.mustache\\'" . web-mode))
;;   ()))

;;----------------------------------------------;;

;; ;; “It's useful to be able to restart emacs from inside emacs.”
;; (use-package restart-emacs)

;;----------------------------------------------;;

;; (use-package mmm-mode
;;   :commands (mmm-mode)
;;   :config ())

;; ^ "mmm" abbreviates "[M]ultiple [M]ajor [M]odes".

;; ^ Links:
;;
;;   • URL `https://github.com/purcell/mmm-mode'
;;   • URL `'
;;

;;----------------------------------------------;;

;; (use-package better-registers

;;   :bind
;;   (:map global-map ("M-j" . better-registers-jump-to-register))

;;   :init
;;   (setq-default better-registers-use-C-r nil)

;;   :config
;;   (rr/expose-bindings better-registers-map '("<f1>" "C-j" "C-x r"))

;;   ())

;; ^ Links:
;;
;;   • URL `https://www.emacswiki.org/emacs/BetterRegisters'
;;

;;----------------------------------------------;;
;;; Personal Packages --------------------------;;
;;----------------------------------------------;;

;; (when (require 'sboo-mtg nil :no-error)

;;   ;;--------------------------;;

;;   (use-package mtg

;;     :config ())

;;   ;;--------------------------;;

;;   (use-package mtg-company

;;     :hook ((text-mode . mtg-company-setup)
;;            (markdown-mode . mtg-company-setup)
;;            )

;;     :config ())

;;   ;;--------------------------;;

;;   (use-package mtg-helm

;;     :commands (mtg-helm-card-name)

;;     :config ())

;;   ;;--------------------------;;

;;   ())

;;----------------------------------------------;;
;;; Conditional Configuration ------------------;;
;;----------------------------------------------;;

(when (load "sboo-os" :no-error)

  (add-hook 'window-setup-hook #'sboo-maximize-frame t)

  ())

;; ^ NOTE:
;;
;;  • `window-setup-hook' is similar to ‘emacs-startup-hook’.
;;

;;----------------------------------------------;;

(when (load "sboo-ui" :no-error)

  ())

;;----------------------------------------------;;

;; (use-package edit-server
;;   :if (and window-system (not (sboo-emacs-secondary-p)))
;;   :init
;;   (add-hook 'after-init-hook 'server-start t)
;;   (add-hook 'after-init-hook 'edit-server-start t)
;;   ())

;;----------------------------------------------;;

;; TODO https://www.emacswiki.org/emacs/SpreadSheet

;;----------------------------------------------;;
;;; Finalization -------------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-fonts nil :no-error)
  (sboo-fonts-config!))

;;----------------------------------------------;;
;;; Notes --------------------------------------;;
;;----------------------------------------------;;

;; `wgrep' notes
;;
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

;; `dbus-call-method':
;;
;; 

;;----------------------------------------------;;

;; `wrap-region'...
;;
;; • `wrap-region-table'
;;
;; Default `wrap-region-table':
;;
;;      '(("\"" "\"")
;;       ("'"  "'")
;;       ("("  ")")
;;       ("{"  "}")
;;       ("["  "]")
;;       ("<"  ">"))
;;
;; 
;;
;; • `wrap-region-add-wrappers':
;;
;; e.g.:
;;
;;   (wrap-region-add-wrappers
;;    '(("$" "$")
;;      ("{-" "-}" "#")
;;      ("/" "/" nil ruby-mode)
;;      ("/* " " */" "#" (java-mode javascript-mode css-mode))
;;      ("`" "`" nil (markdown-mode ruby-mode))))
;;
;; `wrap-region-add-wrappers' extends `wrap-region-table'.
;;
;; `wrap-region-add-wrappers' calls `wrap-region-add-wrapper'.
;;
;; 
;;
;; • `wrap-region-add-wrapper':
;;
;;   (wrap-region-add-wrapper LEFT RIGHT &optional KEY MODE-OR-MODES)
;;
;; >Add new LEFT and RIGHT wrapper.
;; >
;; >Optional KEY is the trigger key and MODE-OR-MODES is a single
;; mode or multiple modes that the wrapper should trigger in.
;;
;; 

;;----------------------------------------------;;

;; `awesome-tab'...
;;
;; GroupRules:
;;
;; • `awesome-tab-buffer-groups-function' defaults to function `awesome-tab-buffer-groups'.
;;
;; • 

;;----------------------------------------------;;

;; People's « init.el »s (which I've copied from or been inspired by):
;;
;; • URL `https://github.com/jwiegley/dot-emacs/blob/master/init.el'
;; • URL `https://github.com/DamienCassou/emacs.d/blob/master/init.el'
;; • URL `https://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Snippets.html'
;; • URL `'
;; • URL `'
;; • URL `'
;; • URL `'
;; • URL `https://www.draketo.de/light/english/emacs/babcore'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

;;; sboo-init.el ends here

;; Local Variables:
;; End:
