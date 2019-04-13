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

(require 'cl-lib)
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
;; Settings (early) ----------------------------;;
;;----------------------------------------------;;

(setq tool-bar-style 'both)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defmacro sboo-append-to-list! (variable list)

  "Append (the value) LIST to (the variable) VARIABLE."

  `(setq ,variable (append ,list ,variable)))

;;----------------------------------------------;;

(eval-when-compile

  ;;--------------------------;;

  (defmacro sboo-custom-set (variable expression &optional comment)

    "`custom-set-variables' wrapper."
    
    (declare (indent 2) (doc-string 3))

    `(ignore-errors
       (custom-set-variables
        (list (quote ,variable) ,expression :eager nil ,comment)))) ;TODO handle variables, not just symbols

  ;;--------------------------;;

  

  ;;--------------------------;;

  ())

;;----------------------------------------------;;

(defun sboo-init-use-package ()

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
;; Settings ------------------------------------;;
;;----------------------------------------------;;

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

;;----------------------------------------------;;

;;(sboo-init-use-package)

;;----------------------------------------------;;

(list (require 'diminish) (require 'bind-key))

;;==============================================;;
;; Variable Safety:

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

;;==============================================;;

;;----------------------------------------------;;
;; Register ------------------------------------;;
;;----------------------------------------------;;

(add-to-load-path! sboo-root-directory)
(add-to-load-path! sboo-lisp-directory)

;;----------------------------------------------;;
;; Groups --------------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defgroup sboo

  nil

  "« sboo »'s customization."

  :link '(url-link "https://github.com/sboosali/.emacs.d#readme")
  :group 'local)

;;----------------------------------------------;;
;; Settings ------------------------------------;;
;;----------------------------------------------;;

(ignore-errors
  (setq custom-file sboo-custom-file))

;;----------------------------------------------;;

(ignore-errors (sboo-load-file! "sboo-settings.el"))
(ignore-errors (sboo-load-file! "sboo-keybindings.el"))
(ignore-errors (sboo-load-file! "sboo-aliases.el"))
(ignore-errors (sboo-load-file! "sboo-commands.el"))

;; ^ NOTE `load' these files (rather than `require' them)
;;        because they execute statements (rather than `provide' definitions).
;;
;;        Later configuration (i.e. `sboo-*' features) musn't depend on them
;;        (whether explicitly via `require' or implicitly via reference).
;;        Instead, « sboo-utilities.el » has any general-purpose dependencies.
;;        Hence, we `ignore-errors' (rather than aborting the configuration).
;;

;;----------------------------------------------;;
;; Themes --------------------------------------;;
;;----------------------------------------------;;

(ignore-errors

  (when (>= emacs-major-version 24)

    (add-to-theme-path! sboo-theme-directory)
    (add-to-theme-path! (emacs-subdir "themes"))

    (progn
      (sboo-register-submodule-packages! "solarized")
      (sboo-register-submodule-themes!   "solarized")
      (load-theme 'solarized :no-confirm))

    ()))

;;----------------------------------------------;;

(ignore-errors

  (add-to-icon-path! sboo-icon-directory)
  (add-to-icon-path! (emacs-subdir "icons"))

  ())

;;----------------------------------------------;;
;; Packages (Builtins) -------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-toolbar nil :no-error)
;;(setq tool-bar-map sboo-tool-bar-map)
  ())

;;----------------------------------------------;;

(when (and (>= emacs-major-version 26)
           (require 'sboo-autosave nil :no-error)
           )

  (sboo-autosave-init!)

  (add-startup-hook! #'sboo-autosave-config!))

;;----------------------------------------------;;;

(when (require 'sboo-auto-mode nil :no-error)

  ;;------------------------;;

  (sboo-add-auto-mode-basename "LICENSE" #'text-mode)
  (sboo-add-auto-mode-basename "TODO"    #'text-mode)
  (sboo-add-auto-mode-basename "NOTES"   #'text-mode)

  (sboo-add-auto-mode-basename ".gitignore"     #'conf-mode)
  (sboo-add-auto-mode-basename ".gitattributes" #'conf-mode)

  (sboo-add-auto-mode-basename "terminalrc" #'conf-mode)
  ;; ^ for the « xfce4-terminal » program.

  (sboo-add-auto-mode-basename ".xbindkeysrc" #'conf-mode)
;;(sboo-add-auto-mode-basename "xbindkeysrc.scm" #'xbindkeys-scheme-mode)
  ;; ^ for the « xbindkeys » program.

  ;;------------------------;;

  (sboo-add-auto-mode-file-extension "service"    #'conf-mode) ; e.g. « /etc/services »
  (sboo-add-auto-mode-file-extension "interfaces" #'conf-mode) ; e.g. « /etc/network/interfaces »

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
;;            (require 'sboo-theme nil :no-error))
;;   (add-to-list 'custom-theme-load-path sboo-theme-directory)
;;   (sboo-theme-set!)
;;   ())

;; ^ `load-theme':
;;
;; (defun load-theme (THEME &optional NO-CONFIRM NO-ENABLE)

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

(when (require 'sboo-elisp nil :no-error)

  (dolist (MODE sboo-lisp-modes)
    (font-lock-add-keywords MODE sboo-lisp-keywords))

  ())

;;----------------------------------------------;;

;; (use-package compile
;;   :bind (("s-m" . compile))
;;   :config 
;;          
;;                 compilation-read-command nil
;;                 compile-command "make")
;;  ())

(when (require 'sboo-compilation nil :no-error)

  (sboo-compilation-init!)

  (add-startup-hook! #'sboo-compilation-config!))

;;==============================================;;

(use-package grep

  :config

  (dolist (CONS '(("hs" . "*.hs")
                  ("md" . "*.md")
                  ))
    (add-to-list 'grep-files-aliases CONS))

  ())

;; `grep-files-aliases':
;;
;;     (("all" . "* .[!.]* ..?*")
;;      ("el" . "*.el")
;;      ("ch" . "*.[ch]")
;;      ...)
;;
;;

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

  :config

  (add-hook 'sh-mode-hook 'flycheck-mode)

  ;; ^ FlyCheck builds-in a « shellcheck » checker
  ;; ^ « shellcheck » is a Bash Linter.

  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

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

  ())

;;----------------------------------------------;;
;;; Internal Packages --------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-server nil :no-error)
  (add-startup-hook! #'server-start-unless-running))

;;----------------------------------------------;;

(require 'sboo-widgets)
;;;  (sboo-minibuffer-config))
;;;  (sboo-config-fonts))

;;----------------------------------------------;;

(when (require 'sboo-make nil :no-error)
  (add-hook 'makefile-mode-hook #'sboo-show-trailing-whitespace))

;;----------------------------------------------;;

;; (when (require 'sboo-prog-mode nil :no-error)
;;   (dolist (HOOK sboo-prog-mode-hooks)
;;     (add-hook 'prog-mode-hook HOOK)))

;;----------------------------------------------;;

(add-hook 'text-mode-hook #'sboo-set-input-method-TeX)

;; (when (require 'sboo-text-mode nil :no-error)
;;   (dolist (HOOK sboo-text-mode-hooks)
;;     (add-hook 'text-mode-hook HOOK)))

;;----------------------------------------------;;

(when (require 'sboo-shell nil :no-error)

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

(when (require 'dired nil :no-error)

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

(when (require 'saveplace nil :no-error)

  (setq-default save-place t)
  ;TODO; (setq save-place-file (expand-file-name "save-point-places" user-emacs-directory))

  ())

;; ^ Save point position between sessions.
;;
;; "Save the position I was in each file, i.e. no scrolling down to paragraph N or function foo when I reopen my files."
;;

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

;;----------------------------------------------;;

(when (require 'man nil :no-error)

  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face    :bold t)
  (set-face-attribute 'Man-underline  nil :inherit font-lock-keyword-face :underline t)

  ())

;; (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "orange red")
;; (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "forest green")

;; Or to be theme agnostic:

;; (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
;; (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

;;----------------------------------------------;;

;; Non-Package features must be configured more because,
;; with `with-eval-after-load', `define-key', etc.

(with-eval-after-load 'comint

  (bind-key "<up>"   'comint-previous-matching-input-from-input comint-mode-map)
  (bind-key "<down>" 'comint-next-matching-input-from-input     comint-mode-map)

  (setq comint-scroll-to-bottom-on-output 'others)
  (setq comint-scroll-to-bottom-on-input  'this)

  (sboo-custom-set comint-buffer-maximum-size
      2000
    "Increase.")

  ())
;;----------------------------------------------;;
;; `package-builtins': -------------------------;;
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

;; no multiframe ediff
(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;----------------------------------------------;;

(use-package sql
  :defer t

  :config

  (when (require 'sboo-sql nil :no-error)
    ())

  ())

;;----------------------------------------------;;
;; Internal Packages: Settings -----------------;;
;;----------------------------------------------;;

(progn

  (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

  ())

;;----------------------------------------------;;
;; Internal Packages: Utilities ----------------;;
;;----------------------------------------------;;

;;(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; ^ `flyspell-prog-mode' spell-checks comments.

;;----------------------------------------------;;

(add-to-list 'auto-mode-alist (cons "\\.xpm\\'" #'c-mode))

;;----------------------------------------------;;

(ignore-errors

  (when (require 'sboo-unicode nil :no-error)

    ;;(add-startup-hook! #'sboo-unicode-init)

    (sboo-unicode-init)))

;;----------------------------------------------;;

(ignore-errors

  (when (require 'sboo-comment nil :no-error)

    ()))

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

(when (require 'sboo-packages nil :no-error)

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
;;(sboo-load-file! "sboo-init-use-package.el")

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
                          "Override `edit-indirect-default-guess-mode'.")

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

(when (require 'sboo-haskell nil :no-error)

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

    :hook        ((haskell-mode . interactive-haskell-mode))

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

    :init
    (setq haskell-doc-current-info #'sboo-haskell-doc-current-info)

    ;; (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    ;;
    ;; ^ `haskell-process' repeatedly spams errors for working projects,
    ;; stealing focus from the current buffer.

    (remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

    ;; ^ See « https://wiki.haskell.org/Emacs/Inferior_Haskell_processes ».

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

    :load-path "/home/sboo/.emacs.d/submodules/dante"
 ;;TODO  :load-path "~/.emacs.d/submodules/dante"

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

;;----------------------------------------------;;
;; External Packages: ProgrammingLanguages -----;;
;;----------------------------------------------;;

(when (require 'sboo-nix nil :no-error)

  ;;------------------------;;

  (use-package nix-mode

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

    ())

  ;;------------------------;;

  (use-package nix-repl)

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
;; External Packages: `company-*' Configurations
;;----------------------------------------------;;

(when (require 'sboo-company nil :no-error)

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

(when (require 'sboo-haskell-compilation nil :no-error)

  

  ())

;;----------------------------------------------;;



;;----------------------------------------------;;

;;----------------------------------------------;;
;; External Packages: Miscellaneous ------------;;
;;----------------------------------------------;;

(when (require 'sboo-projectile nil :no-error)

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

(when (require 'sboo-yasnippets nil :no-error)

  (use-package yasnippet

    :demand t

    :mode ("\\.yasnippet\\'" . snippet-mode)

    :bind (("<kp-home>" . yas-next-field-or-maybe-expand)
          )
    
    :init
    (setq yas-snippet-dirs (list sboo-snippets-directory))

    ;; ^ `setq' vs `add-to-list': remove the default.

    :config

    ;;------------------------;;

    (defun sboo-yas-reload (force)

      "Recompile and reload all « .yasnippet » files."

      (interactive "P")

      (yas-recompile-all)
      (yas-reload-all force))

    ;;------------------------;;

    (sboo-yas-reload t)

    (yas-global-mode 1)

    (defalias '/y #'yas-insert-snippet)

    ;;------------------------;;

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
;;; `rg': "Rust Grep".

(use-package rg)

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

(use-package json-mode

  ;; :disabled

  :mode (("\\.json\\'" . json-mode)
         )

  :hook ((json-mode . flycheck-mode))

  ;; ^ FlyCheck builds-in a « jsonlint » checker
  ;; ^ « jsonlint » is a JSON Linter.

  )

;;----------------------------------------------;;

(use-package yaml-mode

  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'"  . yaml-mode)
         )

  )

;;----------------------------------------------;;

(use-package bnf-mode

  :load-path "~/.emacs.d/submodules/bnf-mode"

;;  :load-path (sboo-submodule-directory "bnf-mode")

  :commands (bnf-mode))

;;----------------------------------------------;;

;; (use-package xpm
;;   :commands (xpm-grok xpm-finish xpm-raster xpm-as-xpm xpm-put-points xpm-generate-buffer)
;;   :mode (("\\.xpm\\'" . c-mode))
;;   ; :mode (("\\.xpm\\'" . xpm-mode))
;;   ())

;;----------------------------------------------;;

(use-package xmodmap-mode

  :mode        "\\.xmodmap\\'"
  :interpreter "xmodmap"

  :init
  ()

  :config

  (add-hook 'xmodmap-mode-hook #'sboo-set-font-to-iosevka)
  
  ())

;;----------------------------------------------;;
;; External Packages: Editing ------------------;;
;;----------------------------------------------;;

(use-package wrap-region

  :config

  (add-to-list 'wrap-region-except-modes 'magit-status-mode)

  (require 'sboo-html nil :no-error)

  (wrap-region-add-wrappers

   '(
     ("{-" "-}"    ";" (haskell-mode))

     ("/* " " */"  ";" (nix-mode javascript-mode css-mode java-mode))

     ("<!--" "-->" ";" ,(or (bound-and-true-p sboo-html-modes-list) html-mode))

     ;; ^ « ; » (i.e. the semicolon character) is our universal trigger-key for commenting a region.
     ;;
     ;;   c.f. « M-; » runs `comment-dwim' across langauges.

     ("`" "`" nil (markdown-mode gfm-mode))

     ;; ^ « <code> » syntax.

     ("%" "%" nil (bat-mode))

     ;; ^ Environment Variable syntax.

     ))

  (when (bound-and-true-p sboo-html-wrap-region-table)
    (wrap-region-add-wrappers (sboo-html-wrap-region-table)))

  ;;TODO...
  ;; (when (bound-and-true-p sboo-markdown-wrap-region-table)
  ;;   (wrap-region-add-wrappers (sboo-markdown-wrap-region-table)))

  (wrap-region-mode +1))

;; ^ `wrap-region-add-wrappers':
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

;; ^ `wrap-region-table'
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

;; ^ `wrap-region-add-wrapper':
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



;;----------------------------------------------;;
;; External Packages: Miscellaneous ------------;;
;;----------------------------------------------;;

(use-package rainbow-mode

    :load-path "/nix/store/qnl0dlq0zsvg5rnd6c9grdn1phv9xc7x-emacs-rainbow-mode-1.0.1/share/emacs/site-lisp/elpa/rainbow-mode-1.0.1/" ;FIXME dont hardcode, must eval at macro-time.

    :hook (prog-mode text-mode)

    :config

    ())

;;----------------------------------------------;;

(use-package which-key

    )

;;----------------------------------------------;;

;;(require volatile-highlights ())

;;----------------------------------------------;;

(when (require 'volatile-highlights nil :no-error)
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

;; e.g. this feature provides `desktop-environment-keyboard-backlight-set':
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

;; `dbus-call-method':
;;
;; 

;;----------------------------------------------;;

(use-package eimp

    :hook (image-mode-hook)

    )

;;----------------------------------------------;;
;; Appearence ----------------------------------;;
;;----------------------------------------------;;

(use-package all-the-icons

  :load-path "~/.emacs.d/submodules/all-the-icons.el"

  )

;; Installation:
;;
;; M-: (all-the-icons-install-fonts)

;; Usage:
;;
;; M-: (insert (all-the-icons-icon-for-file "foo.hs"))
;; M-: (all-the-icons-insert-icons-for 'faicon 10 0.5)   ; height=10px, delay=500ms.

;; Notes:
;;
;; e.g. fontsets: 'faicon 'octicon 'alltheicon

;;----------------------------------------------;;

(use-package all-the-icons-dired

  :load-path "~/.emacs.d/submodules/all-the-icons-dired"

  :hook (dired-mode . all-the-icons-dired-mode)

  )

;;----------------------------------------------;;

(use-package doom-modeline

  :hook (after-init . doom-modeline-mode)

  )

;;----------------------------------------------;;
;; Finalization --------------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-fonts nil :no-error)
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