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

(ignore-errors
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
  (require 'bind-key)
  (require 'diminish)
  (require 'delight))

;;----------------------------------------------;;
;; Settings ------------------------------------;;
;;----------------------------------------------;;
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

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/.emacs.d#readme")

  :group 'local

  ;; ^ Group `local' means: your personal (“site-local”) configuration.

  )

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

(unless (require 'sboo-private "~/.emacs.d/private/sboo-private" :no-error)
  (require 'sboo-private nil :no-error))

;; ^ NOTE if the "true" `sboo-private' can't be loaded
;;        (e.g. doesn't exist yet, file doesn't parse, etc),
;;        load a "fake" one which is always available
;;        (because it's version-controlled,
;;        but doesn't contain any sensitive information.)
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

  (sboo-autosave-config!))

;;(add-startup-hook! #'sboo-autosave-config!))

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

(when (require 'sboo-lisp nil :no-error)

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

;;----------------------------;;

(use-package grep

  :config

  (defun sboo-grep-config ()
    "Hook for `grep-mode'."
    (toggle-truncate-lines 1))

  (add-hook 'grep-mode #'sboo-grep-config)

  (dolist (CONS '(("hs" . "*.hs")
                  ("md" . "*.md")
                  ))
    (add-to-list 'grep-files-aliases CONS))

  (dolist (DIRECTORY '("tmp" "old" "stdout"
                       "dist" "dist-newstyle" "dist-dante" ".stack-work"
                       "elpa" ".cask"
                       "node_modules" ".bundle"
                       ))
    (add-to-list 'grep-find-ignored-directories DIRECTORY))

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

;;(add-hook 'text-mode-hook #'sboo-set-input-method-TeX)

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
;; Completion

(defvar sboo-abbrev-file

  (condition-case _
      (sboo-file "dabbrev/abbrev_defs.el")
    (void-function
     "~/.emacs.d/sboo/dabbrev/abbrev_defs.el"))

  "Personal (version-controlled) `abbrev-file-name'.")

;;----------------------------;;

(use-package dabbrev

  :diminish (abbrev-mode " A")

  :init

  (let* ((DIRECTORY-DABBREV (file-name-directory sboo-abbrev-file))
         )
    (when (not (file-directory-p DIRECTORY-DABBREV))
      (make-directory DIRECTORY-DABBREV :make-parent-directories)))

  :custom

  (abbrev-file-name sboo-abbrev-file "Personal `dabbrev' config.")

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
;; Recent Files

(use-package recentf

  :custom

  (recentf-max-saved-items 200 "Remember more files.")
  (recentf-max-menu-items  15  "Remember more files.")

  :config

  (recentf-mode +1)

  ())

;; ^ "`recentf'" abbreviates "RECENT Files".

;;----------------------------------------------;;
;; `uniquify'

(use-package uniquify

  :custom

  (uniquify-buffer-name-style 'forward
                              "distinguish Synonyms Buffers (when two buffers are open with the same name, this makes it easier to tell them apart).")

  :config

  ())

;;----------------------------------------------;;
;; `saveplace':

(use-package saveplace

  :commands (save-place-mode)

;;:custom

  :config

  (save-place-mode +1)

  ;; ^ `saveplace' remembers your Last Position for Re-Opened Files."

  ())

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

;; Xah Lee functions:

(when (require 'sboo-xah nil :no-error)

  ())

;;----------------------------------------------;;
;; `package-builtins': -------------------------;;
;;----------------------------------------------;;

;; these features (below) can be configured with `use-package',
;; because they are actual packages.
;;
;; See: (describe-variable 'package--builtins)

;;----------------------------------------------;;

(use-package emacs

  :no-require t

  :delight

  (auto-revert-mode " 🗘")
  ;; ^ Shorten `auto-revert-modee'.

  (visual-line-mode " VL")
  ;; ^ Shorten `visual-line-mode'.

  (auto-fill-function " aF")
  ;; ^ Shorten `auto-fill-mode'.

  (buffer-face-mode)
  ;; ^ Hide `buffer-face-mode'.

  :config

  ())

;;----------------------------------------------;;

(use-package autorevert

  :commands (auto-revert-mode)

  :delight (auto-revert-mode " 🗘")
  ;; ^ Shorten `auto-revert-modee'.

  :config

  ())

;;----------------------------------------------;;

(use-package ediff

  :defer t

  :custom

  (ediff-window-setup-function 'ediff-setup-windows-plain "“Plain” means “no multiframe ediff”.")

  :config

  ())

;;----------------------------------------------;;

(use-package eldoc

  :defer t

  :commands (eldoc-mode)

  :delight (eldoc-mode)

;;:custom

  :config

  ())

;;----------------------------------------------;;

(use-package sql

  :defer t

  :config

  (when (require 'sboo-sql nil :no-error)
    ())

  ())

;;----------------------------------------------;;

(use-package find-dired

  :custom

  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld") "“By default Emacs will pass -exec to find and that makes it very slow. It is better to collate the matches and then use xargs to run the command.”")

  :config

  ())

;; ^ URL `https://www.masteringemacs.org/article/working-multiple-files-dired'

;;----------------------------------------------;;

(use-package calendar

  :defer t

  :custom

  (calendar-week-start-day 0 "“0 means Sunday, 1 means Monday, etc”")

  :config

  ())

;;----------------------------------------------;;

(use-package vc

  :defer t

  :custom

  (vc-follow-symlinks t "don't ask when visiting a symbolic link to a version-controlled file (but do warn in the echo area).")

  :config

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
;; External Packages: Prioritized Packages -----;;
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

(use-package dash)

;; ^ URL `https://github.com/magnars/dash.el'

;;----------------------------------------------;;

(use-package s)

;; ^ URL `https://github.com/magnars/s.el'

;;----------------------------------------------;;

(use-package f)

;; ^ URL `https://github.com/rejeep/f.el'

;;----------------------------------------------;;

(use-package ht)

;; ^ URL `https://github.com/Wilfred/ht.el'

;;----------------------------------------------;;

(use-package ov)

;; ^ URL `https://github.com/ShingoFukuyama/ov.el'

;;----------------------------------------------;;
;; External Packages: Completion ---------------;;
;;----------------------------------------------;;

(use-package helm-config

  :custom

  (helm-command-prefix-key "M-q" "the Default (« C-x c ») is too similar to `kill-emacs's keybinding.")

  ;; ^  NOTE `helm-command-prefix-key':
  ;;    becomes immutable once `helm-config' is `load'ed.

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

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; ^ website `google' via program `curl'.

  ())

;;----------------------------------------------;;

(use-package helm

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

  ;; Helm and Ido mode are mutually-exclusive:

  (helm-autoresize-mode +1)
  (ido-mode             -1))

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
;;   (helm-mode 1))

;;----------------------------------------------;;

(use-package helm-sys

  :commands (helm-top)

  )

;;----------------------------------------------;;

(use-package helm-dabbrev

  :commands (helm-dabbrev)

  )

;;----------------------------------------------;;

(use-package helm-buffers

  :commands (helm-buffers-list)

  :custom

  (helm-buffers-fuzzy-matching t "Fuzzily-Match buffer-names (for `helm-mini' when listing buffers).")

  (ido-use-virtual-buffers t "`helm-buffers-list' wants this.")

  :config

  (dolist (MODE '(picture-mode artist-mode))
    (add-to-list 'helm-buffers-favorite-modes MODE))
                                           
  )

;;----------------------------------------------;;

(use-package helm-files

  :commands (helm-find-files)

  :custom

  (helm-ff-file-name-history-use-recentf t "use `recentf'.")
  (helm-ff-search-library-in-sexp        t "search for library in `require' and `declare-function' sexp.")

  (helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
                                "hide these files from the Helm Buffer.")

  )

;;----------------------------------------------;;

(when (require 'sboo-company nil :no-error)

  ;;------------------------;;

  (use-package company

    :diminish (company-mode " ©")

    ;;------------------------;;

    :custom

    (company-backends sboo-company-backends "personal Company Backends.")

    (company-show-numbers               t   "")
    (company-minimum-prefix-length      1   "minimum Prefix Length for Idle Completion.")
    (company-tooltip-align-annotations  t   "")
    (company-dabbrev-downcase           nil "")

    ;;------------------------;;

    :config

    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    ;; ^ use Company for Emacs's Builtin Completion.

    ;; ^ NOTE why not `:bind'?
    ;;   Because `:bind''s syntax is « (`kbd' ...) » only, no « [`remap' ...] ».

    ;;------------------------;;

    ;; `company-active-map':

    (define-key company-active-map (kbd "TAB")       #'company-complete-common-or-cycle)
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

    ;; ^ FYI candidate selection via `M-{number}'
    ;;  (i.e. `M-1', `M-2', ..., `M-9').

    ;;------------------------;;

    (global-company-mode +1)))

;;----------------------------------------------;;
;; External Packages: Templates ----------------;;
;;----------------------------------------------;;

(when (require 'sboo-yasnippets nil :no-error)

  (use-package yasnippet

    :commands (snippet-mode yas-insert-snippet yas-next-field-or-maybe-expand)

    :diminish (snippet-mode " Y")

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
# condition   : (let ((KEY "$1")) (condition-case nil (sboo-yasnippet-condition :key KEY :indentation 6) (void-function (= (current-column) (string-width KEY)))))
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

    (defun sboo-yas-reload (force)

      "Recompile and reload all « .yasnippet » files."

      (interactive "P")

      (yas-recompile-all)
      (yas-reload-all force)
      (require 'sboo-yasnippets nil :no-error))

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

;;----------------------------------------------;;
;; External Packages: Programming --------------;;
;;----------------------------------------------;;

(use-package flycheck

  :defer t

  :delight (flycheck-mode " 🛸")

  :config

  (add-hook 'flycheck-error-list-mode-hook #'visual-line-mode)

  (when (require 'sboo-flycheck nil :no-error)

    (bind-key "<kp-divide>" #'sboo-flycheck)

    (add-to-list 'display-buffer-alist sboo-flycheck-display-buffer))

  ())

;; ^ Links:
;;
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
;;; `magit': "eMAcs GIT".

(progn

  (use-package magit

    :bind (("s-g s" . magit-status)
           )

    :custom

    (magit-save-repository-buffers 'dontask "don't ask (just save).")

    :config

    ())

  ;;--------------------------;;

  ;; (use-package magithub
  ;;FIXME crashes magit
  ;;   )

  ())

;;----------------------------------------------;;
;; External Packages: Haskell ------------------;;
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

(when (require 'sboo-haskell-compilation nil :no-error)

  ()

  ())

;;----------------------------------------------;;
;;; External Packages: `company-*' Configurations

(when (require 'sboo-company nil :no-error)

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
;; External Packages: Formats ------------------;;
;;----------------------------------------------;;

(when (require 'sboo-html nil :no-error)

  (use-package markdown-mode

    :commands (markdown-mode gfm-mode)

    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'"       . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)
           )

    :bind (:map markdown-mode-map
                ("TAB" . dabbrev-expand)
                :map gfm-mode-map
                ("TAB" . dabbrev-expand)
                )

    :init

    (setq markdown-command "markdown")
    ;; ^ `pandoc'. TODO

    :config

    (dolist (HOOK sboo-html-hooks)
      (dolist (FUNCTION sboo-html-functions)
        (add-hook HOOK FUNCTION)))

    ())

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/jrblevin/markdown-mode'
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

;; ^ `json-mode' registers:
;;
;;     ;; Well formatted JSON files almost always begin with “{” or “[”.
;;     ;;;###autoload
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

;; (use-package xpm
;;   :commands (xpm-grok xpm-finish xpm-raster xpm-as-xpm xpm-put-points xpm-generate-buffer)
;;   :mode (("\\.xpm\\'" . c-mode))
;;   ; :mode (("\\.xpm\\'" . xpm-mode))
;;   ())

;;----------------------------------------------;;
;; External Packages: Editing ------------------;;
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

(use-package rg

  :config

  ())

;;----------------------------------------------;;

(use-package wrap-region

  :delight (wrap-region-mode " 🎁")

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

;; ^ Links
;;
;;   • URL `https://github.com/rejeep/wrap-region.el'
;;   • URL `http://pragmaticemacs.com/emacs/wrap-text-in-custom-characters/'
;;   • URL `https://www.youtube.com/watch?v=9SWAKPF0fHE'
;;

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

(use-package expand-region

  :delight (expand-region-mode " ")

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/magnars/expand-region.el'
;;   • URL `'
;;

;;----------------------------------------------;;

(use-package sed-mode

  :delight (sed-mode " ")

  :config

  ())

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

  :config

  ;;--------------------------;;

  (defun sboo-edit-indirect-guess-mode (parent-buffer parent-region-begin parent-region-end)

    "Guess the major mode for an edit-indirect buffer.
Calls `set-auto-mode', which parses the « mode » file-local (special) variable 
(i.e. « -*- mode: ... -*- »)."

    (set-auto-mode t))

  ;;--------------------------;;

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/Fanael/edit-indirect'
;;


;;----------------------------------------------;;
;; External Packages: Navigation ---------------;;
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

  )

;; ^ Links:
;;
;;   • URL `https://github.com/ShingoFukuyama/helm-swoop#readme'
;;

;;----------------------------------------------;;

(use-package smartscan

  :commands (global-smartscan-mode)

  :config

  ;;(global-smartscan-mode +1)

  ())

;; ^ Make « M-n » and « M-p » look for the `symbol-at-point'.
;;
;;   • URL `https://github.com/mickeynp/smart-scan'
;;   • URL `https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el'
;;

;;----------------------------------------------;;
;; External Packages: Filesystem ---------------;;
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

  :config

  ())

;; ^  URL `https://github.com/jaypei/emacs-neotree'

;;----------------------------------------------;;
;; External Packages: Terminal -----------------;;
;;----------------------------------------------;;

(use-package shell-pop

  :bind (("C-t" . shell-pop))

  :config

  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)

  ())

;; ^ URL `http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/'

;; ^ URL `https://github.com/kyagi/shell-pop-el'

;;----------------------------------------------;;
;; External Packages: Highlighting -------------;;
;;----------------------------------------------;;

(use-package volatile-highlights

  :diminish

  :config

  (volatile-highlights-mode +1)

  ())

;; ^ URL `'

;;----------------------------------------------;;

(use-package rainbow-mode

  :commands (rainbow-mode)

  :delight (rainbow-mode " 🌈")

  ;; :hook (prog-mode text-mode)

  :config

  ())

;; ^ URL `'
;; ^ URL `https://jblevins.org/log/rainbow-mode'

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

  :delight (highlight-numbers-mode)

  :hook (prog-mode . rainbow-identifiers-mode)

  :config

  ())

;; ^ `highlight-numbers': highlight numbers (in any language).

;; ^ URL `https://github.com/Fanael/highlight-quoted'

;;----------------------------------------------;;

(use-package highlight-quoted

  :commands (highlight-quoted-mode)

  :delight (highlight-quoted-mode)

  :hook (prog-mode . rainbow-identifiers-mode)

  :config

  ())

;; ^ URL `https://github.com/Fanael/highlight-numbers'

;; ^ `highlight-quoted': highlight *Lisp Symbols* (e.g. `'foo`).

;;----------------------------------------------;;

(use-package highlight-blocks

  :commands (highlight-blocks-mode)

  :delight (highlight-blocks-mode)

  :config

  (let* ((LISP-HOOKS
          (if (require 'sboo-lisp nil :no-error)
              sboo-lisp-hooks
            '(emacs-lisp-mode-hook)))
         )

    (dolist (HOOK LISP-HOOKS)
      (add-hook HOOK #'highlight-blocks-mode)))

  ())

;; ^ URL `https://github.com/Fanael/highlight-blocks'

;; ^ `highlight-blocks': highlights *block* at-`point' (i.e. innermost parenthetical grouping(s)).

;;----------------------------------------------;;

(use-package highlight-escape-sequences

  :commands (highlight-escape-sequences-mode)

  :delight (highlight-escape-sequences-mode)

  :hook (prog-mode . rainbow-identifiers-mode)

  :config

  ())

;; ^ URL `https://github.com/dgutov/highlight-escape-sequences'

;; ^ `highlight-escape-sequences': highlight *Escape Sequences* (e.g. `"\n"`).

;;----------------------------------------------;;
;; External Packages: Windows/Buffers ----------;;
;;----------------------------------------------;;

(use-package awesome-tab

  :config

  (awesome-tab-mode +1)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/manateelazycat/awesome-tab'
;;

;;TODO;; (add-to-list helm-source-list awesome-tab-build-helm-source)

;;----------------------------------------------;;
;; External Packages: Appearence ---------------;;
;;----------------------------------------------;;

(use-package telephone-line

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

  :load-path "~/.emacs.d/submodules/unicode-fonts"

  :config

  (unicode-fonts-setup)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/rolandwalker/unicode-fonts'
;;

;;----------------------------------------------;;

(use-package all-the-icons
  :disabled

  :load-path "~/.emacs.d/submodules/all-the-icons.el"

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/domtronn/all-the-icons.el'
;;

;;----------------------------------------------;;

(use-package icons-in-terminal
  :disabled

  :load-path "~/.emacs.d/submodules/icons-in-terminall"

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/sebastiencs/icons-in-terminal'
;;

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
  :disabled

  :load-path "~/.emacs.d/submodules/all-the-icons-dired"

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

  :load-path "~/.emacs.d/submodules/doom-modeline"

  :hook (after-init . doom-modeline-mode)

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/seagle0128/doom-modeline'
;;

;;----------------------------------------------;;
;; External Packages: Clipboard ----------------;;
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
;; External Packages: Media --------------------;;
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

  :if window-system

  :load-path "site-lisp/pdf-tools/lisp"

  :commands (pdf-view-mode)

  :magic ("%PDF" . pdf-view-mode)

  :config

  (pdf-tools-install)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/politza/pdf-tools'
;;

;;----------------------------------------------;;

(use-package vlf

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
;; External Packages: Help ---------------------;;
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

  (which-key-mode +1)

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/justbur/emacs-which-key'
;;

;;----------------------------------------------;;
;; External Packages: Miscellaneous ------------;;
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

;; `bm' (a `featurep').
;; > Quickly save and restore point using registers
;; 

;; `bmkp' a.k.a bookmark+

;; ^ Links:
;;
;;   • URL `https://github.com/joodland/bm'
;;

;;----------------------------------------------;;

(use-package deft

  :commands (deft-new-file deft-new-file-named)

  :config

  ())

;; ^ Deft is an Emacs mode for quickly browsing, filtering, and editing
;;   directories of plain text notes, inspired by Notational Velocity.

;; ^ Links:
;;
;;   • URL `http://jblevins.org/projects/deft'
;;   • URL `https://github.com/jrblevin/deft'
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
;; Conditional Configuration -------------------;;
;;----------------------------------------------;;

(when (require 'sboo-os nil :no-error)

  (add-hook 'window-setup-hook #'sboo-maximize-frame t)

  ())

;; ^ NOTE:
;;
;;  • `window-setup-hook' is similar to ‘emacs-startup-hook’.
;;

;;----------------------------------------------;;

(when (require 'sboo-ui nil :no-error)

  ())

;;----------------------------------------------;;

;; (use-package edit-server
;;   :if window-system
;;   :init
;;   (add-hook 'after-init-hook 'server-start t)
;;   (add-hook 'after-init-hook 'edit-server-start t)
;;   ())

;;----------------------------------------------;;
;; Finalization --------------------------------;;
;;----------------------------------------------;;

(when (require 'sboo-fonts nil :no-error)
  (sboo-fonts-config!))

;;----------------------------------------------;;
;;; Notes: -------------------------------------;;
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

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-init)

;;; sboo-init.el ends here