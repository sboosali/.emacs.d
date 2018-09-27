



















































;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;TODO doesn't work.
;; (defun redo (&optional arg)
;;   "Redo some previouly-undone changes.
;; Use **`undo`**, not this function, to continue the `redoing`.
;; A numeric ARG serves as a repeat count."
;;   (interactive "*P")
;;   (keyboard-quit)
;;   (undo arg)
;;   )
;; ;; ^ see `undo` in `simple.el`.
;; ;; see https://stackoverflow.com/questions/3527142/how-do-you-redo-changes-after-undo-with-emacs






  ;;OLD
  ;; (let*
  ;;     ((n "*shell*")
  ;;      (b (get-buffer n)))
  ;;   (if (bufferp b)
  ;;       (switch-to-buffer b nil 'force-same-window)
  ;;     (shell n))))










(defun sboo-buffers-list ()

  "Try `helm-buffers-list', fallback to `list-buffers'.
  
  (When `helm' isn't loaded/installed, this command falls back 
  to the standard-library command upon which that package improves.)
  "
  (interactive)

  (let ((*command* #'helm-buffers-list))
  
    (if (and (commandp *command*) 
             (fboundp  *command*))
             
      (call-interactively *command*)

     (call-interactively #'list-buffers))))

;; ^ NOTE 
;; the predicates succeed even when command is marked with `autoload'.















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration: External Packages ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-helm nil t)
  (sboo-config-helm!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provisioning: External Packages ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile

  (load-path! ./submodules/use-package)

  ;; ^ either vendored...
  
  ;;;(load-path! ./elpa/use-package-2.3)

  ;; ^ ... or `package-install'ed.

  (require 'use-package))

  ;; ^ `use-package' is a macro;
  ;; as such, it's a compile-time dependency only (i.e. not run-time).
  ;;
















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














(defun sboo-buffers-list ()

;;  (if (fboundp 'helm-buffers-list)
;;  (if (featurep 'helm)
;;    (helm-buffers-list)  ;TODO; preserve abilit to autoload
;;   (list-buffers)))
;;    (call-interactively #'helm-buffers-list)
;;   (call-interactively #'list-buffers)))












;;;;;;;;;;
;; older versions...

;; (global-set-key (kbd "<f7>")  'split-window-right)
;; ;; ^ i.e. [C-x 3]
;; ;; (split-window-right &optional SIZE)

;; (global-set-key (kbd "<f8>")  'other-window)
;; ;; ^ i.e. [C-x o]
;; ;; (other-window COUNT &optional ALL-FRAMES)












;(require 'bind-key)                     ;TODO rm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `bind-keys*' Global KeyBindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(bind-keys*
;; ;;
;; ("M-p"     . backward-paragraph)
;; ("M-n"     . forward-paragraph)
;;  ;; ^^ paragraph navigation
;; ;;
;; ("C-x \\"  . align-regexp)
;; ;;
;; ("C-c r"   . revert-buffer)
;; ;;
;; ("C-h a"   . apropos)
;; ;; ^ Help should search more than just commands
;; ;;
;; ;;("M-c"     . toggle-char-case) 
;; ;; ^ TODO set this key to a similar but saner idea
;; ;;
;; ("C-c b"   . copy-file-name-to-clipboard)
;; ;; ^ copy file name to clipboard
;; ;;
;; ("<kp-end>" . xref-find-definitions)
;; ;;
;;)

;; (bind-key* "TAB" 'dabbrev-expand)
;; ;; ^ for all minor-modes override the "tab-character". TODO

;; (bind-key "<tab>" 'dabbrev-expand)
;; ;; ^ for all modes, don't override the tab-key, if a keybinding already exists (?) TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `bind-key*': 
;;
;; the keybinding overrides all minor-modes that may also bind the same key, use the `bind-key*' 
;;
;; 
















(defmacro ~USER (FilePath)
  "Construct a filepath literal, relative to the user's home directory.

  M-: (~USER ./haskell)
  \"/home/sboo/haskell\"
  "

  `(expand-file-name 
    (concat "~/"
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~EMACS (FilePath)
  "Construct a filepath literal, relative to `user-emacs-directory'.

  M-: (~EMACS lisp)
  \"/home/sboo/.emacs.d/lisp"
  "

  `(expand-file-name 
    (concat user-emacs-directory
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~SBOO (FilePath)
  "Construct a filepath literal, relative to `sboo-directory',
  my emacs-dotfiles location.

  M-: (~SBOO ./sboo-init-builtins.el)
  \"/home/sboo/.emacs.d/sboo/sboo-init-builtins.el"
  "

  `(expand-file-name 
    (concat sboo-directory "/" 
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




















(defmacro ./ (FilePath)
  "Construct a filepath literal, relative to the current `default-directory'.

  (TODO the current buffer's, or the file currently being loaded).
  
  M-: (./ haskell)
  \"/home/sboo/haskell\"

  "

  `(expand-file-name 
    (concat default-directory "/" 
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~/ (FilePath)
  "Construct a filepath literal, relative to the user's home directory.

  M-: (~/ haskell)
  \"/home/sboo/haskell\"

  "

  `(expand-file-name 
    (concat "~/" 
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~/.emacs.d/ (FilePath)
  "Construct a filepath literal, relative to `user-emacs-directory'.

  (TODO: the root directory of the currently running emacs instance).

  M-: (~/.emacs.d/ lisp)
  \"/home/sboo/.emacs.d/lisp"

  "

  `(expand-file-name 
    (concat user-emacs-directory "/" 
            (symbol-name (quote ,FilePath)))))












(defmacro ./ (FilePath)
  `(expand-file-name (concat load-file-name (symbol-name (quote ,FilePath)))))

(defmacro path! (FilePath)
  `(expand-file-name (symbol-name (quote ,FilePath))))

(defmacro sboo-register! (FilePath)
  `(add-to-list 'load-path (expand-file-name (symbol-value ,FilePath))))

(sboo-register! ./sboo/)








(defmacro sboo-bind! (KeySequence Command &optional KeyMap)













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My (default, featured) Emacs Configuration.
;;
;; To import (but not invoke) all configuration functions,
;; call « (require 'sboo) ».
;;
;; See `./README.md'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-bootstrap)

(sboo-bootstrap!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo)















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my configs (namespaced under "sboo").

(require 'sboo-initialization)    ;TODO mv these to before use-package, and remove all external dependencies
;; ^
;; Initially, do simple configurations (like keybindings, custom variables, etc),
;; which (should) always succeed.

(require 'sboo-internal) ;TODO mv these to before use-package, and remove all external dependencies
;; ^ builtin-packages i.e. "1st party".
;;
;; Properly configure any builtin-packages,
;; before configuring installed(i.e. third-party) packages.
;;

(require 'sboo-packages)
;; ^ my packages i.e. "2nd party".
;;
;; 

(require 'sboo-settings)
;; ^ Further settings.

(require 'sboo)            ;TODO rename
;; ^ other packages i.e. "3rd party".
;;
;; 

;;TODO (require 'haskell--projectile-compile--direnv-nixshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (server-start-unless-running))
  ;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-init-effects)
;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(progn
;;  (require 'sboo-desktop)
;;  (sboo-config-desktop))
;;      ;; ^ the `sboo-desktop` module has *only* definitions, no actions.
;;  (require 'sboo-quitting))
;;  ;; ^ requires `sboo-desktop`

  ;;^ 
  ;; Don't restore any buffers until all modes have been initialized/configured.
  ;; Otherwise, file-extensions won't have been registered with the correct modes,
  ;; custom typefaces won't have been associated, etc.
  ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-external)
;; ^
;; Finally, configure any installed (i.e. third-party) packages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More Shortcuts (TODO rm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;; (this is later to be defined after its dependent definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 sboo-emacs-initialized t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;































(progn
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize))









  (dolist (*p* sboo-installed-packages)

      (when (not (package-installed-p *p*))

        (package-install *p*)))







(when (>= emacs-major-version 24)

  (require 'package)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "elisp"))

;(add-to-list 'load-path (expand-file-name "submodules/use-package"))
;(add-to-list 'load-path (expand-file-name "submodules/dante"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install `sboo' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (add-to-list 'load-path (expand-file-name "sboo"))
  (require 'sboo)
  (sboo-register-sboo-load-paths!)
  ;; ^ register all `sboo-*` `load-path`s before `load`ing any `sboo-*` package.
  ())







;;(require 'sboo-dependencies)
;; 
;; ;;TODO expose command-line-arguments or environment-variables;
;; ;; `--install-dependencies' or `$SBOO_EMACS_INSTALL_DEPENDENCIES'.
;; 

(when sboo-install?
  (sboo-install-emacs-packages nil))


















;; NOTE `()' is `nil' (and parses as a pattern, not a variable).









  ;;(setq package-archives (assq-delete-all 'elpa 'package-archives))
  ;;(setq package-archives (assq-delete-all "elpa" 'package-archives))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo/string->symbol (STRING)
  "`intern` the `STRING', returning its symbol.
  
  Munge the string into an idiomatic elisp symbol:
  
  * clean up, by dropping any non-printable characters;
  * normalize casing, via `downcase';
  * normalize word boundaries, by replacing *all* "unidiomatic" substrings 
  (i.e. one-or-more whitespace and/or non-alphanumeric characters)
  with a single hyphen.

  M-: (sboo/string->symbol "foo-bar")
  'foo-bar

  M-: (sboo/string->symbol "foo, bar!")
  'foo-bar

  M-: (sboo/string->symbol ":Foo:|:Bar:")
  'foo-bar

  "
  (interactive)

  (let ((*symbolic-string* STRING)) ;TODO; implement
  
    (intern *symbolic-string*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Profiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sboo-profile-name ;TODO; clean/normalize envvar val
 (getenv sboo-profile-environment-variable))

;; ^ NOTE (`getenv' VAR) is:
;;
;; `nil' if: VAR is undefined in the environment;
;; ‘""’ if: VAR is set but null;
;; the value of VAR (as a string), otherwise.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcase sboo-profile-name

  ("sboo"
    (load "~/.emacs.d/sboo/sboo-default-init.el"))

  ("core"
    (load "~/.emacs.d/sboo/sboo-core-init.el"))

  ("builtins"
    (load "~/.emacs.d/sboo/sboo-builtins-init.el"))

  (_
    (load "~/.emacs.d/sboo/sboo-default-init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







      (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")  t)
      ;;;(add-to-list 'package-archives '("melpa"        . "http://melpa.milkbox.net/packages/") t)




#TODO# melpaPackages ++ stablePackages ++ optionalPackages







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sboo-dependencies)
;; 
;; ;;TODO expose command-line-arguments or environment-variables;
;; ;; `--install-dependencies' or `$SBOO_EMACS_INSTALL_DEPENDENCIES'.
;; 
;; (when ()                                ;TODO; (getenv "SBOO_EMACS_INSTALL")
;;   (sboo-configure-emacs-package-repositories!)
;;   (sboo-install-emacs-packages! t))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcase sboo-profile-name

  ((nil | "default" "sboo"
    (load "~/.emacs.d/sboo/sboo-default-init.el"))

  ("minimal"
    (load "~/.emacs.d/sboo/sboo-minimal-init.el"))

  (_
    (load "~/.emacs.d/sboo/sboo-default-init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;(setq sboo-profile-name
;;  (sboo/string->symbol
;;    (getenv sboo-profile-environment-variable)))

;;(pcase sboo-profile-name
;;  ('sboo    (load "~/.emacs.d/sboo/sboo-init.el"))
;;  ('minimal (load "~/.emacs.d/sboo/sboo-init-minimal.el")) 
;;  (_        (load "~/.emacs.d/sboo/sboo-init.el"))


(let ((*sboo-profile-string-raw* ))
  (pcase *sboo-profile-string-raw* 
    ("minimal" (setq sboo-profile-name 'minimal))))

