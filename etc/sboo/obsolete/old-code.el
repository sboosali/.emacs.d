














(require 'sboo-aliases)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-xah)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-settings-widgets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-settings-bookmarks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-settings-minibuffer nil t)
  (sboo-minibuffer-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-settings-fonts nil t)
  (sboo-config-fonts))
















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-directory

  (file-name-as-directory (concat emacs-directory "sboo/"))

  "The root directory of my personal configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lisp-directory

  (file-name-as-directory (concat emacs-directory "lisp/"))

  "The root directory of any vendored lisp files.")














;;(load (concat emacs-directory (file-name-as-directory "sboo/") "sboo-init.el"))














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `use-package' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile

  (progn
    (sboo-add-subdir-to-load-path "submodules" nil '("use-package"))
    (sboo-add-subdir-to-load-path "elpa"       nil '("use-package-2.3")))

  ;; ^ i.e. "submodules/use-package" (if available) shadows "elpa/use-package-*". TODO check this
  ;;
  ;; or Choose one (i.e. uncomment):
  ;;
  ;; - vendored (via `git-sumbodule').
  ;; - installed (via `package-install'ed).
  ;;

  (require 'use-package))

  ;; ^ `use-package' is a macro. As such,
  ;; it's a compile-time dependency (i.e. not run-time).
  ;; 













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package helm
;;   :init
;;   (progn
;;     (setq helm-mode-fuzzy-match                 t)
;;     (setq helm-completion-in-region-fuzzy-match t)
;;     ;; ^ fuzzy-matching.
;;    ;; helm-boring-buffer-regexp-list '()
;;    ;; ;; ^
;;    ;; ;; by default, `helm-boring-buffer-regexp-list' is:
;;    ;; ;;     ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")
;;    ;; ;;
;;     (setq helm-allow-mouse t)
;;     ;; ^ the mouse is gratuitously disabled by default.
;;     ;; this enables, for example, clicking on a helm candidate to activate it,
;;     ;; rather than navigating it with several arrow and/or character keypresses.
;;     ;;
;;     nil)
;;   :bind (:map helm-command-map
;;               ("<f9>" . helm-quit-and-helm-mini))
;; ;   ("C-x C-f" .  helm-find-files)
;;   :config
;;   (helm-mode 1))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-helm! ()

  "Require `helm' and configure `helm-mode'.

  Idempotent (invoking it twice is the same as calling once).

  Safe (doesn't throw an error when the package can't be `load'ed).
  "
  (interactive)

  (when (and (require 'helm-config nil t) (require 'helm nil t))
    (helm-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

;; (global-set-key (kbd "M-x")
;;                 #'helm-M-x)

;; (global-set-key (kbd "C-x r b")
;;                 #'helm-filtered-bookmarks)

;; (global-set-key (kbd "C-x C-f")
;;                 #'helm-find-files)

;;; ^ Helm provides generic functions for completions to replace tab-completion in Emacs, with no loss of functionality.







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LoadPaths: External Packages ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-add-subdir-to-load-path "elpa" nil

 '("helm-3.0"
   "helm-core-3.0"    ; `helm` dependency
   "async-1.9.3"      ; `helm` (transitive) dependency
   "popup-0.5.3"      ; `helm` (transitive) dependency
 ))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization: External Packages ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (require 'sboo-helm nil t)

  (progn

    (sboo-init-helm!)

    (add-hook 'after-init-hook
              #'sboo-config-helm!))

  (message "[sboo] can't find %s." 'sboo-helm))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration: External Packages ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'use-package nil t)  

  (require 'sboo-haskell nil t)

  ())
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty-Print Information (via `message')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (message ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")

  (dolist (@x load-path)
    (message "[load-path] %s" @x))
  
  ;; ^ pretty-print the `load-path'.

  (message ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")

  (dolist (@x features)
    (message "[feature]   %s" @x))
  
  ;; ^ pretty-print `features' (i.e. loaded packages).

  (message ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")

  ())




















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation: External Packages ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-list

  '((use-package    . "2.3")
    (helm           . "3.0")
    (real-auto-save . t)
    (yasnippet      . t)
    (projectile     . t)
    (haskell-mode   . t)
    (dante          . t)
    )

 "Packages that must be installed.")







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (require 'package)

  (setq package-enable-at-startup nil)

  (setq package-archives sboo-package-archives)

  (package-initialize)

  (dolist (pv sboo-package-list)
    (pcase pv
      (`(,p . ,v)
        (package-install p))

  ())


















;;(when (and (>= emacs-major-version 24)
;;          (require 'sboo-packages nil t))
;;  (if sboo-install-p
;;      (sboo-packages-install!)     ;; install and activate
;;      (sboo-packages-activate!)))  ;; only activate













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-directories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-add-to-load-path sboo-directory t)
(sboo-add-to-load-path lisp-directory t)

;; ^ "Which (sub)directories to register under the `load-path'."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-add-to-load-path cloned-package-directory    nil 
  '("dante"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-add-to-load-path installed-package-directory nil
  '("dante"
    "dante"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-macros)
(require 'sboo-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths: `sboo' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-conditions)

  ;; ^ `package-install'ed packages.













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-directories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-load-path  ;;TODO

  (list sboo-directory
        lisp-directory
        cloned-package-directory
        installed-package-directory
  )

  "Which subdirectories to register under the `load-path'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (@directory sboo-load-path)

  (sboo-add-to-load-path @directory t))














(use-package dante
  
  :commands dante-mode

  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . dante-mode))

  :config (progn
            (setq dante-repl-command-line-methods-alist sboo-dante-repl-command-line-methods-alist)
            ()))











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects: Boostrapping ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (@file *sboo-bootstrap-filenames*)

  (load (concat *sboo-sboo-root* @file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-add-to-load-path "sboo" t)

(sboo-add-to-load-path "elpa"       nil '("helm-3.0" "helm-core-3.0" "popup" "async"))
(sboo-add-to-load-path "submodules" nil '("use-package"))











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-haskell-mode)
;; ^
;; my baseline (and the standard) `haskell`-language configuration. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-dante)
;; ^
;; my `dante` configuration.
;; `dante` is a lightweight, configurable Haskell IDE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sboo-lsp)
;; ;; ^
;; ;; `haskell-ide-engine' uses `LSP'.
;; ;;
;; ;;

;; (use-package dante
;;   :commands dante-mode)
;; ;; ^
;; ;; load `dante.el', which registers `dante-target' (&al) as `safe-local-var'(s).
;; ;; autoload `dante-mode', so we can run 《 M-x dante-mode 》.
;; ;;
;; ;; but don't **configure** it, i.e. no `hook's;
;; ;; when this statement is uncommented, we're using `lsp-haskell'.
;; ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'sboo-intero)
;; ^
;; my `intero` configuration.
;; `intero` is a `stack`-only Haskell IDE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'sboo-ghcid)
;; ^
;; my custom `ghcid`-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)



















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-root

  (file-name-as-directory (expand-file-name (concat user-emacs-directory "sboo")))

  "The root directory of my configuration (for bootstrapping).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (@file '(
                "sboo-directories.el
                "sboo-load-path.el
                ))

  (load (concat sboo-root @file)))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap `load-path's.
;;
;; (for `sboo-*' features **and** for `init.el' itself).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-add-to-load-path (BaseDirectory &optional RegisterBaseDirectory SubDirectoryNames)

  "Register all descendant directories (i.e. recursive subdirectories) of `BaseDirectory' onto the `load-path'. 

   Wraps `normal-top-level-add-to-load-path'.

   Arguments:

   * `BaseDirectory': a string. a filepath relative to `user-emacs-directory'.

   * `SubDirectoryNames': a list of strings. a whitelist of directory names (no trailing slash required). `nil' means no whitelist, i.e. all subdirectories.

   * `RegisterBaseDirectory': a boolean. Whether to also register `BaseDirectory` itself.
  "

  (let* ((*emacs-directory* (file-name-as-directory (expand-file-name (or user-emacs-directory "~/.emacs.d/"))))
         (*base-directory*  (file-name-as-directory (concat *emacs-directory* BaseDirectory))))

    (when RegisterBaseDirectory
        (add-to-list 'load-path *base-directory*))

    (let* ((default-directory *base-directory*))
      (normal-top-level-add-to-load-path SubDirectoryNames))))

;; ^ 
;; 
;; TODO `normal-top-level-add-subdirs-to-load-path' versus `normal-top-level-add-to-load-path'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-load-path)











(progn
  (sboo-add-to-load-path "submodules" nil '("use-package"))
  (sboo-add-to-load-path "elpa"       nil '("use-package-2.3")))
  
  ;; ^ i.e. "submodules/use-package" (if available) shadows "elpa/use-package-*". TODO check this









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration: Features ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-haskell nil t)
  ())











;; [C-x b] was originally:
;; (switch-to-buffer BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (load-path! ./sboo)
  (load-path! ./sboo/installation)
  (load-path! ./sboo/initialization)
  (load-path! ./sboo/configuration)
  (load-path! ./sboo/configuration/02-platforms)
  (load-path! ./sboo/configuration/03-window-systems)
  (load-path! ./sboo/configuration/04-utilities)
  (load-path! ./sboo/configuration/05-keybindings)
  (load-path! ./sboo/configuration/06-initialization)
  (load-path! ./sboo/configuration/07-settings)
  (load-path! ./sboo/configuration/10-internal-packages)
  (load-path! ./sboo/configuration/20-my-packages/dictation)
  (load-path! ./sboo/configuration/25-vendored-packages)
  (load-path! ./sboo/configuration/30-external-packages)
  (load-path! ./sboo/configuration/35-external-configurations)
  (load-path! ./sboo/configuration/50-meta-configurations)
  ())

  ;; ^ `sboo' package (TODO flatten).








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths: Bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((*emacs-directory* 
                          (or user-emacs-directory "~/.emacs.d/"))
       (*sboo-directory*
                          (expand-file-name (concat *emacs-directory* "sboo"))))

  (add-to-list 'load-path *sboo-directory*))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for an Emacs Server.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-start-unless-running ()

  "Run `server-start` unless another Emacs Server is already running.
  
  Platform-Compability: UNIX only.
  
  By @markhellewell
  "
  (interactive)

  (let* ((tmp
          "/tmp")
          ;; (getenv "TMPDIR"))
          ;; ;; ^ i.e. "$TMPDIR"
         (uid
          (number-to-string (user-real-uid)))
          ;; ^ i.e. "$UID"
         (server-socket-file
          (concat tmp "/" "emacs" uid "/" "server")))
          ;; ^
          ;; e.g. "/tmp/emacs1001/server"
          ;; i.e. "/tmp/emacs<UserId>/<ServerName>"

  (unless (file-exists-p server-socket-file)
    (server-start)))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load (a whitelist of) my configurations for vendored packages.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'sboo-go-back)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-vendored)












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================================================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-use-package)       ;TODO
;; ^ my `use-package` configuration.
;; Most `sboo-*` features call `use-package`.








;; (defun server-start-once ()
;;   "Start an Emacs Server for the `emacsclient` command, 
;;   unless a server is already running.
;;   We check for the presence of another Emacs Server 
;;   via the existence of a specific socket; for example,
;;   named \"/tmp/emacs1001/server\".
;;   `server-start-once` is idempotent, modulo race conditions."
;;   (interactive)
;;   (let 
;;       ( (server-socket-file "/tmp/emacs1001/server") ;;TODO shell out for "$UID"
;;          ;; ^
;;          ;; e.g. "/tmp/emacs1001/server"
;;          ;; i.e. "/tmp/emacs<UserId>/<ServerName>"
;;          ;; 
;;          ;; NOTE when evaluated from a running Emacs Server,
;;          ;; `server-socket-file` should equal `(concat server-socket-dir server-name)`
;;          ;; (otherwise, some `server-*` variables are undefined).
;;       )
;;     (unless (file-exists-p server-socket-file)
;;       ;; ^ check whether another server (named `server-name`) is already running.
;;       (server-start)))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-server)








;;TODO
;; (set-face-attribute 'default nil :font "Garamond" :height 200)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-font (FONT)
  "Sets the font of the `current-buffer' to `FONT`. 
  `FONT` is a string.

  e.g. `M-: (sboo-set-font \"Iosevka\")`
  e.g. `M-x sboo-set-font RET Iosevka`
  "

  (interactive "sFont name [C-h v font-family-list]: ") ;;TODO `try-completion; from `font-family-list'.
  
  ;; ^ `"s"` means "read a string from the user until they press RET".

  (if (find-font (font-spec :name FONT))
      ;; ^ 
      ;; e.g. (find-font (font-spec :name "iosevka"))
      ;; e.g. (find-font (font-spec :name "garamond"))
    
    (progn
      (buffer-face-set `(:family ,FONT))
      (buffer-face-mode)
      t)
    
    nil))

;; ^
;; Starting with Emacs 23, you can set the face for the current buffer, using ‘M-x buffer-face-set’. You can toggle this on/off using ‘M-x buffer-face-mode’. 
;; Internally, this uses ‘face-map-add-relative’ to remap faces. For example, (face-remap-add-relative 'default :family "Source Code Pro" :height 140)
;; 
;; also, you can list all fonts with `M-: (print (font-family-list))`.
;;

;;OLD;;
;;                            (setq buffer-face-mode-face '(:family "Inconsolata"))
;;                            (buffer-face-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun sboo-set-font-to-iosevka ()
;;    "Sets a custom font for code, `Iosevka`, in the current buffer, if present.
;;    By @HanfeiSun.
;;    By /u/MrTJC."
;;    (interactive)

;;    (cond
    
;;     ((find-font (font-spec :name "Iosevka"))
;;      (progn
;;        (setq
;;         buffer-face-mode-face '(:family "Iosevka"))
;;        (buffer-face-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-fonts)









;; TODO

;; (add-to-list 'safe-local-eval-forms
;;              '(locate-dominating-file default-directory "cabal.project"))

;; (add-to-list 'safe-local-variable-values
;;              ('dante-project-root
;;               . '(locate-dominating-file default-directory "cabal.project")))

;;TODO (put 'dante-project-root 'safe-local-variable (== (list 'locate-dominating-file 'default-directory "cabal.project")))
;; (put 'dante-project-root 'safe-local-variable #'stringp) is in dante.el
;;

;; ^ for `.dir-locals.el' & `dante'.
;; 

(provide 'sboo-settings-dirlocals)








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

