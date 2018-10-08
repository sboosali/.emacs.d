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
;;; Internal Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (add-hook 'after-init-hook #'server-start-unless-running))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-settings-widgets)
;;;  (sboo-minibuffer-config))
;;;  (sboo-config-fonts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Installation ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcase (sboo-install-p)

  ('submodules (progn
                 (sboo-register-submodule! "use-package/")
                 (sboo-register-submodule! "helm/")
                 (sboo-register-submodule! "real-auto-save/")))

  ('melpa      (progn
                 (sboo-load-file! "sboo-packages-by-installing.el")))

  ('nixpkgs    (progn))

  (_           (progn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: Core Configuration ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-load-file! "sboo-init-helm.el")
(sboo-load-file! "sboo-init-real-auto-save.el")
(sboo-load-file! "sboo-init-use-package.el")

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

    :config (setq dante-repl-command-line-methods-alist 
                  sboo-dante-repl-command-line-methods-alist)
    ())

  ;; ^ Configure `dante':
  ;;
  ;; * load `dante.el', which registers `dante-target' and `dante-project-root' as `safe-local-var's.
  ;; * `autoload' the `dante-mode' command, so we can run 《 M-x dante-mode 》 manually.
  ;; 
  ;; 

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages: (Miscellaneous) Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init)