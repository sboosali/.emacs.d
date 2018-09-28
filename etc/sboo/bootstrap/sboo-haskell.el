;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante-cabal-new-repl (root)

  "Locate a `cabal.project' file.
  "
  (interactive)

  (when (or (directory-files root nil ".+\\.project$") (file-exists-p "cabal.project"))

    '("cabal" "new-repl" dante-target "--builddir=dist-newdante")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-dante-repl-command-line-methods-alist

  `((new-build . sboo-dante-cabal-new-repl)
    (stack     . ,(lambda (root) (dante-repl-by-file root '("stack.yaml") '("stack" "repl" dante-target))))
    (bare      . ,(lambda (_) '("cabal" "repl" dante-target "--builddir=dist/dante"))))

  "Override `dante-repl-command-line-methods-alist'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante-mode ()
  
  "Start/restart `dante'.
  
  (i.e. `dante-mode' or `dante-restart').
  "
  (interactive)

  (if (bound-and-true-p 'dante-mode)

    (dante-restart)

   (dante-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dante
  
  :commands dante-mode

;;;  :hook ((haskell-mode . flycheck-mode)
;;;         (haskell-mode . dante-mode))

  :bind (:map haskell-mode-map
              (("" . sboo-dante-mode)))

  :config (progn
            (setq dante-repl-command-line-methods-alist sboo-dante-repl-command-line-methods-alist)
            ()))

;; ^ Configure `dante':
;;
;; * load `dante.el', which registers `dante-target' and `dante-project-root' as `safe-local-var's.
;; * `autoload' the `dante-mode' command, so we can run 《 M-x dante-mode 》 manually.
;; 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)