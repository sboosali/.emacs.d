(provide 'my-haskell)

; haskell-interactive-bring
; haskell-session

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(add-to-list 'auto-mode-alist '("\\.elm$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.curry$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.idr$" . haskell-mode))

;; (defun haskell-style ()
;;   "Sets the current buffer to use Haskell Style. Meant to be
;; added to `haskell-mode-hook'"
;;   (interactive)
;;   (haskell-indent-mode 1)
;;   (haskell-doc-mode 1)
;;   (setq haskell-indent-thenelse 1
;;         haskell-indent-after-keywords '(("where" 1 0)
;;                                         ("of" 1)
;;                                         ("do" 1)
;;                                         ("in" 1 0)
;;                                         ("{" 1)
;;                                         ("if" 1)
;;                                         ("then" 0)
;;                                         ("else" 0)
;;                                         ("let" 0)))
;;   (local-set-key (kbd "RET") 'newline)
;;   (setq tab-width 1))
;; (add-hook 'haskell-mode-hook 'haskell-style)

;; rebind inferior mode to interactive mode
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

;; custom haskell-mode settings
(custom-set-variables
 '(haskell-process-log t)
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-suggest-hoogle-imports t)
)

(defun my-haskell-mode-hook ()
 (haskell-session)
)

