(provide 'my-haskell)
(require 'etc)
(require 'align)
(require 'speedbar)

; package-install flycheck
; Symbol's function definition is void: "macroexp-progn"

; git clone https://github.com/nilcons/hi2

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode) ;; still sucks
;;; sub word mode lets you navigate (e.g. M-b) between "sub words" of a camelcased word
(add-hook 'haskell-mode-hook 'subword-mode)

; haskell-interactive-bring
; haskell-session

; haskell-process-do-type
; haskell-process-do-info

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

(eval-after-load "haskell-mode" '(progn
 ;; rebind inferior mode to interactive mode
 (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
 (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
 (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
 (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
 (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
 (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
 (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
 (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
 (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
 ;; my bindings
 (define-key haskell-cabal-mode-map (kbd "M-u") 'haskell-process-cabal-build)
))

;(key (kbd "M-u") 'haskell-process-cabal-build) ; bound to 'compile instead

;; custom haskell-mode settings
(custom-set-variables
 '(haskell-process-log t)
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 ;'(haskell-process-suggest-hoogle-imports t)
 ;'(haskell-stylish-on-save t)
 ;'(haskell-tags-on-save t)
)

; put cursor at my place and run: align
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

(defun my-haskell-mode-hook ()
 (haskell-session)
 ;(haskell-process-cabal-build)          ; compilation-shell-minor-mode
 ;(turn-on-haskell-simple-indent)
 ;(turn-on-haskell-indentation)
 ;(turn-on-haskell-doc-mode)
 ;(global-linum-mode 1)
)

(speedbar-add-supported-extension ".hs")

;(global-set-key [mouse-1] 'haskell-process-do-info)
