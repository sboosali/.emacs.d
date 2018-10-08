;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases for Commands
;;
;; Motivation: shorten frequently-typed commands
;; (c.f. keybindings, i.e. frequently-pressed commands).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '/t 'dabbrev-expand)
;; ^ « t » for "tab-complete".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '/ic 'insert-char)

(defalias '/rq 'query-replace-regexp)
(defalias '/rs 'replace-string)

(defalias '/ar  'align-regexp)
(defalias '/ae  'align-entire)

(defalias '/ir   'indent-region)

;(defalias '/sl   'sort-lines)
;(defalias '/rr   'reverse-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '/eb   'eval-buffer)
(defalias '/er   'eval-region)
(defalias '/ed   'eval-defun)
(defalias '/el   'eval-last-sexp)

(defalias '/rb   'revert-buffer)

;; (defalias '/wm   'whitespace-mode)
;; (defalias '/gwm  'global-whitespace-mode)
;; (defalias '/glm  'global-linum-mode)
;; (defalias '/vm   'visual-line-mode)

;; (defalias '/sbgc  'set-background-color)

;; (defalias '/sh   'shell)

;; (defalias '/fb   'flyspell-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '/ras 'real-auto-save-mode)

(defalias '/pg #'projectile-grep)
(defalias '/pf #'projectile-find-file)
(defalias '/pc #'projectile-compile-project)

(defalias '/neo  'neotree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See
;;    - http://ergoemacs.org/emacs/emacs_alias.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-aliases)
