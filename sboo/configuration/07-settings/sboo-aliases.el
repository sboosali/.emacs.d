;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases for Commands
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;
;; make frequently used commands short:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'de   'dabbrev-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'qrr  'query-replace-regexp)
(defalias 'rs   'replace-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'ar  'align-regexp)
(defalias 'ae  'align-entire)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'g    'grep)
(defalias 'gf   'grep-find)
(defalias 'fd   'find-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'rb   'revert-buffer)
(defalias 'sbc  'set-background-color)
(defalias 'rof  'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'sh   'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'lml  'list-matching-lines)
(defalias 'dml  'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw  'delete-trailing-whitespace)
(defalias 'sl   'sort-lines)
(defalias 'rr   'reverse-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defalias 'fb   'flyspell-buffer)
;; (defalias 'lcd  'list-colors-display)
;; (defalias 'cc   'calc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; elisp

(defalias 'eb   'eval-buffer)
(defalias 'er   'eval-region)
(defalias 'ed   'eval-defun)
(defalias 'els  'eval-last-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'lf   'load-file)

;; (defalias 'eis  'elisp-index-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; major modes

(defalias 'tm   'text-mode)

;; (defalias 'elm  'emacs-lisp-mode)
;; (defalias 'om   'org-mode)
;; (defalias 'ssm  'shell-script-mode)
;; (defalias 'hm   'html-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; minor modes
;; (defalias 'wsm  'whitespace-mode)
;; (defalias 'glm  'global-linum-mode)

;; (defalias 'gwsm 'global-whitespace-mode)
;; (defalias 'vlm 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous

(defalias 'ic 'insert-char)
;; ^ e.g.:
;;
;;     M-x insert-char RET identical to RET
;;     ;; ≡
;;
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3rd-Party Packages

(defalias 'ras 'real-auto-save-mode)

(defalias 'neo  'neotree)
(defalias 'neor 'neotree-refresh)

(defalias 'godl 'god-local-mode)
(defalias 'godg 'god-mode-all)

(defalias 'dr 'dante-restart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; See
;;    - http://ergoemacs.org/emacs/emacs_alias.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-aliases)