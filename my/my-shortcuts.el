(provide 'my-shortcuts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<tab>") 'dabbrev-expand)
;; (global-set-key (kbd "<f1>") 'execute-extended-command)
;; (global-set-key (kbd "<f1>")  'execute-extended-command) ;; stupid Dragon NaturallySpeaking shortcut, it crashes when I edit the shortcuts, I should try to find any entries in the registry
(global-set-key (kbd "<f2>")  'eval-expression)
(global-set-key (kbd "<f11>") 'pp-eval-expression) ;; eval-expression
(global-set-key (kbd "<f12>") 'execute-extended-command)
;; (global-set-key (kbd "<kp-insert>") 'execute-extended-command)

(global-set-key (kbd "<kp-insert>") 'electric-buffer-list) 

(global-set-key (kbd "<pause>") 'set-mark-command)
(global-set-key "\M-r" 'query-replace-regexp)
(global-set-key "\M-`" 'previous-buffer) ;; mnemonic: it's near ALT-TAB 

;; NOTE Windows-specific 
(global-set-key (kbd "<apps>") 'execute-extended-command)

(global-set-key (kbd "C-M-m") 'maximize-frame) 

; (global-set-key (kbd "<f9>") 'pop-tag-mark)
					;(global-set-key (kbd "<f10>") 'intero-goto-definition)
;; (global-set-key (kbd "<kp-home>") 'other-window)

(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; unset
;; (global-set-key (kbd "C-x SPC") nil)

;; ;; set

;; (global-set-key "\C-o" 'compile) 

;; (global-set-key "\C-x k" 'force-kill-buffer)

;; (global-set-key "\M-T" 'transpose-paragraph)

;; (global-set-key "\M-c" 'kill-ring-save)
;; (global-set-key "\M-v" 'yank)

;; (global-set-key "\M-x" 'kill-region)
;; (global-set-key "\C-w" 'execute-extended-command)

;; (global-set-key "\M-a" 'mark-whole-buffer)

;; (global-set-key "\M-w" 'capitalize-word)

;; (global-set-key "\M-g" 'goto-line)

;; (global-set-key [(meta up)] 'beginning-of-buffer)
;; (global-set-key [(meta down)] 'end-of-buffer)

;; (global-set-key "\C-x\C-o" 'other-window)

;; (global-set-key "\M-q" 'save-buffers-kill-terminal)
;; (global-set-key "\C-xk" 'kill-this-buffer)
;; (global-set-key "\C-x\C-k" 'kill-this-buffer)
;; (global-set-key "\C-x\C-b" 'ido-switch-buffer)
;; ;;; (global-set-key "\C-x\C-b" 'electric-buffer-list)
;; ;;; (global-set-key "\C-x\C-b" 'buffer-menu)ido-switch-buffer
;; (global-set-key "\M-m" 'switch-to-buffer)
;; (global-set-key "\M-`" '"\C-xb")
;; (global-set-key "\M-s" 'save-buffer)

;; (global-set-key "\C-\\" 'ido-find-file)


;; (global-set-key "\M--" "\C-a-  -  -  -  -  -  -  -\C-j\C-q\C-l\C-e\C-j\C-j")
;; (global-set-key (kbd "M-0") nil)

;; (global-set-key "\M-i" 'ucs-insert)

;; (global-set-key [C-return] 'dabbrev-expand)
;; (global-set-key (kbd "<tab>") 'dabbrev-expand)

;; ;(global-set-key (kbd "M-<escape>") 'kmacro-start-macro-or-insert-counter)
;; ;(global-set-key (kbd "<escape>") 'kmacro-end-call-mouse)
;; ;(global-set-key "\C-<right>" 'other-window) ;TODO 'windmove-* maybe?
;; ;(global-set-key "\C-<right>" 'BACKWARDS-other-window)

;; (global-set-key "\M-r" 'query-replace-regexp)
;; (global-set-key "\C-s" 'isearch-forward-regexp)
;; (global-set-key "\C-r" 'isearch-backward-regexp)

;; (global-set-key "\M-z" 'undo) ;; f5 in xmonad
;; (global-set-key (kbd "<f6>") 'keyboard-quit)
;; ;(global-set-key (kbd "<f7>") 'flycheck-next-error)
;; (global-set-key (kbd "<f7>") 'flycheck-list-errors)
;; (global-set-key (kbd "<f8>") 'find-file-existing)
;; ;(global-set-key (kbd "<f8>") 'find-file-at-point) 

;; (global-set-key [S-mouse-2] 'browse-url-at-mouse)

;; ;(global-set-key "\M-/" 'pop-global-mark)  ;; M-. is "jump to def"
;; (global-set-key (kbd "<f11>") 'pop-tag-mark)
;; (global-set-key (kbd "<f12>") 'intero-goto-definition)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
