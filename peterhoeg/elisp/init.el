;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
;; ^ the standard keybindings: C-c, C-x, C-v, C-z.

(setq cua-keep-region-after-copy t) 
;; ^ Standard Windows behaviour

(transient-mark-mode 1) 
;; ^ No region when nothing is highlighted.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;