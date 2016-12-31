(provide 'my-deft)
(require 'deft)

(setq deft-extension "note")
(setq deft-directory "~/Dropbox")

(setq deft-use-filename-as-title t)
;(setq deft-text-mode 'markdown-mode)
(setq deft-separator "")

(global-set-key [f9] 'deft)

; dynamic scope...
(defun deft-parse-summary (contents title) "")
