(provide 'my-edit-server)
;; http://www.emacswiki.org/emacs/Edit_with_Emacs
;;; seems to work only with <textarea>s, but not e.g. <input>s

(setq edit-server-new-frame nil)

;; save every edit by copying, because most websites don't persist
;; your text. Just in case
(add-hook 'edit-server-done-hook (lambda () (kill-ring-save (point-min) (point-max))))

;;; for compatibility with Gmail's compose window
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
;;; (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;;; for copying and pasting
(add-hook 'edit-server-start-hook (lambda ()
 (insert "edit-server-done")
 (sleep-for 1)
 (kill-line 0)
))

(eval-after-load "edit-server-mode" '(progn
 (define-key edit-server-mode-map "\C-x\C-k" (lambda () (interactive) (edit-server-done nil nil)))
 ;; (define-key edit-server-mode-map "\C-x\C-k" "\C-c\C-c")
))

