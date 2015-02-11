(provide 'my-notes)


(defun note-hook ()
 (when (is-note-file)
  (note-mode)))

(add-hook 'find-file-hook 'note-hook)

(defun note-mode ()
 (set-input-method "TeX") ; inline latex. inserts unicode.
 (goto-address-mode)
 (setq comment-start ".")
 (setq comment-end "")
 (setq coding-system 'utf-8)
)

(defun is-note-file ()
 (or
  (not (string-match "\\." buffer-file-name)) 
  (string-match "\\.note$" buffer-file-name)))
