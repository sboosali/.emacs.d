;; (require ')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-mode ()
  (interactive)
  (toggle-read-only)
  (setq auto-revert-interval 1)
  (auto-revert-mode))

(setq shared-folder-transcription-file-regular-expression
 "transcription\\.txt\\'")

(add-to-list 'auto-mode-alist
 (cons shared-folder-transcription-file-regular-expression 'refresh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dictation)
