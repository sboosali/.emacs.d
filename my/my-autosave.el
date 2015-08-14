(provide 'my-autosave)

; saves the buffer to a backup file
(setq auto-save-timeout 1) ; save after this many seconds of idle time
(setq auto-save-interval 20) ; save after this many input events

;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html
;; "The function run-with-idle-timer returns a timer value which you can use in calling cancel-timer"
(defvar save-all-timer
  ; run callback after this many idle seconds with repeats
  (run-with-idle-timer 1 t 'save-all-file-buffers-no-prompt))

(defun save-all-file-buffers-no-prompt ()
  (save-some-buffers t nil))

(defun save-file-buffer ()
 (when (is-file-buffer)
  (basic-save-buffer)))

; (is-file-buffer "todo")    => true
; (is-file-buffer "*shell*") => false
(defun is-file-buffer ()
 (not (string-match "^\\*.*\\*$" buffer-file-name)))
