(provide 'my-autosave)

;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html
;; "The function run-with-idle-timer returns a timer value which you can use in calling cancel-timer"
(defvar save-all-timer
  ; run callback after this many idle seconds with repeats
  (run-with-idle-timer 1 t 'save-all-file-buffers-no-prompt))

(defun save-all-file-buffers-no-prompt ()
  (save-some-buffers t nil))

; saves the buffer to a backup file
(setq auto-save-timeout 1) ; save after this many seconds of idle time
(setq auto-save-interval 20) ; save after this many input events

(defun save-file-buffer ()
 (when (is-file-buffer)
  (basic-save-buffer)))

; (is-file-buffer "todo")    => true
; (is-file-buffer "*shell*") => false
(defun is-file-buffer ()
 (not (string-match "^\\*.*\\*$" buffer-file-name)))

; doesn't work in Emacs 24.5:
;  http://emacs.stackexchange.com/questions/14706/suppress-message-in-minibuffer-when-a-buffer-is-saved
; disables all messages during save-buffer
(defun save-buffer-quiet ()
 (let (message-log-max) ; set to zero, temporarily. dynamic scope for the win
  (save-buffer)))

