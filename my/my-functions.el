(provide 'my-functions)


(put 'downcase-region 'disabled nil)
(defun save-macro (name)                  
    "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro    
     (kmacro-name-last-macro name)         ; use this name for the macro    
     (find-file user-init-file)            ; open ~/.emacs or other user init file 
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro 
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer

(defun indent-and-next () (interactive)
  (move-beginning-of-line 1)
  (insert " ")
  (next-line)
  (move-beginning-of-line 1))
(global-set-key "\M-j" 'indent-and-next)

(defun kill-line-save () (interactive)
  (kill-line)
  (yank))
(global-set-key "\M-k" 'kill-line-save)

(defun force-kill-buffer () (interactive)
  (kill-buffer (buffer-name)))
(global-set-key "\C-x k" 'force-kill-buffer)

(defun delete-line ()
  "Deletes a line, but preserves the kill-ring."
  (interactive)
  (if (not (equal (point) (point-max))) ; end of buffer
      (kill-line)
    (setq kill-ring (cdr kill-ring))))

(defun new-note () (interactive)
  (end-of-buffer)
  (backward-paragraph)
  (forward-paragraph)
  (delete-line) (delete-line) (delete-line) (delete-line)
  (newline) (newline))
(global-set-key "\M-n" 'new-note)

(defun transpose-paragraph () (interactive)
 (backward-paragraph)
 (kill-paragraph)
 (backward-paragraph)
 (yank))

(defun align-repeat (start end regexp)
 "Repeat alignment with respect to the given regular expression."
 (interactive "r\nsAlign regexp: ")
 (align-regexp start end 
  (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun lines-region (start end)
 (let ((region (buffer-substring start end)))
  (split-string region "[\n]")))

(defun google-region (start end)
 (interactive "r")
 (let ((lines (lines-region start end)))
  (mapc 'google-it lines)))

(defun google-it (it)
 (browse-url (concat "http://www.google.com/search?q=" it))
 (sit-for (+ 3 (random 1))))

(defun spiros-log () (message "post-command-hook"))
;(add-to-list 'post-command-hook 'spiros-log)

(defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))
  
(defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))

(defun wc () (interactive) (shell-command (concat "wc " buffer-file-name)))

(defun shift-region (distance)
  (interactive (list
   (read-string (format "Number of spaces to shift (negative means left) (default: %d): " 1) nil nil 1 nil)))
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-region-right ()
  (interactive)
  (shift-region 1))

(defun shift-region-left ()
  (interactive)
  (shift-region -1))

(defun frontmost ()
  (message "frontmost")
  (let ((script
         (concat
            "tell application \"Emacs\"\n"
            " set frontmost to true\n"
            "end tell\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

