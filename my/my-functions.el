(provide 'my-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 's)
;; (require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(with-temp-buffer (insert "abcdefg") (buffer-substring 2 4))
; ==> "bc"

(defun cons! (x xs) ; ~ add-to-list
  (setq xs (cons x xs)))

(defun key (key act)                    ;deprecate 
  (global-set-key key act))

(defun configuration ()
 (pp (current-frame-configuration)))

;; (defun current-line ()
;;  (let ((line (thing-at-point 'line)))
;;   (with-temp-buffer
;;    (insert (substring-no-properties line 0 (length line)))
;;    (delete-trailing-whitespace)
;;     ; to strip final whitespace
;;    (buffer-substring 0 (- (length (buffer-string)) 1))
;;    )))

(defun current-line ()
 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun string/ends-with (string suffix)
      "Return t if STRING ends with SUFFIX."
      (and (string-match (rx-to-string `(: ,suffix eos) t)
                         string)
           t))

(defun my/ends-with (string suffix)
 (let
  ((n (length string))
   (k (length suffix)))
  (string-equal suffix (substring string (- n k) n))))

(defun string/starts-with (s begins)
      "Return non-nil if string S starts with BEGINS."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))

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

(defun google-region (start end)
 (interactive "r")
 (let ((lines (lines-region start end)))
  (mapc 'google-it lines)))

(defun google-it (it)
 (browse-url (concat "http://www.google.com/search?q=" it))
 (sit-for (+ 3 (random 1))))

(defun lines-region (start end)
 (let ((region (buffer-substring start end)))
   (--filter (s-present? (s-trim it))
	     (s-lines region))))

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

(defun show-dot-emacs-structure ()
      "Show the outline-mode structure of ~/.emacs"
      (interactive)
      (occur "^;;;;+"))

;; (defun my/find-file () (interactive)
;;  (if (= 1 (count-windows))
;;   (ido-find-file)
;;   (ido-find-file-other-window)))

;; (defun my/find-file () (interactive)
;;  (if (= 1 (count-windows))
;;      (find-file-at-point)
;;    (let ((filename (ffap-file-at-point)))
;;      (if filename
;; 	 (progn
;; 	   (other-window 1)
;; 	   (find-file filename))
;;         (find-file)))))

;; (defun my/find-file ()
;;   (interactive)
;;   (ido-find-file))

;; (defun my/find-file ()
;;   (interactive)
;;   (ffap))

;; before: (getenv "EMACSPATH")
(defun when-app (name initialize)
  (if (string-match name (expand-file-name invocation-directory))
      (funcall initialize)))

(defun when-host (name initialize)
  (if (string-match name (system-name))
      (funcall initialize)))

; I forget why
(defun set-exec-path-from-shell-path ()
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$"
          ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun any-buffers-modified? ()
 (--any? (buffer-modified-p it)
  (--filter (buffer-file-name it)	; non-nil if the buffer is visiting a file
   (buffer-list))))

;; (add-hook 'haskell-mode-hook (lambda()
;; 			       (unless (string= "/home/sboo/.xmonad/xmonad.hs" (buffer-file-name (current-buffer)))
;; 				 (intero-mode))))

; (desperately-compile "Makefile" "make -k")
(defun desperately-compile (f c)
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory f)
  (with-temp-buffer
    (cd (locate-dominating-file default-directory f))
    (compile c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-region-or-buffer ()
  (interactive)
  (let ((debug-on-error t))
    (cond
     (mark-active
      (call-interactively 'eval-region)
      (message "Region evaluated!")
      (setq deactivate-mark t))
     (t
      (eval-buffer)
      (message "Buffer evaluated!")))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-i") 'eval-region-or-buffer)))

(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see http://ivanmalison.github.io/dotfiles/

(defmacro make-interactive-function (function)
  `(lambda (&rest args)
     (interactive)
     (apply ,function args)))

(defmacro measure-time-of (&rest body)
  "Measure the running time of the given code block, returning the result."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun get-last-message (&optional num)
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
    (forward-line (- 1 num))
    (backward-char)
    (let ((end (point)))
      (forward-line 0)
      (buffer-substring-no-properties (point) end))))))

(defun random-choice-from (choices)
  (nth (random (length choices)) choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'color-theme)

(defun darkroom-mode ()
	"Make things simple-looking by removing decoration 
	 and choosing a simple theme."
        (interactive)
;        (switch-full-screen 1)     ;; requires above function 
	(color-theme-retro-green)  ;; requires color-theme
;        (setq left-margin 10)
;        (menu-bar-mode -1)
;        (tool-bar-mode -1)
;        (scroll-bar-mode -1)
        (set-face-foreground 'mode-line "gray15")
        (set-face-background 'mode-line "black")
        (auto-fill-mode 1)
)

(defun darkroom-mode-reset ()
   (interactive)
;   (switch-full-screen -1)
   (color-theme-subtle-hacker) ;; Choose your favorite theme
;   (menu-bar-mode 1)
;   (tool-bar-mode 1)
;   (scroll-bar-mode 1)
;   (set-left-margin 0)
)	  

(defun maximize-frame () (interactive) 
  (set-frame-parameter nil 'fullscreen 'maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
