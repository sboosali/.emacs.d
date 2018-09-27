;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities without Dependencies.
;;
;; Both General-Purpose and Special-Purpose.
;;
;; TODO clean up, del stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Munging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-tokenize (STRING) (let ((*separators* "[-/]+") (*omit-nulls* t)) (split-string STRING *separators* *omit-nulls*)))

;; ^
;; 
;;   >>> (sboo-tokenize "some-command")
;;   ("sboo" "command")
;;
;;   >>> (mapcar #'(lambda (s) (substring s 0 1)) '("sboo" "command"))
;;   ("s" "c")
;;
;;   >>> (apply #'string (mapcar #'string-to-char '("sboo" "command")))
;;   "sc"
;;
;; NOTE `sboo-tokenize' is inlined into `.yasnippet's
;; (keep it a one-liner, for easy transfer).
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-abbreviate (STRING) (let ((*separators* "[-/]+") (*omit-nulls* t)) (apply #'string (mapcar #'string-to-char (split-string STRING *separators* *omit-nulls*)))))

;; ^
;; 
;;   >>> (sboo-abbreviate "some-command")
;;   "sc"
;;
;; NOTE `sboo-abbreviate' is inlined into `.yasnippet's
;; (keep it a one-liner, for easy transfer).
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Strings.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filesystem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun sboo-find-user-init-file ()
  (interactive)
  (find-file user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacs.stackexchange.com/questions/7475/recursively-go-up-to-find-makefile-and-compile

(defun find-file-in-ancestor-directory (FILE)
  "Traveling backwards in the filesystem from the current file's directory, find FILE and open (or activate) it in another window. (TODO returning the buffer."
  (interactive)
  (let* ((directory (locate-dominating-file default-directory FILE))
         (filepath  (concat directory FILE)))
    (progn
      (if directory
       (find-file-other-window filepath)
       (message "[find-file-in-ancestor-directory] not found: `%s`" FILE)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;;
;; https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
(defun press-C-g ()
  (interactive)
  (setq unread-command-events (listify-key-sequence "\C-g")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  

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

(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-kill-file-buffers-matching-file-extension (SUFFIX)
  ""
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-message-warning (input)

  (interactive)
  "(by @lawlist)."
  (message ;;;"%s"
    (propertize input 'face 'font-lock-warning-face)))

;; ^

;-;-;-;-;-;-;-;-;-;-;-;-;

;; See:
;;     - https://www.gnu.org/s/emacs/manual/html_node/elisp/Displaying-Messages.html
;;     - https://stackoverflow.com/questions/2742435/in-emacs-how-do-i-display-a-message-in-the-minibuffer-with-font-face-properties
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-copy-buffer-filepath-to-clipboard ()
  "Put the current file name on the clipboard.
   
   From `https://stackoverflow.com/a/2417617/1337806'. 
   "
  (interactive)

  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-restore-default-shell-command-switch ()
   "Restores the default `shell-command-switch'. 

   i.e.:

   * `-i', \"run terminal interactively\";
   * `-c', \"run the following command\".

   "
   (interactive)

   (progn
     (setq
      shell-command-switch "-ic")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-utilities)