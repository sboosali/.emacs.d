;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (some of) XahLee's utilites
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-new-empty-buffer () 

  "Create a new empty buffer.
  New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
  
  It returns the buffer (for elisp programing).
  
  URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
  Version 2017-11-01" 
  
  (interactive) 
  
  (let (($buffer (generate-new-buffer "untitled")))
  
    (progn
      (switch-to-buffer $buffer)
      (funcall initial-major-mode)
      (setq buffer-offer-save t) 
      $buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((-path (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" -path)
        (browse-url -path)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" -path)
            (progn
              (let (
                    (-fpath (match-string 1 -path))
                    (-line-num (string-to-number (match-string 2 -path))))
                (if (file-exists-p -fpath)
                    (progn
                      (find-file -fpath)
                      (goto-char 1)
                      (forward-line (1- -line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fpath))
                      (find-file -fpath))))))
          (progn
            (if (file-exists-p -path)
                (find-file -path)
              (if (file-exists-p (concat -path ".el"))
                  (find-file (concat -path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -path))
                  (find-file -path ))))))))))

;; ^ this command will:
;; - open the file path under cursor, without confirmation;
;; - and jump to line number, if the path ends with ":<number>".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List Major Modes:
;;
;; You can view a list of major mode names by Alt+x `describe-variable' on `auto-mode-alist':
;; 
;;     (describe-variable 'auto-mode-alist)
;;

;; list-matching-lines
;;
;; ^ M-x `list-matching-lines' shows all lines in a buffer that match some regexp.

;; See:
;;    - http://www.wilkesley.org/~ian/xah/emacs/emacs_open_file_path_fast.html
;;    -

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-xah)