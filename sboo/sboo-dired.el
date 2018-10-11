;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (require 'dired)

  (setq dired-listing-switches "-l --recursive --almost-all --ignore-backups --human-readable --group-directories-first")

  ;; ^ `ls' options (must include ‘-l’).
  ;;
  ;; * `--recursive': list subdirectories recursively.
  ;; * `--almost-all': omit « ./ » and « ../ »
  ;; * `--ignore-backups': omit « ~ »-suffixed files.
  ;; * `--human-readable': print sizes like 1K, 234M, 2G, etc (needs `-l' and `-s').
  ;;

  (setq dired-auto-revert-buffer t)


  ;; ^ revert Dired buffers automatically.

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 26)
  
  (require 'wdired)

  (setq wdired-create-parent-directories t)


  ;; ^ create new subdirectories, when adding slashes or editing (sub)directory names.
  ;;
  ;; i.e. when editing filenames with slash characters,
  ;; and when adding slash characters to filenames,
  ;; automatically create the induced directories.
  ;;

  ())

;; ^
;;
;; NOTE 'W' gets bound to `browse-url-of-dired-file' (e.g. for viewing HTML files).
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dired-descendants ()
  ""
  (interactive)
  ())  ;; TODO find-name-dired?

  (let ((include-directories t)
         (filepath-blacklist-regexp ""))
    (directory-files-recursively default-directory filepath-blacklist-regexp include-directories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Invoke Dired 
;;
;; * M-x dired
;; * C-x d
;;
;; 

;; Dired Keymap
;;
;; ‘i’: Insert Subdirectory (in Dired buffer). i.e. expand its contents, or descend into it.
;; ‘^’: Ascend into the Parent Directory.
;;

;; M-x `find-name-dired'
;;
;; reads arguments DIRECTORY and PATTERN,
;; finds all files in DIRECTORY or (its subdirectories) whose individual names match PATTERN,
;; then displays them in a single Dired buffer.
;;

;; M-x `find-grep-dired'
;;
;; 

;; See:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-Enter.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Subdirectories-in-Dired.html#Subdirectories-in-Dired
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-and-Find.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
;; - https://emacs.stackexchange.com/questions/33332/recursively-list-all-files-and-sub-directories
;; - http://man7.org/linux/man-pages/man1/ls.1.html
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dired)