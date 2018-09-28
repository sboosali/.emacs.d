;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for registering `load-path's.
;;
;; (for `sboo-*' features **and** for bootstrapping `init.el' itself).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-add-to-load-path (BaseDirectory &optional RegisterBaseDirectory SubDirectoryNames)

  "Register the subdirectories `SubDirectoryNames' of `BaseDirectory' onto the `load-path'. 

   Wraps `normal-top-level-add-to-load-path'.

   Arguments:

   * `BaseDirectory': a string. a filepath relative to `user-emacs-directory'.

   * `SubDirectoryNames': a list of strings. a whitelist of directory names (no trailing slash required). `nil' means no whitelist, i.e. all subdirectories.

   * `RegisterBaseDirectory': a boolean. Whether to also register `BaseDirectory` itself.
  "

  (let* ((*emacs-directory* (file-name-as-directory (expand-file-name (or user-emacs-directory "~/.emacs.d/"))))
         (*base-directory*  (file-name-as-directory (concat *emacs-directory* BaseDirectory))))

    (when RegisterBaseDirectory
        (add-to-list 'load-path *base-directory*))

    (let* ((default-directory *base-directory*))
      (normal-top-level-add-to-load-path SubDirectoryNames))))

;; ^ 
;; 
;; TODO `normal-top-level-add-subdirs-to-load-path' versus `normal-top-level-add-to-load-path'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-load-path)