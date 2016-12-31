(provide 'my-compilation)

;;; 
(setq compilation-filenames '("Makefile" "makefile"))
(defun get-nearest-compilation-path ()
  "Search for the compilation file traversing up the directory tree."
  (let ((dir default-directory)
	(parent-dir (file-name-directory (directory-file-name default-directory)))
	(nearest-compilation-path 'nil))
    (while (and (not (string= dir parent-dir))
		(not (string= dir (expand-file-name "~")))
		(not nearest-compilation-path))
      (dolist (filename compilation-filenames)
	(setq file-path (concat dir filename))
	(when (file-readable-p file-path)
	  (setq nearest-compilation-path dir)))
      (setq dir parent-dir
	    parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-path))

(setq compilation-auto-jump-to-first-error t)
(setq next-error-highlight t)
(setq next-error-follow-minor-mode t)
(setq compilation-read-command nil)
;; (setq compile-command "cd ~/voice/commands-core/; make")
;;; make-local-variable, a buffer-local variable
(add-hook 'prog-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (format "cd %s ; make" (get-nearest-compilation-path)))))
;; (setq-local compile-command (format "cd %s ; make" (get-nearest-compilation-path)))

;;; compilation-minor-mode
(add-hook 'compilation-mode-hook (lambda () (progn
 ;(define-key compilation-mode-map (kbd "<mouse-1>") 'compile-goto-error)
 ;(define-key compilation-mode-map (kbd "RET") 'comint-send-input)
)))

