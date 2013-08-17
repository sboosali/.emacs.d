;;;;;;;;;;;;;;; reed's init.el ;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; packages

;; Add marmalade repos
(require 'package)
(add-to-list 'package-archives
             ;'("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit smex undo-tree magit solarized-theme smart-tabs-mode))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Use smex for M-x
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

;; Prevent Emacs from extending file when
;; pressing down arrow at end of buffer.
(setq next-line-add-newlines nil)
;; Silently ensure newline at end of file
;; (setq require-final-newline t)
;; or make Emacs ask about missing newline
(setq require-final-newline nil)


;;;;;;;;;;;;;;; cheetah mode
(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  (setq indent-tabs-mode 1)
  (setq tab-width 4))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . cheetah-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))


;;;;;;;;;;;;;;; yelp tabs
(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'smart-tabs-mode)
(smart-tabs-advice python-indent-line-1 python-indent)
(defun my-yelp-python-mode-hook ()
	  (setq tab-width 4)
	  (setq py-indent-offset 4)
          (let ((yelp-project-p (and buffer-file-name (or (string-match "/pg/yelp-main/" (buffer-file-name))
                                                          (string-match "/pg/clientlibs/" (buffer-file-name))
                                                          (string-match "/pg/submodules" (buffer-file-name))
                                                          (and (string-match "/pg/services/" (buffer-file-name))
                                                               (not (or (string-match "/zygote/" (buffer-file-name))
                                                                        (string-match "/rate_tracker/" (buffer-file-name))
                                                                        (string-match "/google/" (buffer-file-name)))))))))
        (when yelp-project-p
          (setq indent-tabs-mode yelp-project-p)
          (smart-tabs-mode)))

	  ;; (if indent-tabs-mode (setq tab-width 3)) ;; for funsies
	  (setq py-indent-offset tab-width)
	  (setq python-indent tab-width)
	  (setq py-smart-indentation nil)

	  (setq-default show-trailing-whitespace t)
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)

	  (message "yelp python mode, tabs is %s" indent-tabs-mode))

(add-hook 'python-mode-hook 'my-yelp-python-mode-hook)

