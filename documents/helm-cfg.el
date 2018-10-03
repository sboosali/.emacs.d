(setq initial-scratch-message (concat initial-scratch-message
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; This Emacs is Powered by `HELM' using\n;; emacs program \"emacs\".\n;; This is a minimal `helm' configuration to discover `helm' or debug it.\n;; You can retrieve this minimal configuration in \"/tmp/helm-cfg.el\".\n;; Some original Emacs commands are replaced by their `helm' counterparts:\n\n;; - `find-file'(C-x C-f)            =>`helm-find-files'\n;; - `occur'(M-s o)                  =>`helm-occur'\n;; - `list-buffers'(C-x C-b)         =>`helm-buffers-list'\n;; - `completion-at-point'(M-tab)    =>`helm-lisp-completion-at-point'[1]\n;; - `dabbrev-expand'(M-/)           =>`helm-dabbrev'\n;; - `execute-extended-command'(M-x) =>`helm-M-x'\n\n
;; Some other Emacs commands are \"helmized\" by `helm-mode'.\n;; [1] Coming with emacs-24.4, `completion-at-point' is \"helmized\" by `helm-mode'\n;; which provides Helm completion in many places like `shell-mode'.\n;; Find context help for most Helm commands with `C-h m'.\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"))

(setq load-path (quote ("/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/site-lisp" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/site-lisp" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/vc" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/url" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/textmodes" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/progmodes" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/play" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/org" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/nxml" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/net" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/mh-e" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/mail" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/leim" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/language" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/international" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/image" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/gnus" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/eshell" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/erc" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/emulation" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/emacs-lisp" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/cedet" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/calendar" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/calc" "/nix/store/aa1g52ghahbbb732ii99zmzryi8d7zs4-emacs-26.1/share/emacs/26.1/lisp/obsolete")))
(require 'package)
;; User may be using a non standard `package-user-dir'.
;; Modify `package-directory-list' instead of `package-user-dir'
;; in case the user starts Helm from a non-ELPA installation.
(unless (file-equal-p package-user-dir "~/.emacs.d/elpa")
  (add-to-list 'package-directory-list (directory-file-name
                                        (file-name-directory
                                         (directory-file-name default-directory)))))

(setq package-load-list '((helm-core t) (helm t) (async t) (popup t)))
(package-initialize)
(add-to-list 'load-path (file-name-directory (file-truename "./submodules/helm/emacs-helm.sh")))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(blink-cursor-mode -1)
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "/tmp/helm-cfg.el") (delete-file "/tmp/helm-cfg.el"))))
