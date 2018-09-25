;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dante-project-root-safe-p (x)
  "Whether `dante-project-root' is a safe `sexp'.

  e.g.
      M-: (dante-project-root-safe-p '(locate-dominating-file default-directory \"cabal.project\"))
      t

      M-: (dante-project-root-safe-p '(progn (locate-dominating-file default-directory \"cabal.project\")))
      nil

      TODO
      M-: (dante-project-root-safe-p '(locate-dominating-file default-directory \"cabal-ghcjs.project\"))
      t
  "
  (or (progn
        (and (stringp x)
             x))
      
      (progn
        (and
         (equal '(locate-dominating-file default-directory "cabal.project") x)
         (locate-dominating-file default-directory "cabal.project")))))

  ;;TODO any .project

;; e.g.
;; (dante-project-root-safe-p '(locate-dominating-file default-directory "cabal.project"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante-cabal-new-repl (root)
  (when (or (directory-files root nil ".+\\.project$") (file-exists-p "cabal.project"))
    '("cabal" "new-repl" dante-target "--builddir=dist-newdante")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-dante-repl-command-line-methods-alist

  `((new-build . sboo-dante-cabal-new-repl)
    (stack     . ,(lambda (root) (dante-repl-by-file root '("stack.yaml") '("stack" "repl" dante-target))))
    (bare      . ,(lambda (_) '("cabal" "repl" dante-target "--builddir=dist/dante"))))

  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-dante-blacklist

  '("~/haskell/haskell-project-skeleton/projects"
    "~/haskell/commands"
    )

  "Directories (e.g. projects) that `dante' will ignore.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante--does-directory-contain-current-buffer? (DIRECTORY)
  ""

  (f-descendant-of?
   (f-canonical (or (buffer-file-name) ""))
   (f-canonical (expand-file-name DIRECTORY))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante--is-current-buffer-blacklisted? ()

  "Whether `buffer-file-name' is a descendant of any directory in `sboo-dante-blacklist'."

  (-any? #'sboo-dante--does-directory-contain-current-buffer?
         sboo-dante-blacklist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dante-mode-on? ()

  "Wrap `dante-mode'. 

  Don't enable on buffers whose `buffer-file-name' is not (recursively) contained by a blacklisted directory in `sboo-dante-blacklist'.
  "
  (interactive)

  (unless (sboo-dante--is-current-buffer-blacklisted?)
    (dante-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dante

  :config
  (progn

    ;; (put 'dante-project-root 'safe-local-variable
    ;;      #'dante-project-root-safe-p)
    (function-put #'locate-dominating-file 'safe-local-eval-function t)

    (setq dante-repl-command-line-methods-alist
          sboo-dante-repl-command-line-methods-alist))

  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . sboo-dante-mode-on?))

  :commands dante-mode)

  ;; ^
  ;;
  ;; i.e.:
  ;;
  ;;     :init
  ;;     (add-hook 'haskell-mode-hook #'dante-mode)
  ;;     (add-hook 'haskell-mode-hook #'flycheck-mode)
  ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;;
;; customize these variables:
;;
;; - `dante-project-root', and/or
;; - `dante-repl-command-line'.
;;
;; at these scopes:
;;
;; - file-locally, or
;; - directory-locally.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (flet ((is-contained-by-blacklisted-directory?
  ;;        (*blacklisted-directory*)
  ;;        (f-descendant-of
  ;;         (f-canonical *blacklisted-directory*)
  ;;         (f-canonical (or (buffer-file-name) "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dante)
