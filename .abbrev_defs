;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table '())

(define-abbrev-table 'haskell-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'jython-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'literate-haskell-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'octave-abbrev-table
  '(
   ))

(define-abbrev-table 'octave-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("r" "return")
   ))

(define-abbrev python-mode-abbrev-table
               "p"
               "print()"
               'after-abbrev-expand-hook)

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())
