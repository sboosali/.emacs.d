;;; sboo-keymaps.el --- Personal keymaps -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 13 Jun 2019
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal `keymapp's.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  (cl-defmacro defun-dired (name directory &key doc)

    "Define a command which launches `dired' at DIRECTORY.

• NAME — (unquoted) `symbolp'.
  the function name.
• DIRECTORY — a `stringp'.
  a directory filepath.
• DOC — a `stringp'.
  the function documentation.
  (defaults to the empty string.)

Output:

• an `interactive' `defun' declaration.

Example:

• M-: (pp-macroexpand-expression (defun-dired sboo-dired-emacs \"~/.emacs.d\"))

     ⇒ (defun sboo-dired-emacs ()
     ⇒   \"Launch `dired' at directory « ~/emacs.d ».\"
     ⇒   (interactive)
     ⇒   (dired \"~/emacs.d\"))

Related:

• `dired'"

    (declare (indent 1) (doc-string 3))

    (let* ((NAME     name)
           (DIRECTORY directory)

           (DOCSTRING (or doc
                          (format "Launch `dired' at directory « %s »." DIRECTORY)))
           )

      `(defun ,NAME ()
         ,DOCSTRING
         (interactive)
         (dired ,DIRECTORY)))))

;; ^ e.g. `defun-dired':
;;
;; M-: (pp-macroexpand-expression '(defun-dired sboo-dired-emacs "~/emacs.d"))
;;
;;   ⇒ (defun sboo-dired-emacs ()
;;   ⇒   "Launch `dired' at directory « ~/emacs.d »."
;;   ⇒   (interactive)
;;   ⇒   (dired "~/emacs.d"))
;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Keymaps -------------------------------------;;
;;----------------------------------------------;;

;; Provide ‘sboo-*-keymap’s...

;;==============================================;;

(progn

  (defvar sboo-launch-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for launchning applications/programs.

its “Prefix Command” is bound to « \\[sboo-launch-keymap] ».

\\{sboo-launch-keymap}")   
                           
  (define-prefix-command 'sboo-launch-keymap nil "Σ Run")

;;(define-key sboo-launch-keymap (kbd "a") #')
;;(define-key sboo-launch-keymap (kbd "b") #')
;;(define-key sboo-launch-keymap (kbd "c") #')
 (define-key sboo-launch-keymap (kbd "d") #'sboo-paths-keymap)
;;(define-key sboo-launch-keymap (kbd "e") #')
;;(define-key sboo-launch-keymap (kbd "f") #')
;;(define-key sboo-launch-keymap (kbd "g") #')
;;(define-key sboo-launch-keymap (kbd "h") #')
;;(define-key sboo-launch-keymap (kbd "i") #')
;;(define-key sboo-launch-keymap (kbd "j") #')
;;(define-key sboo-launch-keymap (kbd "k") #')
;;(define-key sboo-launch-keymap (kbd "l") #')
;;(define-key sboo-launch-keymap (kbd "m") #')
;;(define-key sboo-launch-keymap (kbd "n") #')
;;(define-key sboo-launch-keymap (kbd "o") #')
  (define-key sboo-launch-keymap (kbd "p") #'helm-top) ; to[P].
;;(define-key sboo-launch-keymap (kbd "q") #')
;;(define-key sboo-launch-keymap (kbd "r") #')
  (define-key sboo-launch-keymap (kbd "s") #'shell)          ; [S]hell.
  (define-key sboo-launch-keymap (kbd "t") #'sboo-ansi-term) ; [T]erminal.
;;(define-key sboo-launch-keymap (kbd "u") #')
;;(define-key sboo-launch-keymap (kbd "v") #')
  (define-key sboo-launch-keymap (kbd "w") #'eww)
;;(define-key sboo-launch-keymap (kbd "x") #')
;;(define-key sboo-launch-keymap (kbd "y") #')
;;(define-key sboo-launch-keymap (kbd "z") #')

  #'sboo-launch-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-edit-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for editing/manipulating text.

its “Prefix Command” is bound to « \\[sboo-edit-keymap] ».

\\{sboo-edit-keymap}")

  ;; [E]diting Functions"
  (define-prefix-command 'sboo-edit-keymap nil "Σ Edit")

;;(define-key sboo-edit-keymap (kbd "a") #')
;;(define-key sboo-edit-keymap (kbd "b") #')
;;(define-key sboo-edit-keymap (kbd "c") #')
;;(define-key sboo-edit-keymap (kbd "d") #')
  (define-key sboo-edit-keymap (kbd "e") #'sboo-edit-indirect-dwim)
  (define-key sboo-edit-keymap (kbd "f") #'fill-dwim)
;;(define-key sboo-edit-keymap (kbd "g") #')
;;(define-key sboo-edit-keymap (kbd "h") #')
  (define-key sboo-edit-keymap (kbd "i") #'indent-dwim)
;;(define-key sboo-edit-keymap (kbd "j") #')
;;(define-key sboo-edit-keymap (kbd "k") #')
;;(define-key sboo-edit-keymap (kbd "l") #')
;;(define-key sboo-edit-keymap (kbd "m") #')
;;(define-key sboo-edit-keymap (kbd "n") #')
;;(define-key sboo-edit-keymap (kbd "o") #')
;;(define-key sboo-edit-keymap (kbd "p") #')
;;(define-key sboo-edit-keymap (kbd "q") #')
;;(define-key sboo-edit-keymap (kbd "r") #')
;;(define-key sboo-edit-keymap (kbd "s") #')
;;(define-key sboo-edit-keymap (kbd "t") #')
;;(define-key sboo-edit-keymap (kbd "u") #')
;;(define-key sboo-edit-keymap (kbd "v") #')
;;(define-key sboo-edit-keymap (kbd "w") #')
;;(define-key sboo-edit-keymap (kbd "x") #')
;;(define-key sboo-edit-keymap (kbd "y") #')
;;(define-key sboo-edit-keymap (kbd "z") #')

  #'sboo-edit-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-navigate-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for navigation.

its “Prefix Command” is bound to « \\[sboo-navigate-keymap] ».

\\{sboo-navigate-keymap}")

  (define-prefix-command 'sboo-navigate-keymap nil "Σ Nav")

;;(define-key sboo-navigate-keymap (kbd "a") #')
;;(define-key sboo-navigate-keymap (kbd "b") #')
;;(define-key sboo-navigate-keymap (kbd "c") #')
;;(define-key sboo-navigate-keymap (kbd "d") #')
;;(define-key sboo-navigate-keymap (kbd "e") #')
;;(define-key sboo-navigate-keymap (kbd "f") #')
;;(define-key sboo-navigate-keymap (kbd "g") #')
;;(define-key sboo-navigate-keymap (kbd "h") #')
;;(define-key sboo-navigate-keymap (kbd "i") #')
;;(define-key sboo-navigate-keymap (kbd "j") #')
;;(define-key sboo-navigate-keymap (kbd "k") #')
;;(define-key sboo-navigate-keymap (kbd "l") #')
;;(define-key sboo-navigate-keymap (kbd "m") #')
;;(define-key sboo-navigate-keymap (kbd "n") #')
;;(define-key sboo-navigate-keymap (kbd "o") #')
  ;; (define-key sboo-navigate-keymap (kbd "p <up>")   #'sboo-page-backward) ; [P]age-Breaks. (c.f.`page-delimiter').
  ;; (define-key sboo-navigate-keymap (kbd "p <down>") #'sboo-page-forward)  ; [P]age-Breaks. (c.f.`page-delimiter').
;;(define-key sboo-navigate-keymap (kbd "q") #')
;;(define-key sboo-navigate-keymap (kbd "r") #')
;;(define-key sboo-navigate-keymap (kbd "s") #')
;;(define-key sboo-navigate-keymap (kbd "t") #')
;;(define-key sboo-navigate-keymap (kbd "u") #')
;;(define-key sboo-navigate-keymap (kbd "v") #')
;;(define-key sboo-navigate-keymap (kbd "w") #')
;;(define-key sboo-navigate-keymap (kbd "x") #')
;;(define-key sboo-navigate-keymap (kbd "y") #')
;;(define-key sboo-navigate-keymap (kbd "z") #')

  #'sboo-navigate-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-search-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for editor/browser search.

its “Prefix Command” is bound to « \\[sboo-search-keymap] ».

\\{sboo-search-keymap}")

  (define-prefix-command 'sboo-search-keymap nil "Σ Search")

;;(define-key sboo-search-keymap (kbd "a") #')
;;(define-key sboo-search-keymap (kbd "b") #')
;;(define-key sboo-search-keymap (kbd "c") #')
;;(define-key sboo-search-keymap (kbd "d") #')
;;(define-key sboo-search-keymap (kbd "e") #')
;;(define-key sboo-search-keymap (kbd "f") #')
;;(define-key sboo-search-keymap (kbd "g") #')
;;(define-key sboo-search-keymap (kbd "h") #')
;;(define-key sboo-search-keymap (kbd "i") #')
;;(define-key sboo-search-keymap (kbd "j") #')
;;(define-key sboo-search-keymap (kbd "k") #')
;;(define-key sboo-search-keymap (kbd "l") #')
;;(define-key sboo-search-keymap (kbd "m") #')
;;(define-key sboo-search-keymap (kbd "n") #')
;;(define-key sboo-search-keymap (kbd "o") #')
  (define-key sboo-search-keymap (kbd "p") #'projectile-grep)
;;(define-key sboo-search-keymap (kbd "q") #')
;;(define-key sboo-search-keymap (kbd "r") #')
;;(define-key sboo-search-keymap (kbd "s") #')
;;(define-key sboo-search-keymap (kbd "t") #')
;;(define-key sboo-search-keymap (kbd "u") #')
;;(define-key sboo-search-keymap (kbd "v") #')
;;(define-key sboo-search-keymap (kbd "w") #')
;;(define-key sboo-search-keymap (kbd "x") #')
;;(define-key sboo-search-keymap (kbd "y") #')
;;(define-key sboo-search-keymap (kbd "z") #')

  #'sboo-search-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-mark-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for marking/selecting/highlighting text.

its “Prefix Command” is bound to « \\[sboo-mark-keymap] ».

\\{sboo-mark-keymap}")


  (define-prefix-command 'sboo-mark-keymap nil "Σ Mark")

  (define-key sboo-mark-keymap (kbd "a") #'mark-beginning-of-buffer)
  (define-key sboo-mark-keymap (kbd "b") #'mark-whole-buffer)
;;(define-key sboo-mark-keymap (kbd "c") #'mark-)
  (define-key sboo-mark-keymap (kbd "d") #'mark-defun)
  (define-key sboo-mark-keymap (kbd "e") #'mark-end-of-buffer)
;;(define-key sboo-mark-keymap (kbd "f") #'mark-)
;;(define-key sboo-mark-keymap (kbd "g") #'mark-)
;;(define-key sboo-mark-keymap (kbd "h") #'mark-)
;;(define-key sboo-mark-keymap (kbd "i") #'mark-)
;;(define-key sboo-mark-keymap (kbd "j") #'mark-)
;;(define-key sboo-mark-keymap (kbd "k") #'mark-)
;;(define-key sboo-mark-keymap (kbd "l") #'mark-)
;;(define-key sboo-mark-keymap (kbd "m") #'mark-)
;;(define-key sboo-mark-keymap (kbd "n") #'mark-)
;;(define-key sboo-mark-keymap (kbd "o") #'mark-)
  (define-key sboo-mark-keymap (kbd "p") #'mark-paragraph)
;;(define-key sboo-mark-keymap (kbd "q") #'mark-)
;;(define-key sboo-mark-keymap (kbd "r") #'mark-)
;;(define-key sboo-mark-keymap (kbd "s") #'mark-)
;;(define-key sboo-mark-keymap (kbd "t") #'mark-)
;;(define-key sboo-mark-keymap (kbd "u") #'mark-)
;;(define-key sboo-mark-keymap (kbd "v") #'mark-)
  (define-key sboo-mark-keymap (kbd "w") #'mark-word)
  (define-key sboo-mark-keymap (kbd "x") #'mark-sexp)
;;(define-key sboo-mark-keymap (kbd "y") #'mark-)
;;(define-key sboo-mark-keymap (kbd "z") #'mark-)

  #'sboo-mark-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-insert-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for inserting snippets/characters.

its “Prefix Command” is bound to « \\[sboo-insert-keymap] ».

\\{sboo-insert-keymap}")

  (define-prefix-command 'sboo-insert-keymap nil "Σ Insert")

;;(define-key sboo-insert-keymap (kbd "a") #')
;;(define-key sboo-insert-keymap (kbd "b") #')

  (define-key sboo-insert-keymap (kbd "c h") #'sboo-comment-insert-header)
  (define-key sboo-insert-keymap (kbd "c 1") #'sboo-comment-insert-h1)
  (define-key sboo-insert-keymap (kbd "c 2") #'sboo-comment-insert-h2)
  (define-key sboo-insert-keymap (kbd "c 3") #'sboo-comment-insert-h3)
  (define-key sboo-insert-keymap (kbd "c !") #'sboo-comment-insert-h1)
  (define-key sboo-insert-keymap (kbd "c @") #'sboo-comment-insert-h2)
  (define-key sboo-insert-keymap (kbd "c #") #'sboo-comment-insert-h3)

;;(define-key sboo-insert-keymap (kbd "d") #')
;;(define-key sboo-insert-keymap (kbd "e") #')
;;(define-key sboo-insert-keymap (kbd "f") #')
;;(define-key sboo-insert-keymap (kbd "g") #')
;;(define-key sboo-insert-keymap (kbd "h") #')
;;(define-key sboo-insert-keymap (kbd "i") #')
;;(define-key sboo-insert-keymap (kbd "j") #')
;;(define-key sboo-insert-keymap (kbd "k") #')
;;(define-key sboo-insert-keymap (kbd "l") #')
;;(define-key sboo-insert-keymap (kbd "m") #')
;;(define-key sboo-insert-keymap (kbd "n") #')
;;(define-key sboo-insert-keymap (kbd "o") #')
;;(define-key sboo-insert-keymap (kbd "p") #')
;;(define-key sboo-insert-keymap (kbd "q") #')
;;(define-key sboo-insert-keymap (kbd "r") #')
;;(define-key sboo-insert-keymap (kbd "s") #')
;;(define-key sboo-insert-keymap (kbd "t") #')
;;(define-key sboo-insert-keymap (kbd "u") #')
;;(define-key sboo-insert-keymap (kbd "v") #')
;;(define-key sboo-insert-keymap (kbd "w") #')
;;(define-key sboo-insert-keymap (kbd "x") #')
  (define-key sboo-insert-keymap (kbd "y") #'sboo-yas-insert-snippet)
;;(define-key sboo-insert-keymap (kbd "z") #')

  #'sboo-insert-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-buffer-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for buffer & file operations.

its “Prefix Command” is bound to « \\[sboo-buffer-keymap] ».

\\{sboo-buffer-keymap}")

  ;; [B]uffer (& File) Functions"
  (define-prefix-command 'sboo-buffer-keymap nil "Σ Buffer")

;;(define-key sboo-buffer-keymap (kbd "a") #')
;;(define-key sboo-buffer-keymap (kbd "b") #')
;;(define-key sboo-buffer-keymap (kbd "c") #')
;;(define-key sboo-buffer-keymap (kbd "d") #')
;;(define-key sboo-buffer-keymap (kbd "e") #')
;;(define-key sboo-buffer-keymap (kbd "f") #')
;;(define-key sboo-buffer-keymap (kbd "g") #')
;;(define-key sboo-buffer-keymap (kbd "h") #')
;;(define-key sboo-buffer-keymap (kbd "i") #')
;;(define-key sboo-buffer-keymap (kbd "j") #')
;;(define-key sboo-buffer-keymap (kbd "k") #')
;;(define-key sboo-buffer-keymap (kbd "l") #')
;;(define-key sboo-buffer-keymap (kbd "m") #')
;;(define-key sboo-buffer-keymap (kbd "n") #')
;;(define-key sboo-buffer-keymap (kbd "o") #')
;;(define-key sboo-buffer-keymap (kbd "p") #')
;;(define-key sboo-buffer-keymap (kbd "q") #')
  (define-key sboo-buffer-keymap (kbd "r") #'rename-file)
;;(define-key sboo-buffer-keymap (kbd "s") #')
;;(define-key sboo-buffer-keymap (kbd "t") #')
;;(define-key sboo-buffer-keymap (kbd "u") #')
;;(define-key sboo-buffer-keymap (kbd "v") #')
;;(define-key sboo-buffer-keymap (kbd "w") #')
;;(define-key sboo-buffer-keymap (kbd "x") #')
;;(define-key sboo-buffer-keymap (kbd "y") #')
;;(define-key sboo-buffer-keymap (kbd "z") #')

  #'sboo-buffer-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-paths-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for frequently-visited filepaths.

its “Prefix Command” is bound to « \\[sboo-paths-keymap] ».

\\{sboo-paths-keymap}")

  (define-prefix-command 'sboo-paths-keymap nil "Σ Paths")

  ;; ^ aliases and /or keyboard shortcuts for frequently-visited files and/or directories .

  (defun-dired sboo-dired-emacs         "~/.emacs.d")
  (defun-dired sboo-dired-haskell       "~/haskell")
  (defun-dired sboo-dired-configuration "~/configuration")

;;(define-key sboo-paths-keymap (kbd "a") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "b") #'sboo-dired-)
  (define-key sboo-paths-keymap (kbd "c") #'sboo-dired-configuration)
;;(define-key sboo-paths-keymap (kbd "d") #'sboo-dired-)
  (define-key sboo-paths-keymap (kbd "e") #'sboo-dired-emacs)
;;(define-key sboo-paths-keymap (kbd "f") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "g") #'sboo-dired-)
  (define-key sboo-paths-keymap (kbd "h") #'sboo-dired-haskell)
;;(define-key sboo-paths-keymap (kbd "i") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "j") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "k") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "l") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "m") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "n") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "o") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "p") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "q") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "r") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "s") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "t") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "u") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "v") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "w") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "x") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "y") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "z") #'sboo-dired-)

  #'sboo-paths-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-mode-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for mode-specific commands.

its “Prefix Command” is bound to « \\[sboo-mode-keymap] ».

\\{sboo-mode-keymap}")

  (define-prefix-command 'sboo-mode-keymap nil "Σ Mode-Specific")

;;(define-key sboo-mode-keymap (kbd "a") #')
;;(define-key sboo-mode-keymap (kbd "b") #')
;;(define-key sboo-mode-keymap (kbd "c") #')
;;(define-key sboo-mode-keymap (kbd "d") #')
;;(define-key sboo-mode-keymap (kbd "e") #')
;;(define-key sboo-mode-keymap (kbd "f") #')
;;(define-key sboo-mode-keymap (kbd "g") #')
;;(define-key sboo-mode-keymap (kbd "h") #')
;;(define-key sboo-mode-keymap (kbd "i") #')
;;(define-key sboo-mode-keymap (kbd "j") #')
;;(define-key sboo-mode-keymap (kbd "k") #')
;;(define-key sboo-mode-keymap (kbd "l") #')
;;(define-key sboo-mode-keymap (kbd "m") #')
  (define-key sboo-mode-keymap (kbd "n") #'sboo-prog-new)
;;(define-key sboo-mode-keymap (kbd "o") #')
;;(define-key sboo-mode-keymap (kbd "p") #')
;;(define-key sboo-mode-keymap (kbd "q") #')
;;(define-key sboo-mode-keymap (kbd "r") #')
;;(define-key sboo-mode-keymap (kbd "s") #')
;;(define-key sboo-mode-keymap (kbd "t") #')
;;(define-key sboo-mode-keymap (kbd "u") #')
;;(define-key sboo-mode-keymap (kbd "v") #')
;;(define-key sboo-mode-keymap (kbd "w") #')
;;(define-key sboo-mode-keymap (kbd "x") #')
;;(define-key sboo-mode-keymap (kbd "y") #')
;;(define-key sboo-mode-keymap (kbd "z") #')

  #'sboo-mode-keymap)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; ‘yasnippet.el’:
;;
;; • ‘sboo-*-keymap’s are written via « ~/.emacs.d/sboo/snippets/emacs-lisp-mode/defvar-keymap.yasnippet ».
;;

;; Links:
;;
;;   • URL `http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/'
;;   • URL `https://bendersteed.gitlab.io/post/rediscovering-vanilla-emacs-text-editing/'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-keymaps)

;;; sboo-keymaps.el ends here