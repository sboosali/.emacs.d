;;; -*- lexical-binding: t -*-

;;; Commentary:

;;----------------------------------------------;;
;; Configuration for the `bookmark' (builtin) package.
;;
;; Motivation: shorten frequently-typed commands
;; (c.f. keybindings, i.e. frequently-pressed commands).
;;
;;----------------------------------------------;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin packages:

(require 'cl-lib)
(require 'pcase)

(require 'bookmark)

;; sboo packages:

(require 'sboo-xdg nil :noerror)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;
;;; `:init'

(defun sboo-bookmark-init! ()

  "Initialize `bookmark' variables."

  (interactive)

  (if (require 'sboo-xdg nil :noerror)

      (setq bookmark-default-file (sboo-xdg-data "bookmarks.el" :subdir "emacs/bookmark"))

    (setq bookmark-default-file "bookmarks.el"))

  ())

;;----------------------------------------------;;
;;; `:config'
;;----------------------------------------------;;

(defun sboo-bookmark-config! ()

  "Configure `bookmark' resources."

  (interactive)

  (make-directory (file-name-directory bookmark-default-file)
                  :create-parent-directories)

  ())

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
(provide 'sboo-bookmark)