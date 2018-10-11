;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rx)

(require 'dirtrack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `comint' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq comint-scroll-to-bottom-on-input t)

;; ^ non-`nil' = insertion and yank commands scroll the selected window to the bottom before inserting.

(setq comint-input-ignoredups t)

;; ^ whether successive identical inputs are stored in the input history.

(setq comint-completion-addsuffix t) 

;; ^ whether completion inserts a space or a slash after a fully-completed file or directory (respectively).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `shell' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-path-regexp

  (rx (any "/.~")
      (zero-or-more (not (any "$"))))

   "Regular expression (fragment) matching a (POSIX) filepath.")

(defvar sboo-prompt-regexp

  (rx bol
      (zero-or-more space)
      (group (any "/.~")                   ;; the present working directory
             (zero-or-more (not (any "$"))))
      "$"
      (zero-or-more space))

      "Regular expression (for a whole line) to extract the working directory from a command prompt.

MUST be able to match the user's `PS1'.

For `dirtrack-list'.

e.g. Prompt \"~/.emacs.d$ \" holds directory \"~/.emacs.d\".")

;; ^ e.g. matches this command prompt

(setq dirtrack-list (list sboo-prompt-regexp 0))

(add-hook 'shell-mode-hook #'dirtrack-mode)

;; ^ `dirtrack' tracks the working directory from the command prompt

(setq shell-completion-fignore '("~" "#" "%"))

;; ^ filename extensions to ignore during completion.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `comint-mode'
;;
;; `comint' abbreviates "COMmand INTerpreter".
;;
;; 
;;

;;; `dirtrack-mode'
;;
;; 
;;

;;; `rx'
;;
;; 
;;

;;; Links
;;
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell-Options.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Tracking.html
;; - https://emacs.stackexchange.com/questions/5589/automatically-update-default-directory-when-pwd-changes-in-shell-mode-and-term-m
;; - https://snarfed.org/why_i_run_shells_inside_emacs
;; - https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/ 
;; - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-shell) 