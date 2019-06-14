;;; sboo-init-settings.el --- Personal `custom-set-variables' -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

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

;; Personal settings.
;;
;; Settings should be both **fast** and **safe**,
;; Like « (use-package :init ...) », they are eager.
;;
;; Most settings are literally set-statements.
;; (`sboo-custom-set', `custom-set-variables', `setq', etc)
;;
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'cl-lib))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  ;;--------------------------;;

  (when (not (fboundp #'sboo-custom-set))

    (defmacro sboo-custom-set (variable expression &optional comment)

      "`custom-set-variables' wrapper."
      
      (declare (indent 2) (doc-string 3))

      `(with-demoted-errors "[Warning] %s"
         (custom-set-variables
          (list (quote ,variable) (quote ,expression) :eager nil ,comment))))) ;TODO handle variables, not just symbols

  ;;--------------------------;;

  ())

;;----------------------------------------------;;

(defun sboo-display-message-or-buffer (string)
  "`display-message-or-buffer' wrapper."

  (when (stringp string)
    (display-message-or-buffer string)))

;; Returns either the string shown in the echo area, or when a pop-up
;; buffer is used, the window used to display it.

;; If MESSAGE is a string, then the optional argument BUFFER-NAME is the
;; name of the buffer used to display it in the case where a pop-up buffer
;; is used, defaulting to ‘*Message*’.  In the case where MESSAGE is a
;; string and it is displayed in the echo area, it is not specified whether
;; the contents are inserted into the buffer anyway.

;; Optional arguments ACTION and FRAME are as for ‘display-buffer’,
;; and are only used if a pop-up buffer is displayed.

;;----------------------------------------------;;

(defun sboo-proced-settings ()

  "My `proced' settings (enable Auto-Refresh)."

  (progn
    (setq proced-auto-update-interval 3)  ; in Seconds.
    (proced-toggle-auto-update +1)
    ()))

;;----------------------------------------------;;

(defun sboo-dired-settings ()

  "My `dired' settings (enable Auto-Refresh)."

  (progn
    (setq dired-auto-revert-buffer t)
    ()))

;;----------------------------------------------;;
;;; Settings: Unicode  -------------------------;;
;;----------------------------------------------;;

(progn

  (prefer-coding-system       'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (setq buffer-file-coding-system 'utf-8)

  ;; ^ UTF-8 as the default Unicode Encoding.

  (set-language-environment "UTF-8"))

;;----------------------------------------------;;
;; Settings: Collective ------------------------;;
;;----------------------------------------------;;

(cua-mode +1)

;; ^ the standard keybindings: C-c, C-x, C-v, C-z.

(transient-mark-mode +1)

;; ^ No region when nothing is highlighted.

(delete-selection-mode +1)

;; ^ Overwrite region when typing/pasting (manually),
;;   but not `insert'ing (programmatically).

(when (>= emacs-major-version 26)
  (global-linum-mode +1))

;; ^ show Line-Number Sidebar (`linum-mode' is fast, written in C)/

(global-font-lock-mode +1)

;; ^ Syntax Highlighting, by default.

(auto-compression-mode +1)

;; ^ automatically open Compressed Files.

(show-paren-mode +1)

;; ^ automatically highlight Matching Parentheses.

(ffap-bindings)

;; ^ « ffap » abbreviates `find-file-at-point'.

(fset 'yes-or-no-p #'y-or-n-p)

;; ^ concise boolean prompts.
;;
;; ^ with `y-or-n-p', you press just one key for boolean prompts.
;;   (i.e. the single character "y", instead of typing out the phrase "yes".)
;;
;; e.g. in `dired-mode':
;; when deleting a file, you are prompted by `y-or-n-p';
;; but when deleting a directory, you are prompted by (the more verbose) `yes-or-no-p'.
;;

;; ^ (Why `fset'??)

;;----------------------------------------------;;
;; Settings: Personal Information --------------;;
;;----------------------------------------------;;

(sboo-custom-set user-full-name    "Spiros Boosalis"       "")
(sboo-custom-set user-mail-address "SamBoosalis@gmail.com" "")

;;----------------------------------------------;;
;; Settings: Modeline --------------------------;;
;;----------------------------------------------;;

(sboo-custom-set column-number-mode t
  "show Column Numbers in the Modeline (by default, only Row Numbers are shown).")

(sboo-custom-set size-indication-mode t
  "show Buffer Size in the Modeline.")

;;----------------------------------------------;;
;; Settings: Faces -----------------------------;;
;;----------------------------------------------;;

;; (custom-set-faces
;;   `(minibuffer-prompt ((t (:family "Iosevka")))))

;;----------------------------------------------;;
;; Settings: Themes ----------------------------;;
;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s"

  (let ((THEME-DIRECTORY
         (if (bound-and-true-p sboo-theme-directory)
             sboo-theme-directory
           (expand-file-name "~/.emacs.d/sboo/themes")))
        )

    (custom-set-variables `(custom-theme-directory ,THEME-DIRECTORY :eager nil "Personal themes."))))

;;----------------------------------------------;;

;(set-background-color "#f4f4f4")

 ;; ^
 ;; i.e. R=xF4 G=xF4 B=xF4
 ;; i.e. rgb(244, 244, 244)
 ;; i.e. faint gray (near-white)
 ;;
 ;; see:
 ;;     https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme

;;(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
;;(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;;----------------------------------------------;;
;; Settings: Toolbar ---------------------------;;
;;----------------------------------------------;;

(sboo-custom-set tool-bar-position 'left
  "the Tool Bar is on the left.")

(sboo-custom-set auto-resize-tool-bars t
  "do grow the Tool Bar when enough Menu Items are added.")

(sboo-custom-set auto-raise-tool-bar-buttons t
  "visually raise a Tool Bar Item when the Mouse hovers over it.")

;;----------------------------------------------;;
;; Settings: Undo ------------------------------;;
;;----------------------------------------------;;

(sboo-custom-set undo-limit 20000000
  "maximize Undo History.")

(sboo-custom-set undo-strong-limit 40000000
  "maximize Undo History.")

;;----------------------------------------------;;
;; Settings: Keyboard --------------------------;;
;;----------------------------------------------;;

;; (sboo-custom-set  "")

;;----------------------------------------------;;
;; Settings: Mouse -----------------------------;;
;;----------------------------------------------;;

(sboo-custom-set mouse-wheel-mode t
  "enable the Mouse-Wheel.") 

(sboo-custom-set mouse-yank-at-point t
  "don't move `point' when pasting with the mouse.")

(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)) t nil "`1' means: Mouse-Wheel scrolls one line per flick (by default, it's 5)."))

(sboo-custom-set mouse-wheel-follow-mouse t
  "scroll Window under Mouse (by default, scroll `selected-window' always).")

;;----------------------------------------------;;
;; Settings: Clipboard -------------------------;;
;;----------------------------------------------;;

(sboo-custom-set select-enable-clipboard t
  "Non-nil means: cutting and pasting uses the clipboard.")

(sboo-custom-set cua-keep-region-after-copy t
  "standard Windows behavior.")

(sboo-custom-set kill-whole-line t
  "« C-k » eats newlines (a.k.a. it kills the whole line). By enabling `kill-whole-line', we can type just « C-k » where before we typed « C-k C-k ».")

;; (setq select-enable-primary   t)

;; (setq mouse-yank-at-point t)

;; (setq interprogram-cut-function #'gui-select-text)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; ^ URL `http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html'

;;----------------------------------------------;;
;; Settings: Viewing ---------------------------;;
;;----------------------------------------------;;

(sboo-custom-set view-read-only t
  "`t' means: always launch read-only buffers/files with `view-mode'.")

;;----------------------------------------------;;
;; Settings: Highlighting ----------------------;;
;;----------------------------------------------;;

(sboo-custom-set set-mark-command-repeat-pop t
  "If you set set-mark-command-repeat-pop to non- nil , then immediately after you type C-u C-<SPC> , you can type C-<SPC> instead of C-u C-<SPC> to cycle through the mark ring.")

;;----------------------------------------------;;
;; Settings: Scrolling -------------------------;;
;;----------------------------------------------;;

(sboo-custom-set scroll-step 1
  "")

(sboo-custom-set next-screen-context-lines 0
  "")

(progn

  ;; Scroll Smoothly:

  (sboo-custom-set scroll-margin 0
    "Scroll smoothly.")

  (sboo-custom-set scroll-conservatively 10000
    "Scroll smoothly.")

  (sboo-custom-set scroll-preserve-screen-position 'always
    "`t' means that Emacs adjusts point to keep the cursor at the same screen position whenever a scroll command moves it off-window, rather than moving it to the topmost or bottommost line.")

  ())

(sboo-custom-set redisplay-dont-pause t
  "“Peeking” behavior when scrolling.")

;;TODO
;; URL `https://superuser.com/questions/527356/dont-change-the-cursor-position-when-scrolling-in-emacs'

;;TODO
;;(sboo-custom-set auto-window-vscroll nil "")

;;----------------------------------------------;;

(sboo-custom-set visible-bell t
  "flash a Black Square onto the screen on User Errors (instead of honking loudly through the speakers).")

(sboo-custom-set ring-bell-function 'ignore
  "disable Blinking and Flashing.")

;;----------------------------------------------;;

(sboo-custom-set inhibit-splash-screen     t "fewer Startup Buffers.")
(sboo-custom-set initial-scratch-message nil "fewer Startup Buffers.")

;;----------------------------------------------;;
;; Settings: Backups ---------------------------;;
;;----------------------------------------------;;

;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; (setq delete-old-versions -1)
;; (setq version-control t)
;; (setq vc-make-backup-files t)
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; “By default, Emacs saves backup files in the current directory. These are the files ending in ~ that are cluttering up your directory lists. The following code stashes them all in ~/.emacs.d/backups, where I can find them with C-x C-f (find-file) if I really need to.”
;; “Disk space is cheap. Save lots.”

;;----------------------------------------------;;
;; Settings: Whitespace ------------------------;;
;;----------------------------------------------;;

(sboo-custom-set truncate-lines nil
  "enable Continuation Lines.")

(sboo-custom-set require-final-newline nil
  "no Automatic Insertion of a Final Newline.")

(sboo-custom-set mode-require-final-newline nil
  "no Automatic Insertion of a Final Newline.")

(sboo-custom-set indent-tabs-mode nil
  "no Extraneous Tabs.")

(sboo-custom-set show-paren-delay 0
  "no Delay.")

(sboo-custom-set show-paren-style 'mixed
  "highlight the Parenthesized Expression, unless the Matching Parenthesis is visible (not just the Parenthesis).")

;;----------------------------------------------;;
;; Settings: Buffers ---------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Settings: Files -----------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Settings: Settings --------------------------;;
;;----------------------------------------------;;

;; (sboo-custom-set t
;;   ".")

;; (sboo-custom-set t
;;   ".")

;;----------------------------------------------;;

;; (sboo-custom-set initial-major-mode 'text-mode
;;   "the default Major Mode for a new Buffer.")

;(setq initial-buffer-choice 'xah-new-empty-buffer)
;; ^ Start Emacs with Empty Buffer

;;----------------------------------------------;;

(sboo-custom-set linum-format 'dynamic
  "")

;; (sboo-custom-set linum-format "%3d"
;;   "Three-Digit Line-Numbers (e.g. « 001 », not « 1 »).")

;;----------------------------------------------;;

;; (setq echo-keystrokes 0.1)

;; (setq switch-to-buffer-preserve-window-point t)

;;----------------------------------------------;;

(sboo-custom-set use-dialog-box nil
  "`nil' replaces Dialog Boxes with `yes-or-no' prompts.")

;; ^
;;
;; i.e. minibuffer prompts, which use the echo area and keyboard input.
;;
;; `use-dialog-box' also determines whether to use native file selection windows.
;;

;;----------------------------------------------;;

(sboo-custom-set enable-recursive-minibuffers   t
  "so you can: ① press « M-x » within a « M-x »; ② search through (via a second « C-s ») the minibuffer of a search command for the (non-mini) buffer (having pressed the first « C-s »).")

(sboo-custom-set minibuffer-depth-indicate-mode t
  "e.g. minibuffer displays « M-x [2] » when you've (often accidentally) double-« M-x »'d.")

;;----------------------------------------------;;

(sboo-custom-set mouse-1-click-follows-link t
  "Left-Click a HyperLink to open it.")

;;----------------------------------------------;;

(setq Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)

;;----------------------------------------------;;

;:FIXME;; (setq show-help-function #'sboo-display-message-or-buffer)

;; Default `show-help-function' is `tooltip-show-help'.

;;----------------------------------------------;;
;; Minibuffer:

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; ^ `eldoc-mode', in `minibuffer-*-mode', displays signatures in the Mode-Line.

;;----------------------------------------------;;
;; Echo Area:


;;----------------------------------------------;;
;; Text

(sboo-custom-set sentence-end-double-space nil
  "sentences end with a Single Period.")

;;----------------------------------------------;;
;; Help

(sboo-custom-set help-at-pt-display-when-idle t
  "automatically show Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point) on point-over (i.e. when `point' moves there, not when the `cursor' hovers over).")

(progn
  (sboo-custom-set help-at-pt-timer-delay 3
    "Wait this many seconds before showing Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point)")
  (help-at-pt-set-timer))

;; ^ `help-at-pt-display-when-idle':
;;
;; Automatically show local help on point-over.
;;
;; If the value is t, the string obtained from any ‘kbd-help’ or
;; ‘help-echo’ property at point is automatically printed in the
;; echo area, if nothing else is already displayed there, or after a
;; quit.  If both ‘kbd-help’ and ‘help-echo’ produce help strings,
;; ‘kbd-help’ is used.
;;

(sboo-custom-set echo-keystrokes 1
  "Wait this many seconds, then echo the currently-pressed key sub-sequence.")

(sboo-custom-set message-truncate-lines t
  "don't resize Echo Area for long messages (instead, truncate the message).")

;;----------------------------------------------;;

(sboo-custom-set x-underline-at-descent-line t
  "put the Underline below the Font Bottomline (instead of the Font Baseline).")

;;----------------------------------------------;;

(sboo-custom-set history-length 10000
  "increase Minibuffer History.")

;;----------------------------------------------;;
;; DirEd:

(sboo-custom-set dired-auto-revert-buffer t
  "Auto-Refresh.")

(add-hook 'dired-mode-hook #'sboo-dired-settings)

;;----------------------------------------------;;
;; ProcEd:

(add-hook 'proced-mode-hook #'sboo-proced-settings)

;;----------------------------------------------;;
;; Lisp:

(sboo-custom-set lisp-indent-function 'common-lisp-indent-function
  "format Property Lists correctly.")

;;----------------------------------------------;;
;; Buffer Display:

(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window)
             :do-append)

;; (add-to-list 'display-buffer-alist
;;              '(("\\*Completions\\*" display-buffer-pop-up-window)
;;                ))

;; (custom-set-variables ... t nil ".")

;;----------------------------------------------;;
;; Compilation:

;; (sboo-custom-set compilation-auto-jump-to-first-error t
;;   "Start at the first error (i.e. link).")

;;----------------------------------------------;;
;; Grep:

(sboo-custom-set grep-scroll-output t
  "Jump to result (`point' at end of output window).")

;; jump to the first result

(sboo-custom-set grep-save-buffers t
  "Don't ask (just save all buffers).")

(sboo-custom-set grep-highlight-matches 'always
  "grep ‘--color=’ (`auto' or `always')")

(sboo-custom-set grep-first-column 1
  "One-based columns (≡ 1) or Zero-based columns (≡ 0).")

(sboo-custom-set grep-find-template "find -L <D> <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} +"

  "`find-grep' for `nix'. « -L » traverses symlinks (emacs packages installed via « nix » are symlinks).")

;; Default: « "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +" »
;; Example: « find . -type f -exec grep --color -nH --null -e _ \{\} + »

;;----------------------------;;

;; See: URL `https://stackoverflow.com/questions/28915372/change-the-default-find-grep-command-in-emacs'

;; « C-h f `grep-find-template' »:
;;
;; Placeholders (mandatory):
;;
;;  <D> - base directory for find
;;  <X> - find options to restrict or expand the directory list
;;  <F> - find options to limit the files matched
;;  <C> - place to put the grep options like -i and --color
;;  <R> - the regular expression searched for.
;;
;; For custom placeholders, extend `grep-expand-keywords'.
;;

;; « C-h f `grep-apply-setting' »:
;;
;;
;;

;; M-: « (`grep-compute-defaults') »:
;;
;;     '((localhost (grep-command "grep --color -nH --null -e ")
;;                  (grep-template "grep <X> <C> -nH --null -e <R> <F>")
;;                  (grep-use-null-device nil)
;;                  (grep-find-command ("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49))
;;                  (grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
;;                  (grep-use-null-filename-separator t)
;;                  (grep-find-use-xargs exec-plus)
;;                  (grep-highlight-matches auto)
;;                  )
;;       (nil (grep-command nil)
;;            (grep-template nil)
;;            (grep-use-null-device auto-detect)
;;            (grep-find-command nil)
;;            (grep-find-template nil)
;;            (grep-use-null-filename-separator auto-detect)
;;            (grep-find-use-xargs nil)
;;            (grep-highlight-matches auto-detect))
;;       )
;;
;;

;; `grep-history':
;;
;; grep-history
;; grep-find-history
;; grep-files-history

;;----------------------------------------------;;
;; EShell:

(sboo-custom-set eshell-destroy-buffer-when-process-dies t
  "“To get rid of those lingering buffers.”")

;;TODO;; (add-hook 'eshell-mode-hook 'sboo-eshell-load-bashrc-aliases)

;;----------------------------------------------;;
;; `shell-script-mode':

;; Auto-executable scripts in « *nix ».
;; When saving a file that starts with « #! », make it executable.

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------;;
;; Settings: non-Customizeable -----------------;;
;;----------------------------------------------;;

(add-to-list 'safe-local-variable-values
             '(lexical-binding . t))

;; ^ `safe-local-variable-values':
;;

;;----------------------------------------------;;

;; Enable disabled commands:

(put 'upcase-region   'disabled nil)  ;; « C-x C-u »: same as M-u, but on whole regions.
(put 'downcase-region 'disabled nil)  ;; « C-x C-l »: same as M-l, but on whole regions.

;;----------------------------------------------;;

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;----------------------------------------------;;

(setenv "LD_PRELOAD" "") ;;HACK;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;; Minibuffers
;;
;; common Minibuffers include:
;;
;; - `minibuffer-inactive-mode': the search prompt.
;; -
;; -
;;
;;

;;; `setq-default'
;;
;; `setq-default' sets a values **only in** buffers that don't already have their own (i.e. `buffer-local') values for the variable.
;;
;; `setq-default' vs `setq':
;;

;;; `undo'
;;
;; the "internal" undo behavior is:
;;
;; - To redo, just Press Ctrl+g first then undo. further undo will be redo.
;; - Press Ctrl+g again to reverse direction. ("If you are careful, one can avoid the undo/redo roller-coaster confusion.")
;;
;; > all external undo-packages have corruption problems.

;;; e.g. Mode Discovery
;;
;; [1] open a mini-buffer (e.g. `C-s` for the search mini-buffer, a.k.a `minibuffer-inactive-mode');
;; [2] then, focused on the minibuffer, run `describe-mode` (i.e. `C-h m`).
;;

;;; `dired':
;;
;; <>
;;
;;

;;; `proced':
;;
;; <https://www.masteringemacs.org/article/displaying-interacting-processes-proced>
;;
;; `proced` abbreviates "PROCess EDitor".
;;
;;

;;; `simple':
;;
;;(defvar interprogram-cut-function #'gui-select-text
;;  "Function to call to make a killed region available to other programs.
;;Most window systems provide a facility for cutting and pasting
;;text between different programs, such as the clipboard on X and
;;MS-Windows, or the pasteboard on Nextstep/Mac OS.
;;
;;This variable holds a function that Emacs calls whenever text is
;;put in the kill ring, to make the new kill available to other
;;programs.  The function takes one argument, TEXT, which is a
;;string containing the text which should be made available.")
;;

;;; Links:
;;
;; - http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

;;; sboo-init-settings.el ends here