;;; ninit.el --- sboosali's “nano init.el” -*- coding: utf-8; lexical-binding: t -*-

;;; Code:

(require 'rx)

;; 

(auto-save-visited-mode +1)

(display-line-numbers-mode +1)

(pixel-scroll-precision-mode +1)

;;(modify-frame-parameters nil
;;  `(( . )
;;    ( . )
;;    ( . )))

;; ^ 
;; name, title;
;; height, width, menu-bar-lines;
;; 

(setopt visible-bell t)

(setopt echo-keystrokes 1)

(setopt display-line-numbers t)

;;

(use-package display-line-numbers
  :ensure nil

  :custom
  (display-line-numbers-grow-only   t)
  (display-line-numbers-width-start t)
  )

;;


(setq-default
  line-number-mode   t
  column-number-mode t
  size-indication-mode nil
  mode-line-position                nil
  mode-line-percent-position        nil
  mode-line-in-non-selected-windows nil
  )

;; ^ the Mode-Line.

(setq-default
  cursor-type                    'bar
  cursor-in-non-selected-windows nil
  )

;;

(use-package tool-bar

;;:hook (after-init . tool-bar-mode)

  :custom
  (tool-bar-position 'left)
  (tool-bar-style    'both)

  )

;; tool-bar-border
;; tool-bar-button-margin
;; tool-bar-button-relief
;; tool-bar-images-pixel-height
;; tool-bar-keymap-cache
;; tool-bar-map
;; tool-bar-max-label-size
;; tool-bar-mode-hook
;; tool-bar-mode
;; tool-bar-position
;; tool-bar-separator-image-expression
;; tool-bar-style

;; tool-bar--image-expression tool-bar-add-item-from-menu
;; tool-bar-add-item
;; tool-bar-get-system-style tool-bar-height
;; tool-bar-lines-needed
;; tool-bar-local-item-from-menu
;; tool-bar-local-item
;; tool-bar-make-keymap-1
;; tool-bar-make-keymap
;; tool-bar-mode
;; tool-bar-pixel-width
;; tool-bar-setup

;; ^ 
;;

(use-package scroll-bar

;;:hook (after-init . scroll-bar-mode)

  :custom
  (scroll-bar-width 20)

  )

;; ^ vertical (c.f. ‘horizontal-scroll-bar-mode’).
;;
;; >If Emacs is compiled on the X Window System without X toolkit support, the scroll bar behaves differently. Clicking mouse-1 anywhere on the scroll bar scrolls forward like C-v, while mouse-3 scrolls backward like M-v. Clicking mouse-2 in the scroll bar lets you drag the inner box up and down.
;;
;; ^ width in pixels of vertical scroll bar (buffer-local).

;;

(use-package tab-bar

  :hook (after-init . tab-bar-mode)

  :custom
  (tab-bar-mode t)
;;(tab-bar-position                 'below)
  (tab-bar-tab-hints              t)  ; uniquely-number tabs.
  (tab-bar-close-button-show      'selected)
  (tab-bar-tab-show               1)  ; unless ≤1 open tabs.
  (tab-bar-tab-name-ellipsis      "…")
  (tab-bar-tab-name-truncated-max 20)
  (tab-bar-new-tab-choice         'window)
  (tab-bar-new-tab-group           nil)

  :config
  ;;(add-to-list 'tab-bar-format #'tab-bar-format-tabs-groups nil)
  ())

;; ^ ‘tab-bar-mode’ 

;; 

(use-package tab-line

  :hook (after-init . global-tab-line-mode)

  :init

  (defcustom sboo-file-extension-unicode-emoji-alist
    '(("el"  . ?🦬)
      ("eld" . ?🦬)
      ("txt" . ?📝)
      ("md"  . ?📝)
      ("org" . ?📝)
      ("" . ? )
      ;;("" . ?)
      )
    "")

  (defcustom sboo-major-mode-unicode-emoji-alist
    '((emacs-lisp-mode . ?🦬)
      (text-mode     . ?📝)
      (markdown-mode . ?📝)
      (org-mode      . ?📝)
      ;;("" . ?)
      )
    "")

  (defun sboo-tab-line-get-tab-name (buffer &optional _buffers)
    (let* ((NAME (buffer-name buffer))
           (MODE (buffer-local-value 'major-mode buffer))
           (FEXT (file-name-extension NAME))
           (CHAR (or (alist-get FEXT sboo-file-extension-unicode-emoji-alist nil nil #'equal)
                     (alist-get MODE sboo-major-mode-unicode-emoji-alist nil nil #'eq)))
      (if CHAR
          (format "%c %s" CHAR NAME)
        (format "%s" NAME)))))

  (defun sboo-tab-line-group-buffers-by-project (buffer)
    "Group BUFFER under its ‘project-current’ name."
    (with-current-buffer buffer
      (string-remove-suffix "/"
        (car (project-roots (project-current))))))

  (defun sboo-tab-line-sort-buffers-by-name (a b)
    "Compare A and B by their ‘buffer-name’."
    (string-collate-lessp (buffer-name a)
                          (buffer-name b)
                          "en_US.UTF-8"
                          t))

  :custom
  ;;(global-tab-line-mode t)
  (tab-line-separator         "")
  (tab-line-new-button-show   t)
  (tab-line-close-button-show nil)

  (tab-line-tabs-function tab-line-tabs-buffer-groups)
  (tab-line-tabs-buffer-group-function sboo-tab-line-group-buffers-by-project)
  (tab-line-tabs-buffer-group-sort-function sboo-tab-line-sort-buffers-by-name)

  ;;(tab-line-tab-name-function #'sboo-tab-line-get-tab-name)

  :config
  ()

  ;; :custom-face

  ;; (tab-line nil
  ;;     :background "gray40"
  ;;     :foreground "gray60" :distant-foreground "gray50"
  ;;     :height 1.0 :box nil)
  ;; ;; ^ background behind tabs.

  ;; (tab-line-tab nil
  ;;     :inherit 'tab-line
  ;;     :foreground "gray70" :background "gray90" :box nil)
  ;; ;; ^ active tab in another window.

  ;; (tab-line-tab-current nil
  ;;   :background "#b34cb3" :foreground "white"
  ;;   :box nil)
  ;; ;; ^ active tab in current window.

  ;; (tab-line-tab-inactive nil
  ;;   :background "gray60" :foreground "black"
  ;;   :box nil)
  ;; ;; ^ inactive tab.

  ;; (tab-line-highlight nil
  ;;   :background "white" :foreground 'unspecified)
  ;; ;; ^ mouseover'ed tab.

  )

;; ^ ‘tab-line-mode’ 

;; 

(menu-bar-mode +1)

;; (menu-bar-add-menu-item
;;   '("" "" . ))

;; ^ 

(context-menu-mode t)

;; ^ rebind ‘mouse-3’ (“right-click”/“long-touch”) from ‘mouse-save-then-kill’ to ‘context-menu-?’ (bound only to ‘S-F10’ by default).
;;
;; mouse-1 (Left-button click)
;; mouse-2 (Middle-button click / Wheel click)
;; mouse-3 (Right-button click)
;; mouse-4 (Wheel scroll-up)
;; mouse-5 (Wheel scroll-down)


(use-package window

  :custom
  (switch-to-buffer-obey-display-actions t)

  :keybindings
  (
   ;; "C-x q" #'bury-buffer
   ;; "C-x Q" #'unbury-buffer
   ;; "C-x s" #'window-toggle-side-windows
   )

  :config
  ()
  )

(defconst sboo/display-buffer-alist
  (let ((WIN-PARAMS
          '(window-parameters . ((no-delete-other-windows . t)))
          ))

    `(( 'special-mode
        display-buffer-in-side-window
        (side . right)
        (slot . 0)
        (window-width . 0.1)
        ,WIN-PARAMS)

      ))
  "My `display-buffer-alist'.

Display:

* `special-mode' buffers as narrow (includes `help-mode').")

;; ^ (manually) ‘switch-to-buffer’ing must obey ‘buffer-display-action-alist’, like (programmatically) ‘display-buffer’ing obeys it.

(add-to-list 'display-buffer-alist
  '("\\*Help\\*"
     (display-buffer-reuse-window display-buffer-pop-up-window)))

(defconst sboo-prog-mode/display-buffer-alist
  (let ((WIN-PARAMS
          '(window-parameters . ((no-other-window         . t)
                                 (no-delete-other-windows . t)))))

    `((,(rx "*" (or "Buffer List") "*")
        display-buffer-in-side-window
        (side . top)
        (slot . 0)
        (window-height . ,#'fit-window-to-buffer)
        (preserve-size . (nil . t))  ; keep ‘window-height’ same / don't vertically-resize.
        ,WIN-PARAMS)

      (,(rx "*" (or "xref" "Tags List") "*")  ; = "\\*\\(?:xref\\|Tags List\\)\\*"
        display-buffer-in-side-window
        (side . right)
        (slot . 0)     ; in-the-middle-of.
        (window-width . ,#'fit-window-to-buffer)
        (preserve-size . (t . nil))  ; keep ‘window-width’ the same / don't horizontally-resize.
        ,WIN-PARAMS)

      (,(rx "*" (or "dired") "*")
        display-buffer-in-side-window
        (side . left)
        (slot . 0)
        (window-width . ,#'fit-window-to-buffer)
        (preserve-size . (t . nil))
        ,WIN-PARAMS)

      (,(rx "*" (or "Completions" "help" "Info" "grep") "*")
        display-buffer-in-side-window
        (side . bottom)
        (slot . -1)  ; above-or-left-of.
        (preserve-size . (nil . t))
        ,WIN-PARAMS)

      (,(rx "*" (or "shell" "eshell" "term" "vc" "compilation") "*")
        display-buffer-in-side-window
        (side . bottom)
        (slot . +1)  ; below-or-right-of.
        (preserve-size . (nil . t))
        ,WIN-PARAMS)

       (,(rx (or (bol "test" (char ?_ ?-))
                 ((or "Test" "Tests") eol)))
        display-buffer-in-direction
        (direction . right)))

    )
  "A ‘display-buffer-alist’ for “IDE Panels”:

• on the right, an XRef/TAGS buffer;
• on the left, a DirEd/¿Project? buffer;
• at the bottom-left, a Completions/Help/Info/Grep buffer;
• at the bottom-right, a Shell/Term/Compilation/VC buffer;
• at the top, the Buffer-List buffer.")

;;  ___________________________________
;; |    *Buffer List*                  |
;; |___________________________________|
;; |     |                       |     |
;; |  *  |                       |  *  |
;; |  d  |                       |  T  |
;; |  i  |                       |  a  |
;; |  r  |   Main Window Area    |  g  |
;; |  e  |                       |  s  |
;; |  d  |                       |  *  |
;; |  *  |                       |     |
;; |_____|_______________________|_____|
;; | *help*/*grep*/  |  *shell*/       |
;; | *Completions*   |  *compilation*  |
;; |_________________|_________________|
;; |             Echo Area             |
;; |___________________________________|
;; 

(defun sboo-prog-mode-ui (&optional disable-p)
  "Enable “IDE Panels”."
  (interactive "P")

  (if disable-p
      (prog
        (setq window-sides-slots (list nil nil nil nil))
        (setq display-buffer-alist nil))

    (setq fit-window-to-buffer-horizontally t)
    (setq window-resize-pixelwise           t)
    (let ((LEFT   1)
          (TOP    2)
          (RIGHT  1)
          (BOTTOM 2))
      (setq window-sides-slots (list LEFT TOP RIGHT BOTTOM))
    (setq display-buffer-alist sboo-prog-mode/display-buffer-alist))))

;;
;;   “INTERNAL    ← EXTERNAL”:
;; • ‘auto-save’  ← ‘real-auto-save’?
;; • ‘completion’ ← ‘helm’ 
;; • ‘vc’         ← ‘magit’
;; • ‘project’    ← ‘projectile’ 
;; • ‘flymake’    ← ‘flycheck’ 
;; • ‘skeleton’   ← ‘yasnippet’ 
;; • ‘’ ← ‘’ 

;;

(use-package files

  :init
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

  :config
  (auto-save-visited-mode +1)  ; a Global Minor Mode.

  :custom
  (auto-save-visited-interval 1)  ; = 1s (every one second).

  (auto-save-default t)
  (backup-by-copying t)

;;  :hook  (find-file . )
  )

;; URL ‘https://www.emacswiki.org/emacs/AutoSave#h5o-4’

;;

(use-package completion

  :custom

  (completion-format 'one-column)
  (completion-sort   'alphabetic)

  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case    t)

  :config

  (dolist (SUFFIX '("#" ".bin" ".cache/"))
    (add-to-list 'completion-ignored-extensions SUFFIX))
  )

;; ^ ‘complete’.el
;;
;; URL ‘https://www.masteringemacs.org/article/understanding-minibuffer-completion’
;; URL ‘https://www.gnu.org/software/emacs/manual/html_node/emacs/Ccompletion.html’

;;

(use-package minibuffer

  :custom
  (completion-styles           '(flex basic))  ;;TODO substring
  (completion-cycle-threshold  2)
  (completion-auto-select      'second-tab)

  (completion-category-overrides
    '((buffer 
       (styles . (flex)))
      (project-file 
       (styles . (fle)))
      (symbol-help 
       (styles . (initials)))
      (unicode-name 
       (styles . (substring)))))

  ;; `completion-category-defaults':
  ;; ((buffer
  ;;   (styles basic substring))
  ;;  (unicode-name
  ;;   (styles basic substring))
  ;;  (project-file
  ;;   (styles substring))
  ;;  (xref-location
  ;;   (styles substring))
  ;;  (info-menu
  ;;   (styles basic substring))
  ;;  (symbol-help
  ;;   (styles basic shorthand substring)))

  :bind
  (:map minibuffer-local-completion-map
        ("<backtab>" . minibuffer-force-complete)
        )
  (:map minibuffer-local-filename-completion-map
        ("SPC"   . minibuffer-complete-word)  ;; rebind the unbound (even though filenames can have spaces).
        )
  (:map minibuffer-local-map
        ("TAB"   . minibuffer-complete)
        )

  ;;:hook
  ;;()

  :config
  (dolist (COMPLETER '(elisp-completion-at-point comint-dynamic-complete-filename))
    (add-to-list 'completion-at-point-functions COMPLETER nil))

  )

;; ^ 
;;

;;

(use-package abbrev

  )

;; ^ ‘dabbrev’.el
;;
;; URL ‘https://www.masteringemacs.org/article/correcting-typos-misspellings-abbrev’
;; URL ‘https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines’
;; URL ‘https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html’

()

;;

(use-package isearch

  :custom
  (isearch-lazy-count   t)
  (isearch-yank-on-move t)
  (isearch-allow-motion t)
  (search-ring-max        1024)
  (regexp-search-ring-max 1024)

  )

;; ^ 
;; C-w: copy word after cursor into search minibuffer; can be repeated to copy the following words.
;; M-y: cycle through kill-ring, inserting within minibuffer.
;; M-%: change the ongoing string-search into regexp-search.
;; M-s o: change the ongoing isearch into occur.
;; M-s M-<: jump to first search-result.
;; M-s M->: jump to last search-result.
;;

;;

(use-package ibuffer

  :bind
  (:map ibuffer-mode-map
        ("<mouse-1>" . ibuffer-visit-buffer)
        )

  )

;; ^ 

;;

(use-package dired

  :bind
  (:map dired-mode-map
        ("<mouse-2>" . dired-mouse-find-file)
        )

  )

;; ^ 

;;

(use-package tool-bar
    )

;; ^ 
;; "<tool-bar> <open-file>" -> (menu-find-file-existing)
;; use-dialog-box-p
    
;;

(use-package project

  :custom
  (project-list-file "~/.emacs.d/data/project.el")

  )

;;

(use-package vc

  )

;;

(use-package compile

  :custom
  (compilation-scroll-output           t)
  (compilation-auto-jump-to-first-error 'if-location-known) 
  (compilation-always-kill             t)

  :bind
  (("M-g c" . compile)
   ("M-g r" . recompile)
   )

  :hook
  (compilation-mode . next-error-follow-minor-mode)
  )

(use-package flymake

  :custom
  (flymake-fringe-indicator-position 'right-fringe)

  :bind
  (("H-e" . flymake-show-project-diagnostics)
   )

  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  )

;;(use-package flymake-shellcheck
;;  :if (<= emacs-version 29)
;;  :commands
;;  (flymake-shellcheck-load)
;;  :hook
;;  (sh-mode . flymake-shellcheck-load)
;;  :custom
;;  ((flymake-shellcheck-use-file             t)
;;   (flymake-shellcheck-allow-external-files t)))

;;

(use-package skeleton

  :config

(define-skeleton sboo-skeleton-elisp/let
  "Insert a ‘let*’ with …."
  "Variable: "

  > "(let ((" str " ()))" \n
  > "  (" _ "))" \n)

(define-skeleton sboo-skeleton-elisp/let
  "Insert a ‘let*’ with …."
  nil

  (setq name  (skeleton-read "Name: "))
  (setq value (skeleton-read "Value: "))

  > "(let ((" name " " value "))" \n
  > "  ("  "))" \n)

(define-skeleton sboo-skeleton-elisp/cl-loop-for-in-do
  "Insert a \(‘cl-loop’ for X in XS do …\)."
  nil

  > "(cl-loop for " str " in " str \n
  > "  do ())" \n)

 )

;;

(use-package desktop
  :if window-system

  :custom
  (desktop-save                t)
  (desktop-load-locked-desktop t)

  )

(use-package desktop

    :hook ((after-init . aorst/desktop-restore)
           (desktop-after-read . aorst/desktop-remove))

    :custom
    (desktop-path '("~/.dotfiles/.config/emacs/"))
    (desktop-dirname "~/.dotfiles/.config/emacs/")
    (desktop-base-file-name "emacs-desktop")
    (desktop-save t)
    (desktop-load-locked-desktop t)

    :init
    (defun aorst/desktop-remove ()
      "Remove current desktop, but save `desktop-dirname'."
      (let ((desktop desktop-dirname))
        (desktop-remove)
        (setq desktop-dirname desktop)))

    (defun aorst/saved-desktop-p ()
      "Check if desktop exists."
      (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

    (defun aorst/desktop-restore ()
      "Restore a saved emacs session."
      (interactive)
      (desktop-save-mode t)
      (if (aorst/saved-desktop-p)
          (desktop-read)
        (message "No desktop found.")))
    )

(use-package recentf

  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  )

;;

(use-package shell

  :hook
  (shell-mode . 'compilation-shell-minor-mode)
  )

(use-package term

  )

;;

;; (use-package 

;;   )

;; ;;

;; (use-package 

;;   )

;; 

(use-package emacs

  :custom
  (set-mark-command-repeat-pop t)
  )

;; ^ 
;; C-u C-<SPC>: cycle through ‘local-mark-ring’. 
;; C-x C-<SPC>: cycle through ‘global-mark-ring’. 
;;

;;

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward)
  )

;; ^ 
;; if ‘uniquify-buffer-name-style’ is ‘post-forward’, and
;; if both “/u/rms/tmp/Makefile” and “/usr/projects/zaphod/Makefile” are visited,
;; then their visitors are named “Makefile|tmp” and “Makefile|zaphod”.
;; URL ‘https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html’

;; 

(use-package dictionary

  :custom
  (dictionary-server nil)

  :bind
  (("M-#" . #'dictionary-lookup-definition)
    ; ^ next to “M-$” (“M-x ‘ispell-word’”).
   )

  :config
  (add-to-list 'display-buffer-alist
    `("^\\*Dictionary\\*" display-buffer-in-side-window
       (side         . right)
       (window-width . 50)))
  )

;;

(use-package outline

  :hook
  (xref-after-update . outline-minor-mode))

;;

(use-package server

  :config
  (unless (server-running-p)
    (server-start))
  )

;;

(use-package help
  :custom (help-window-select t))

;;

(use-package doc-view
  :custom (doc-view-resolution 192))

;;

;;

(use-package subword
  :config
  (defalias 'sub #'subword-mode)
  (defalias 'sup #'superword-mode)

  )

(use-package emacs-lisp
  :hook
  (emacs-lisp-mode . eldoc-mode)
  )

(use-package sh-script
  :hook
  (sh-mode . flymake-mode)
  )

;;

(use-package whitespace

  :custom
  (whitespace-styles      '(lines trailing tabs))
  (whitespace-line-column 80)

  :config
  (whitespace-mode +1)
  )

;;

(use-package ediff

  :custom
  (ediff-split-window-function 'split-window-horizontally)

  :hook ((ediff-before-setup . aorst/store-pre-ediff-winconfig)
         (ediff-quit         . aorst/restore-pre-ediff-winconfig)
         )

  :init
  (defvar aorst--ediff-last-windows nil
    "Stores window configuration before `ediff' was invoked.")
  (defun aorst/store-pre-ediff-winconfig ()
    (setq aorst--ediff-last-windows (current-window-configuration)))
  (defun aorst/restore-pre-ediff-winconfig ()
    (set-window-configuration aorst--ediff-last-windows))

  :config
  (advice-add 'ediff-window-display-p :override #'ignore)
  )

;; etc:

(progn

(setq show-paren-context-when-offscreen 'overlay)

(setq process-error-pause-time 0)

(setq use-system-tooltips t)
;; ^ neé ‘x-gtk-use-system-tooltips’

(xterm-mouse-mode +1)

)

;;

(defvar sboo-set-window-parameter--name)

(defun sboo-set-window-parameter (param value &optional window)
  "Set WINDOW's PARAM to VALUE."
  (interactive (list
    (setq sboo-set-window-parameter--name (sboo-read-window-parameter-name (selected-window) nil))
    (sboo-read-window-parameter-value sboo-set-window-parameter--name)
    (selected-window)))

  (set-window-parameter window param value))

(defconst sboo-all-window-parameter-names
  '(no-delete-other-windows no-other-window side slot preserve-size))

(defun sboo-read-window-parameter-name (&optional window)
  "Read the name of a window-parameter, with completion."

  (let* ((PROMPT (or prompt "Window parameter name: "))
         (WINDOW-PARAM-BINDINGS (window-parameters window))
         (WINDOW-PARAM-NAMES (mapcar #'car WINDOW-PARAM-BINDINGS))
         (CANDIDATES (seq-uniq (append WINDOW-PARAM-NAMES sboo-all-window-parameter-names)))
         (ANNOTATION-FUNCTION (sboo-make-plist-annotation-function WINDOW-PARAM-BINDINGS))
         (completion-extra-properties (list :annotation-function ANNOTATION-FUNCTION)))

    (completing-read PROMPT CANDIDATES nil 'confirm nil 'custom-variable-history nil nil)))

(defun sboo-make-plist-annotation-function (alist)
  "Return an annotator of value-for-name within the ALIST.
For ‘completion-extra-properties’ ‘:annotation-function’."

  (lambda (key)
    (let ((VALUE (cdr-safe (assq key alist))))
      (when VALUE
        (format " = %S" VALUE)))))

(defun sboo-read-window-parameter-value (&optional param window)
  "Read a value for window-parameter PARAM (a symbol)."

  (cond (('preserve-size
          (sboo-read-window-parameter-value-for-preserve-size))

         (t
          (let* ((PROMPT (if param (format "‘%s’ value: " (symbol-name param)) "Window-parameter value: ")))
            (read-from-minibuffer PROMPT nil nil t 'read-expression-history nil nil))))))

(defun sboo-read-window-parameter-value-for-preserve-size ()
  "Read a value for the window-parameter ‘\\'preserve-size’."

  (let* ((PROMPT (or prompt ("‘preserve-size’ value (WIDTH-P . HEIGHT-P): ")))
                 (HISTORY 'read-expression-history)
        (INITIAL "(t . t)"))

    (read-from-minibuffer PROMPT INITIAL nil t HISTORY nil nil)))

;;

()

;;

;;  :ensure t :pin melpa-stable

;;

(with-demoted-errors "[sboo] %S"
  (find-file (expand-file-name "~/.emacs.d/sboo/sboo-minit.el")))

(provide 'sboo-ninit)
