(provide 'my-settings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-functions)
(require 'recentf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq enable-local-variables :all)

(put 'dante-target 'safe-local-variable 'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)
(add-hook 'Buffer-menu-mode-hook (lambda() 
;;  (setq Buffer-menu-files-only t) 
  (revert-buffer)))
  ;; see buff-menu.el 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffap-bindings)

;; Prevent Extraneous Tabs
;; Note that this line uses setq-default rather than the setq command that we have seen before. The setq-default command sets values only in buffers that do not have their own local values for the variable.
(setq-default indent-tabs-mode nil)

;; disable automatic indentation on newlines 
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;;(add-hook 'after-init-hook 'global-company-mode)

(prefer-coding-system 'utf-8)

(setq
 redisplay-dont-pause t
;;  scroll-margin 10
 scroll-step 1
;;  scroll-conservatively 10000
 scroll-preserve-screen-position 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

(set-background-color "#f4f4f4")
;; https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme

;; hide menubar and toolbar
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)

;; peek
;; (setq
;;  redisplay-dont-pause t
;;  scroll-margin 10
;;  scroll-step 1
;;  scroll-conservatively 10000
;;  scroll-preserve-screen-position 1)

;; suppresses obnoxious sights and sounds
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAMES AND WINDOWS 


;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html#Window-Frame-Parameters

;; (set-frame-parameter nil ' ')

(set-frame-parameter nil 'title "EMACS") 
;; the title of the operating-system-window 

(set-frame-parameter nil 'fullscreen 'maximized)
;; fullwidth, fullheight, fullboth, or maximized

;; (set-frame-parameter nil 'border-width 0)
;; ERRORS with "error: Cannot change the border width of a frame" 
;; The width (in pixels) of the frame's border.

(set-frame-parameter nil 'left-fringe 0)
(set-frame-parameter nil 'right-fringe nil)
;; an integer, or nil (for the default) 

;; (set-frame-parameter nil 'tool-bar-position 'bottom)
;; top, bottom, left, or right
;; only works on GTK 

;; unsplittable
;; If non-nil, this frame's window is never split automatically.

;; icon-type
;; The type of icon to use for this frame. If the value is a string, that specifies a file containing a bitmap to use; nil specifies no icon (in which case the window manager decides what to show); any other non-nil value specifies the default Emacs icon.

;; window-id
;; The ID number which the graphical display uses for this frame. Emacs assigns this parameter when the frame is created; changing the parameter has no effect on the actual ID number.

;; sticky
;; If non-nil, the frame is visible on all virtual desktops on systems with virtual desktops.

;; font-backend
;; A list of symbols, specifying the font backends to use for drawing fonts in the frame, in order of priority. On X, there are currently two available font backends: x (the X core font driver) and xft (the Xft font driver). On MS-Windows, there are currently two available font backends: gdi and uniscribe (see Windows Fonts).

;; screen-gamma
;; If this is a number, Emacs performs gamma correction which adjusts the brightness of all colors. The value should be the screen gamma of your display.
;; Usual PC monitors have a screen gamma of 2.2, so color values in Emacs, and in X windows generally, are calibrated to display properly on a monitor with that gamma value.
;; If your monitor displays colors too light, you should specify a screen-gamma value smaller than 2.2. This requests correction that makes colors darker. A screen gamma value of 1.5 may give good results for LCD color displays.

;; alpha
;; This parameter specifies the opacity of the frame, on graphical displays that support variable opacity. It should be an integer between 0 and 100, where 0 means completely transparent and 100 means completely opaque. 

(set-frame-parameter nil 'vertical-scroll-bars 'right)
;; left, right, or nil (for no scroll bars) 
;; there is no "outer" option 
;; "left" is more convenient for scrolling, but it's too sensitive and causes misclicks when I'm trying to click at the start of a line in the leftmost window. thus, "right".  
;;  to “undo” (and “redo”) changes in the window configuration with the key commands ‘C-c left’ and ‘C-c right’
(when (fboundp 'winner-mode)
      (winner-mode 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (setq mode-require-final-newline nil)
;; (setq require-final-newline nil)

;; (set-background-color "gray")

;; ;; hide menubar and toolbar
;; ;; (menu-bar-mode -1)
;; ;; (tool-bar-mode -1)

;; ;; peek
;; (setq
;;  redisplay-dont-pause t
;;  scroll-margin 10
;;  scroll-step 1
;;  scroll-conservatively 10000
;;  scroll-preserve-screen-position 1)

;; ;; suppresses obnoxious sights and sounds
;; (setq visible-bell t)
;; (setq inhibit-splash-screen t)
;; (setq initial-scratch-message nil)

;; ;; Remove text in active region if inserting text
;; ;;;  commented out because it's easy to lose your work
;; ;; (pending-delete-mode t)

;; ;; font-size
;; ;(set-face-attribute 'default nil :height 50)
;; ;(set-frame-parameter nil 'font "Monospace-2")

;; ; http://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
;; ; problem: font stays small
;; ; problem: new file opens (up to four) new windows 
;; ;; (add-hook 'after-make-frame-functions (lambda() (if (display-graphic-p)
;; ;;  (set-default-font "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1"))))
;; ;; ;;; solution: 
;; ;; (set-default-font "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")

;; (setq keyboard-coding-system nil)

;; ;; Save a list of recent files visited.
;; ;; (recentf-mode 1) ;;; all the apps share the same recent file. disabled to prevent opening an old random file, from the voice command "buffer _" 
;; (setq recentf-max-saved-items 100) ;; just 20 is too recent
;; (global-set-key "\C-l" nil)
;; (global-set-key "\C-l \C-r" 'recentf-open-files)

;; ;; Prevent Emacs from extending file when
;; ;; pressing down arrow at end of buffer.
;; (setq next-line-add-newlines nil)

;; ;; don't silently ensure newline at end of file
;; (setq require-final-newline nil)

;; ;;; lines can be any length
;; ;; http://stackoverflow.com/questions/11061453/in-emacs-how-to-disable-comment-auto-indent-in-c-c
;; (setq-default fill-column 1000)

;; ; disable splitting long lines into short lines
;; (auto-fill-mode -1)

;; ;;; Unicode
;; (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
;; (set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
;; (setq locale-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; ;;; ?
;; ;; (setq left-margin-width nil)
;; ;; (setq right-margin-width nil)

;; ; prevent stupid disabling pop-up
;; (put 'upcase-region 'disabled nil)

;; ;; must only be set once (because it writes not appends?)
;; ;; (custom-set-faces
;; ;;   '(font-lock-comment-face ((t (:foreground "MediumAquamarine")))))

;; ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Output-Variables.html
;; (setq print-length nil)

;; ; delete seleted text when typing
;; ; (delete-selection-mode 1)

;; ; http://stackoverflow.com/questions/11380918/clean-way-to-perform-commands-in-the-emacs-minibuffer
;; ; for commands' evalEmacs
;; (setq enable-recursive-minibuffers t)
;; (minibuffer-depth-indicate-mode t)

;; ;;; make Emacs' path environment variable, i.e. (getenv "PATH"), consistent with your shell's
;; ;;; from exec-path-from-shell
;; ;; (when (memq window-system '(mac ns))
;; ;;  (exec-path-from-shell-initialize))

;; ;; (when (equal system-type 'darwin)
;; ;;   (set-exec-path-from-shell-path))

;; ; http://emacswiki.org/emacs/EvaluatingExpressions
;; (global-set-key [remap eval-expression] 'pp-eval-expression)
;; (global-set-key [remap eval-last-sexp]  'pp-eval-last-sexp)

;; ; don't warn 
;; (put 'upcase-region 'disabled nil)

;; ; disabled. otherwise, when enabled, it awkwardly reindents the preceding line
;; (electric-indent-mode -1)

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; mouse scroll one line at a time 
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;; ;; {{$ open -a Emacs file.txt}} now opens the file in the current frame 
;; (setq ns-pop-up-frames nil)

;; ;; disable "double hyphen as dash" TeX input
;; ;; input method 
;; ;; M-x input-method-function TeX
;; ;; (quail-defrule "-" "-" "TeX")           ;doesn't work, ignored  
;; ;; (global-set-key "-" '(lambda () (interactive) (insert "-")))           ;doesn't work, ignored 
;; ;; (global-set-key "-" "-<spc><del>")           ;doesn't work, infinite recursion 
;; ;; post-self-insert-hook           ;? 
