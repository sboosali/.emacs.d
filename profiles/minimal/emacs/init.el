
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VARIABLES

;; '''The way I use to maintain several .emacs.d directories in parallel is the following.

;; emacs is started like this:

;; alias emacs-windows='./result/bin/emacs -q --load "~/.emacs.d-windows/init.el"'
;; alias emacs-here='./result/bin/emacs -q --load "./init.el"' # relative filepath

;; Each init.el file begins like this, to correctly set up the user-init-file and user-emacs-directory variables:

(setq user-init-file       (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; The patch which allows you to specify .emacs.d location via `EMACS_USER_DIRECTORY' environment variable is available but not merged.'''

;; e.g.
;; > M-: user-init-file

;; e.g. on Windows: 
;; > user-emacs-directory 
;; "c:/Users/Spiros/AppData/Roaming/.emacs.d/" 

(setq my-profile-name "emacs-minimal")
;; ^ distinguish this "profile" from others.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SIMPLE CUSTOMIZATION

;; I want these few settings to be alwyas present,
;; if the rest of this file fails, 
;; for easier debugging.

;; This section must not have any errors itself,
;; including not doing anything complicated.

(cua-mode t)
;; ^ the standard keybindings: C-c, C-x, C-v, C-z.

(setq cua-keep-region-after-copy t) 
;; ^ Standard Windows behaviour

(transient-mark-mode 1) 
;; ^ No region when nothing is highlighted.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;OTHER CUSTOMIZATION FILES

(require 'use-package)

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
;; e.g.
;; "~/.emacs.d/profiles/minimal/emacs/elisp/*.el"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTO-SAVING

;; auto-save should come early,
;; before re-opening files (e.g. from desktop-save-mode).

(require 'real-auto-save)

(add-hook 'fundamental-mode 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'text-mode-hook 'real-auto-save-mode)

(setq real-auto-save-interval 1) ;; in seconds

(setq auto-save-visited-file-name t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "SESSIONS"

(desktop-save-mode 1)

(setq desktop-auto-save-timeout 5) ;; in seconds 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFECTS

(find-file user-init-file) 
;; ^ should be this file itself, e.g. ".../init.el"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS

(require 'keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFERS 

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq Buffer-menu-name-width 30) ;; (setq Buffer-menu-size-width 6)

(add-hook 'Buffer-menu-mode-hook (lambda() 
  (setq Buffer-menu-files-only t)
  ;; ^ i.e. file-buffers, not all buffers.
  (revert-buffer)
  ;; ^ ?
))
;; ^ see buff-menu.el 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffap-bindings)

(setq-default indent-tabs-mode nil)
;; ^ Prevent Extraneous Tabs
;; '''Note that this line uses setq-default rather than the setq command that we have seen before;
;; The setq-default command sets values only 
;; in buffers that do not have their own local values for the variable.'''

(when (fboundp 'electric-indent-mode)
 (electric-indent-mode -1))
;; ^ disable automatic indentation on newlines(/ pressing return).

(prefer-coding-system 'utf-8)
;; ^ 

(setq
 redisplay-dont-pause t
;;  scroll-margin 10
 scroll-step 1
;;  scroll-conservatively 10000
 scroll-preserve-screen-position 1)
;; ^ 

(defalias 'yes-or-no-p 'y-or-n-p)
;; ^ can just press the Single Character "y",
;; instead of typing out "yes", 
;; for prompts.

(setq mode-require-final-newline nil)
(setq require-final-newline      nil)
;; ^ 

(set-background-color "#f4f4f4")
;; ^ i.e. R=xF4 G=xF4 B=xF4 
;; i.e. rgb(244, 244, 244)
;; i.e. faint gray (near-white)
;; 
;; https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme

;;; hide menubar and toolbar
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)

;; peek
;; (setq
;;  redisplay-dont-pause t
;;  scroll-margin 10
;;  scroll-step 1
;;  scroll-conservatively 10000
;;  scroll-preserve-screen-position 1)

(setq visible-bell              t)
(setq inhibit-splash-screen     t)
;; ^ suppresses obnoxious sights and sounds

(setq initial-scratch-message nil)
;; 

(setq enable-recursive-minibuffers t)
;; ^ e.g. can press "M-x" within a "M-x"
;; e.g. can search through (via a second "C-s") the minibuffer of a 
;; search command for the (non-mini) buffer (having pressed the first "C-s").
(minibuffer-depth-indicate-mode t)
;; ^ e.g. displays "M-x [2]" when you've double-{M-x}'d.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAMES AND WINDOWS 

;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html#Window-Frame-Parameters

;; (set-frame-parameter nil ' ')

(defun maximize-frame () (interactive) 
  (set-frame-parameter nil 'fullscreen 'maximized))

(set-frame-parameter nil 'title my-profile-name)
;; the title of the operating-system-window 
;; `my-frame-title`

(set-frame-parameter nil 'fullscreen 'maximized)
;; fullwidth, fullheight, fullboth, or maximized

;; (set-frame-parameter nil 'border-width 0)
;; ERRORS with "error: Cannot change the border width of a frame" 
;; The width (in pixels) of the frame's border.

(set-frame-parameter nil 'left-fringe    0)
(set-frame-parameter nil 'right-fringe nil)
;; ^ either: an integer; or, nil (for the default).

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
;; ^


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE SHORTCUTS (this is later to be defined after its dependent definitions)

(require 'utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically inserted by emacs via customize-variable:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package)))
 '(safe-local-variable-values
   (quote
    ((dante-repl-command-line "nix-shell" "/home/sboo/haskell/cards/default.nix" "-A" "shells.ghc" "--run" "cabal new-repl cards-frontend")
     (dante-repl-command-line "nix-shell" "/home/sboo/haskell/magic-card-search/shell.nix" "--run" "cabal repl magic-card-search")))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
