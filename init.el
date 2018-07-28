;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANTS/VARIABLES

;;TODO defvar
(setq user-init-file       (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SIMPLE CUSTOMIZATION

;;NOTE
;;
;; I want these few settings to be always present,
;; even if the rest of this file fails, 
;; for easier debugging.
;;
;; Thus, this section must not have any errors itself,
;; nor do anything too complicated.

(cua-mode t)
;; ^ the standard keybindings: C-c, C-x, C-v, C-z.

(setq cua-keep-region-after-copy t) 
;; ^ Standard Windows behaviour.

(transient-mark-mode 1) 
;; ^ No region when nothing is highlighted.

(prefer-coding-system 'utf-8)
;; ^ TODO

(setq
 ;; "peeking" behavior when scrolling.
 redisplay-dont-pause t
 ;; ^
 ;; scroll-margin 10
 ;; ^
 scroll-step 1
 ;; ^
 ;; scroll-conservatively 10000
 ;; ^
 scroll-preserve-screen-position 1)
 ;; ^

(defalias 'yes-or-no-p
  'y-or-n-p)
  ;; ^
  ;; so you can just press one key for prompts
  ;; (i.e. the single character "y",
  ;; instead of typing out the phrase "yes").

(setq
 require-final-newline      nil
 ;; ^ 
 mode-require-final-newline nil)
 ;; ^ 

(set-background-color "#f4f4f4")
 ;; ^
 ;; i.e. R=xF4 G=xF4 B=xF4 
 ;; i.e. rgb(244, 244, 244)
 ;; i.e. faint gray (near-white)
 ;;
 ;; see:
 ;;     https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme
 
(setq
 visible-bell              t)
 ;; ^ on user errors,
 ;; flash a black square on to the screen
 ;; instead of honking loudly through your speakers.

(setq
 ;; fewer startup buffers
 inhibit-splash-screen     t
 ;; ^ 
 initial-scratch-message nil)
 ;; ^ 

(progn
 ;; minibuffer settings.
 (setq
  enable-recursive-minibuffers t)
  ;; ^
  ;; e.g. you can press "M-x" within a "M-x".
  ;; e.g. you can search through (via a second "C-s") the minibuffer of a search command for the (non-mini) buffer (having pressed the first "C-s").
 (minibuffer-depth-indicate-mode t))
 ;; ^
 ;; e.g. displays "M-x [2]" when you've
 ;; (often accidentally) double-{M-x}'d.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;OTHER CUSTOMIZATION FILES

;;(require 'use-package)

(add-to-list
 'load-path
 (concat user-emacs-directory "elisp/"))

 ;; ^ e.g.
 ;; "~/.emacs.d/profiles/default/emacs/elisp/*.el"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTO-SAVING

;; auto-save should come early,
;; before re-opening files (e.g. from desktop-save-mode).

(require 'real-auto-save)

(add-hook 'find-file-hook        'real-auto-save-mode)
;;(add-hook 'fundamental-mode-hook 'real-auto-save-mode)
;;(add-hook 'prog-mode-hook        'real-auto-save-mode)

(setq real-auto-save-interval 1) ;; in seconds

(setq auto-save-visited-file-name t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DESKTOP
;; 
;; (Multiple) Desktop "Sessions"
;; 

;;;; `desktop` builtin:
(progn
  (setq
   desktop-auto-save-timeout 5)
   ;; ^ unit of time is seconds.
  (desktop-save-mode 1)
   ;; ^ (enable a mode after configuring its variables).
)

;; NOTES relevant `desktop-*` functions and variables:
;;
;; desktop-read
;; desktop-save
;; desktop-save-mode
;; desktop-save-hook
;; desktop-enable 
;; desktop-base-file-name
;; desktop-auto-save-timeout
;; desktop-auto-save-timer
;; desktop-buffers-not-to-save
;; desktop-files-not-to-save
;; desktop-modes-not-to-save
;; desktop-path
;; desktop-restore-eager
;; desktop-restore-frames
;; desktop-load-locked-desktop
;;
;; NOTE
;; 
;; To manually interact with your desktop session at any time, use:
;; - the command ‘M-x desktop-save’ to save it.
;; - the command ‘M-x desktop-read’ to load it.
;;
;; 
	
;;;; the `desktop+` package:
;; 
;; (require 'desktop+)
;; (defconst sboo-desktop-name-default
;;  "sboo")
;; (desktop-create sboo-desktop-name-default)
;; (desktop+-load   sboo-desktop-name-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HISTORY
;; the builtin `savehist` library.
;; 

(require 'savehist)

(progn

  (setq savehist-additional-variables
        '(search-ring
          regexp-search-ring
          compile-history
          kill-ring))
  ;; ^ 
  ;; By default, Savehist mode saves only your minibuffer histories.
  ;; The line above saves your search strings, etc, too.
  ;;
  ;; All minibuffer histories are saved automatically,
  ;; so you don’t need to add minibuffer history variables to the
  ;; `savehist-additional-variables` list.
  ;; 

  (setq savehist-file
        (concat user-emacs-directory "persisted/savehist/savehist.el"))

  (savehist-mode 1)
  ;; ^ enabling `savehist-mode` must come after all `savehist-*` customizations
  ;; (otherwise, they will be ignored).
)
;; ^
;; NOTE, Emacs truncates minibuffer history automatically,
;; so the file shouldn't grow indefinitely.

;;;;NOTES:
;;
;; `savehist` Persists arbitrary `read`able `elisp` variables.
;;
;; Values which can't be read back can't be saved and restored, but anything else could be persisted.
;; savehist doesn't care whether or not a variable "changes regularly", but if the variable doesn't contain a history of values then savehist isn't necessarily very useful -- savehist doesn't track the sequence of values of a variable over time; it just saves the current value of the variables it's interested in. Obviously that's fine for variables containing a list of historical values. For variables with a single changing value, you would just be remembering the most recent one of those (and any changes to that value which occurred between savehist saves wouldn't be noticed at all).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFECTS

(progn
  (find-file user-init-file)
  ;; ^ which should open this file itself,
  ;; e.g. ".../init.el".
  (find-file "~/haskell/")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "SBOO", my configurations and utilities

;;TODO
;(require 'sboo-install)

(progn

 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/"))
 
 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/utilities/"))
 
 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/packages/"))
 
 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/configurations/"))

)

;; (add-to-list 'load-path
;;  (concat user-emacs-directory "elisp/sboo/applications/"))

(require 'sboo)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; KEYBINDINGS

;; (require 'sboo-keybindings)

;; ;; ^ my keybindings. 
;; ;; "sboo" is my username.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFERS 

(setq
 ediff-window-setup-function 'ediff-setup-windows-plain)

(setq
 Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)

(add-hook 'Buffer-menu-mode-hook (lambda() 
  (setq Buffer-menu-files-only t)
  ;; ^ i.e. file-buffers, not all buffers.
  (revert-buffer)
  ;; ^ ?
))
;; ^ see buff-menu.el 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffap-bindings)

(setq-default
 indent-tabs-mode nil)
 ;; ^ Prevent Extraneous Tabs
 ;; '''Note that this line uses setq-default rather than the setq command that we have seen before;
 ;; The setq-default command sets values only 
 ;; in buffers that do not have their own local values for the variable.'''
 
;; (when (fboundp 'electric-indent-mode)
;;  (electric-indent-mode -1))
;;  ;; ^ disable automatic indentation on newlines(/ pressing return).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAMES AND WINDOWS 

;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html#Window-Frame-Parameters

;; (set-frame-parameter nil ' ')

(defun maximize-frame () (interactive) 
  (set-frame-parameter nil 'fullscreen 'maximized))

(set-frame-parameter nil 'title "SBoo's Emacs")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; `compilation-mode`

;;TODO
(setq compilation-always-kill t)
;; ^


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; `use(d)-package(s)`

(require 'use-package)

(use-package term
  :bind
  (("C-c t" . term)
   :map term-mode-map
   ("<kp-prior>" . term-send-up)
   ("<kp-next>"  . term-send-down)
   ("<tab>"      . self-insert-command)));;TODO
   ;; ^ "Keys defined by global-set-key are shadowed by any local binding."


;; see:
;;      https://github.com/jwiegley/use-package/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; `hl-line` Highlighting the Current Line

(global-hl-line-mode 1)
  ;; ^ enable hl-line.
  ;; i.e. continuously highlight the current line of the active window (of the topmost frame?)

(set-face-background 'hl-line "#dedede")
  ;; ^ Set a color as the background face of the current line.
  ;; "#dedede" is Light-Gray.

(set-face-foreground 'highlight nil)
  ;; ^ keep syntax highlighting in the current line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; registers

;; (setq register-separator ?+)
;; (set-register register-separator "\n\n")
;;
;; (defun yank-register() 
;;  (interactive)
;; )

(defun sboo-paste-from-register (register-character)
  (interactive "cPaste from Register:")
  (insert-register register-character 't))

(defun sboo-copy-into-register (register-character)
  (interactive "cCopy to Register:")
  (copy-to-register register-character (region-beginning) (region-end))
  (cua-cancel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "EFFECTS"

;; (defun server-start-once ()
;;   "Start an Emacs Server for the `emacsclient` command, 
;;   unless a server is already running.
;;   We check for the presence of another Emacs Server 
;;   via the existence of a specific socket; for example,
;;   named \"/tmp/emacs1001/server\".
;;   `server-start-once` is idempotent, modulo race conditions."
;;   (interactive)
;;   (let 
;;       ( (server-socket-file "/tmp/emacs1001/server") ;;TODO shell out for "$UID"
;;          ;; ^
;;          ;; e.g. "/tmp/emacs1001/server"
;;          ;; i.e. "/tmp/emacs<UserId>/<ServerName>"
;;          ;; 
;;          ;; NOTE when evaluated from a running Emacs Server,
;;          ;; `server-socket-file` should equal `(concat server-socket-dir server-name)`
;;          ;; (otherwise, some `server-*` variables are undefined).
;;       )
;;     (unless (file-exists-p server-socket-file)
;;       ;; ^ check whether another server (named `server-name`) is already running.
;;       (server-start)))
;; )

(defun server-start-unless-running ()
  "Run `server-start`,
   unless another Emacs Server is already running (for the same user).
   Platform-compability: only Unix."
  (interactive)
  (let* ((tmp
          "/tmp")
          ;; (getenv "TMPDIR"))
          ;; ;; ^ i.e. "$TMPDIR"
         (uid
          (number-to-string (user-real-uid)))
          ;; ^ i.e. "$UID"
         (server-socket-file
          (concat tmp "/" "emacs" uid "/" "server")))
          ;; ^
          ;; e.g. "/tmp/emacs1001/server"
          ;; i.e. "/tmp/emacs<UserId>/<ServerName>"
  (unless (file-exists-p server-socket-file)
    (server-start))))
  ;; ^ via @markhellewell

(server-start-unless-running)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://emacs.stackexchange.com/questions/31224/how-to-test-programmatically-whether-the-current-emacs-session-among-several
;; 
;; "server-running-p predicate will evaluate to t if the Emacs server is running, irrespective of which Emacs session currently "owns" the server process."
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE SHORTCUTS (this is later to be defined after its dependent definitions)

;;(require 'sboo-utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs.d/custom.el")
;; ^ the `custom-file` is automatically managed (i.e. inserted into) by emacs,
;; via `customize-variable`.
(load custom-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;