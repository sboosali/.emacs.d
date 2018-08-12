;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Purpose: Framework For Categorizing Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(use-package window-purpose

  :init
  (setq purpose-layout-dirs
    `(,(sboo-database-path "layouts/")))
  ;; (setq purpose-use-default-configuration nil)

  :config
  (add-to-list 'purpose-user-mode-purposes
               '(haskell-mode . hs))
  
  (purpose-compile-user-configuration)
  ;;(purpose-save-window-layout)
  (purpose-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g.
;;
;; (require 'window-purpose)
;; (purpose-mode)
;; ;;
;; ;; c-programming purpose configuration:
;; ;;
;; (add-to-list 'purpose-user-mode-purposes '(c-mode   . c-file-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(asm-mode . c-file-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(c++-mode . c-file-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(cscope-list-entry-mode . cscope-purpose))
;; (add-to-list 'purpose-user-mode-purposes '(compilation-mode . compilation-purpose))
;; ;;
;; ;; build it
;; (purpose-compile-user-configuration)
;; ;;

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration.

;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))


;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))


;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))


;; (setq purpose-use-default-configuration t) 
;;  ;; ^ default is `t'.


;; (purpose-compile-user-configuration)
;;  ;; ^ activates your changes

 
;; e.g.

;;  M-x customize-group purpose

;;     "Purpose User Mode Purposes": recognize purpose according to major mode
;;     "Purpose User Name Purposes": recognize purpose according to buffer name (for exact names)
;;     "Purpose User Regexp Purposes": recognize purpose according to buffer name (for name patterns)
;;     "Purpose Use Default Configuration": toggle default configuration on/off


;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usage.

;; Dedicating Windows

;; Dedicating a window limits which buffers will be displayed in it. There are two types of window dedication: 
;; - buffer-dedication;
;; - purpose-dedication.

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example.


;; step 1: configuration

;;   (add-to-list 'purpose-user-mode-purposes
;;     '(python-mode . py))


;;   (add-to-list 'purpose-user-mode-purposes 
;;     '(inferior-python-mode . py-repl))


;;   (purpose-compile-user-configuration)



;; step 2: change window layout

;; (If you have a previously saved layout, you can load it with purpose-load-window-layout and skip step 2 and step 3.)


;; (1)
;;     open a Python file

;; (2) 
;;     C-c , d (purpose-toggle-window-purpose-dedicated) so window is dedicated ("[py]" in the status bar will change to "[py!]")

;; (3)
;;     C-x 1 (delete-other-windows)

;; (4)
;;     C-x 2 (split-window-below)

;; (5)
;;     C-c C-z (python-shell-switch-to-shell)

;; (6)
;;     C-c , d so window is dedicated

;; (7)
;;     C-x o (other-window) to select the python file's window

;; (8)
;;     C-x ^ (enlarge-window) until you like the sizes of the windows



;; step 3: save window layout

;;   M-x purpose-save-window-layout


;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dedicating Windows:

;; Dedicating a window limits which buffers will be displayed in it. 

;; There are two types of window dedication:
;; - buffer-dedication,
;; - purpose-dedication.


;; - `purpose-toggle-window-buffer-dedicated':
;; Dedicate a window to its buffer. 
;; This window will not display any other buffer while it is buffer-dedicated. 
;; A "#" in the mode-line next to the window's purpose indicates that the window is buffer-dedicated.

;; - `purpose-toggle-window-purpose-dedicated':
;; Dedicate a window to its purpose. 
;; This window will only display buffers with the same purpose. 
;; A "!" in the mode-line next to the window's purpose indicates that the window is purpose-dedicated.


;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switching Buffers.

;; When switching buffers, Purpose will display the new buffer in the correct window, according to the current configuration.

;; switch-to-buffer
;; Switch to any buffer. The buffer will be displayed according to the current purpose-configuration.

;; Use purpose-switch-buffer-with-purpose to switch to another buffer with the same purpose as the current buffer.

;; Use purpose-switch-buffer-with-some-purpose to select a purpose and then switch to a buffer with that purpose.

;; Use switch-buffer-without-purpose to switch to any buffer. The buffer will be displayed using Emacs' original behavior. This is useful when you want to change the window layout.

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Persisting Window Layouts.


;; - `purpose-save-window-layout':

;; Save the current window layout. 
;; In a file named `<layout-name>.window-layout'


;; - `purpose-load-window-layout':

;; Load a window layout.
;; From a directory in `purpose-layout-dirs'.


;; ;; ditto all the "window layout" function/variables above, for "frame layout".

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Changing Purpose Configuration.

;; Define your own purposes, with the variables:
;; - purpose-user-mode-purposes
;; - purpose-user-name-purposes
;; - purpose-user-regexp-purposes

;; You can deactivate the default purpose with:

;;     (setq purpose-use-default-configuration nil)


;;;;;;;;;;;;;;;;;;;;;;;;;

;; Respect Purposes When Killing A Buffer.

;; When killing a visible buffer, Emacs has to decide which buffer to show instead. Enable with:

;;   (require 'window-purpose-x)
;;   (purpose-x-kill-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Changing Where Helm Buffers are Displayed.

;; Since Purpose ignores Helm buffers, you can't use it to control the display of Helm buffers. To change how Helm buffers are displayed you can use Helm's variable `helm-display-function'. 

;; For example, to always display Helm buffers in the bottom of the screen, without hiding current windows:

;; (defvar my-helm-window-height 0.3)
;; (defun my-helm-display-buffer-at-bottom (buffer &optional _resume)
;;   (let ((window (or (purpose-display-reuse-window-buffer buffer nil)
;;                     (purpose-display-reuse-window-purpose buffer nil)
;;                     (purpose-display-at-bottom buffer nil my-helm-window-height))))
;;     (if window
;;         (progn
;;           (select-window window)
;;           ;; don't know why, but it doesn't work without `switch-to-buffer'
;;           (switch-to-buffer buffer t t))
;;       ;; in case the above methods weren't successful, fallback to default
;;       ;; helm display function
;;       (funcall #'helm-default-display-buffer buffer))))
;; (setq helm-display-function #'my-helm-display-buffer-at-bottom)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; See:
;; 
;; - https://github.com/bmag/emacs-purpose/blob/master/README.md
;;
;; - https://github.com/bmag/emacs-purpose/wiki/Integration-With-Other-Packages
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-purpose)