;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAMES AND WINDOWS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See:
;;
;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html#Window-Frame-Parameters
;;
;;     -
;;
;;     -
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (defconst ... "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (defvar ... "")

;; (defvar sboo-frame-title "")
  ;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-maximize-frame () (interactive) 
  (set-frame-parameter nil 'fullscreen 'maximized))
  ;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-frame-parameters ()
  (interactive)
  
  (set-frame-parameter nil
                       'title "SBoo's Emacs")
  ;; ^
  ;; the title of the operating-system-window 
  ;; TODO `sboo-frame-title`

  (set-frame-parameter nil
                       'fullscreen 'maximized)
  ;; ^
  ;; fullwidth, fullheight, fullboth, or maximized
  
  ;;;;(set-frame-parameter nil 'border-width 0)
  ;; ^
  ;; ERRORS with "error: Cannot change the border width of a frame" 
  ;; The width (in pixels) of the frame's border.

  (progn
    (set-frame-parameter nil
                         'left-fringe    0)
    (set-frame-parameter nil
                         'right-fringe nil))
    ;; ^
    ;; :: Maybe Integer
    ;; (i.e. either an integer or, nil (for the default))
  
  ;; (set-frame-parameter nil 'tool-bar-position 'bottom)
  ;; top, bottom, left, or right
  ;; only works on GTK 
  
  (set-frame-parameter nil
                       'vertical-scroll-bars 'right)
  ;; ^
  ;; 'left, 'right, or nil (for no scroll bars).
  ;; there is no "'outer" option.
  ;;
  ;; "left" is more convenient for scrolling, but it's too sensitive and causes misclicks when I'm trying to click at the start of a line in the leftmost window. thus, "right".
  ;;
  ;;  to “undo” (and “redo”) changes in the window configuration with the key commands ‘C-c left’ and ‘C-c right’
  
  ;(set-frame-parameter nil ' ')
  ;; ^
  ;;

  ;(set-frame-parameter nil ' ')
  ;; ^
  ;;

  ;(set-frame-parameter nil ' ')
  ;; ^
  ;;

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFECTS/SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-frames()
  (interactive)
  (progn
    (sboo-set-frame-parameters)))
  ;; ^ 
  ;; export this function to be (optionally) called by a initialization module; e.g. by `'sboo-init`.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; unsplittable
;;
;; If non-nil, this frame's window is never split automatically.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; icon-type
;;
;; The type of icon to use for this frame. If the value is a string, that specifies a file containing a bitmap to use; nil specifies no icon (in which case the window manager decides what to show); any other non-nil value specifies the default Emacs icon.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-id
;;
;; The ID number which the graphical display uses for this frame. Emacs assigns this parameter when the frame is created; changing the parameter has no effect on the actual ID number.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; sticky
;;
;; If non-nil, the frame is visible on all virtual desktops on systems with virtual desktops.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-backend
;;
;; A list of symbols, specifying the font backends to use for drawing fonts in the frame, in order of priority. On X, there are currently two available font backends: x (the X core font driver) and xft (the Xft font driver). On MS-Windows, there are currently two available font backends: gdi and uniscribe (see Windows Fonts).
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-gamma
;;
;; If this is a number, Emacs performs gamma correction which adjusts the brightness of all colors. The value should be the screen gamma of your display.
;; Usual PC monitors have a screen gamma of 2.2, so color values in Emacs, and in X windows generally, are calibrated to display properly on a monitor with that gamma value.
;; If your monitor displays colors too light, you should specify a screen-gamma value smaller than 2.2. This requests correction that makes colors darker. A screen gamma value of 1.5 may give good results for LCD color displays.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha
;;
;; This parameter specifies the opacity of the frame, on graphical displays that support variable opacity. It should be an integer between 0 and 100, where 0 means completely transparent and 100 means completely opaque. 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-frames)