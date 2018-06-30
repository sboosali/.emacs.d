;; (require 'color-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-: (face-attribute 'default :background)
;; "#2e3436"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun darkroom-mode ()
	"Make things simple-looking by removing decoration 
	 and choosing a simple theme."
        (interactive)
;        (switch-full-screen 1)     ;; requires above function 
;;	(color-theme-retro-green)  ;; requires color-theme
;        (setq left-margin 10)
;        (menu-bar-mode -1)
;        (tool-bar-mode -1)
;        (scroll-bar-mode -1)

;        (set-face-foreground 'mode-line "gray15")
;        (set-face-background 'mode-line "black")
        (set-face-background 'default   "#2e3436")
        (auto-fill-mode 1)
)

(defun darkroom-mode-reset ()
   (interactive)
;   (switch-full-screen -1)
;   (color-theme-subtle-hacker) ;; Choose your favorite theme
;   (menu-bar-mode 1)
;   (tool-bar-mode 1)
;   (scroll-bar-mode 1)
;   (set-left-margin 0)
)	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (provide 'colors)
