;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-font (FONT)
  "Sets the font of the `current-buffer' to `FONT`. 
  
  `FONT` is a string.

  e.g. M-: (sboo-set-font ‘Iosevka’)
  e.g. M-x sboo-set-font RET Iosevka
  "

  (interactive "sFont name [C-h v font-family-list]: ") ;;TODO `try-completion; from `font-family-list'.
  
  ;; ^ `"s"` means "read a string from the user until they press RET".

  (if (find-font (font-spec :name FONT))

      ;; ^ 
      ;; e.g. (find-font (font-spec :name "iosevka"))
      ;; e.g. (find-font (font-spec :name "garamond"))
    
    (progn
      (buffer-face-set `(:family ,FONT))
      (buffer-face-mode)
      t)
    
    nil))

;; ^
;;
;; NOTE to list all (available) fonts:
;; 
;;     M-: (print (font-family-list))
;;
;; > Starting with Emacs 23, you can set the face for the current buffer, using ‘M-x buffer-face-set’. 
;; > You can toggle this on/off using ‘M-x buffer-face-mode’. 
;; > Internally, this uses ‘face-map-add-relative’ to remap faces. 
;; > For example, (face-remap-add-relative 'default :family "Source Code Pro" :height 140)
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-font-to-iosevka ()
  (interactive)
  (sboo-set-font "Iosevka"))

;; ^ `Iosevka' is an open-source font, designed for code.
;;
;; See
;;     - https://be5invis.github.io/Iosevka/
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-fonts ()

  "Configuration of Fonts.

  Use the ‘Iosevka’ font for all buffers with code.

  (i.e. whose `major-mode' inherits from `prog-mode';
  e.g. `lisp-mode', `haskell-mode', `nix-mode', etc).
  " 

  (add-hook 'prog-mode-hook
            #'sboo-set-font-to-iosevka))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See 
;;     - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-font-check.el
;;     - https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode
;;     - https://www.emacswiki.org/emacs/FacesPerBuffer
;; 
;;

;; [Font Test] char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings-fonts)