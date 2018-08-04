;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-font (FONT)
  "Sets the current-buffer's (face-)font to `FONT`. 
  `FONT` is a string.
  e.g. `M-: (sboo-set-font \"Iosevka\")`
  e.g. `M-x sboo-set-font RET Iosevka`
  "

  (interactive "sFont name [C-h v font-family-list]: ")
  ;; ^ `"s"` means "read a string from the user until they press RET".

  (if (find-font (font-spec :name FONT))
    
    (progn
      (buffer-face-set `(:family ,FONT))
      (buffer-face-mode)
      t)
    
    nil))

;; ^
;; Starting with Emacs 23, you can set the face for the current buffer, using ‘M-x buffer-face-set’. You can toggle this on/off using ‘M-x buffer-face-mode’. 
;; Internally, this uses ‘face-map-add-relative’ to remap faces. For example, (face-remap-add-relative 'default :family "Source Code Pro" :height 140)
;; 
;; also, you can list all fonts with `M-: (print (font-family-list))`.
;;

;;OLD;;
;;                            (setq buffer-face-mode-face '(:family "Inconsolata"))
;;                            (buffer-face-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-font-to-iosevka ()
  (sboo-set-font "Iosevka"))

;; ^ `Iosevka` is an open-source font, designed for code.
;;
;; See
;;     - https://be5invis.github.io/Iosevka/
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-fonts ()
  "My font-related configuration.
  Use the \"Iosevka\" font for all buffers with code;
  i.e. whose `major-mode` inherits from `prog-mode`,
  (e.g. `lisp-mode`, `haskell-mode`, `nix-mode`, etc).
  " 

  (add-hook 'prog-mode-hook
            'sboo-set-font-to-iosevka))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun sboo-set-font-to-iosevka ()
;;    "Sets a custom font for code, `Iosevka`, in the current buffer, if present.
;;    By @HanfeiSun.
;;    By /u/MrTJC."
;;    (interactive)

;;    (cond
    
;;     ((find-font (font-spec :name "Iosevka"))
;;      (progn
;;        (setq
;;         buffer-face-mode-face '(:family "Iosevka"))
;;        (buffer-face-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See 
;;     - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-font-check.el
;;     - https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode
;;     - https://www.emacswiki.org/emacs/FacesPerBuffer
;; 
;;

; [Font Test] char and monospace:
; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-fonts)