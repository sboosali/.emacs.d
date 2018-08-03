;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-set-font-to-iosevka ()
   "Sets a custom font for code, `Iosevka`, in the current buffer, if present.
   By @HanfeiSun.
   By /u/MrTJC."
   (interactive)

   (cond
    
    ((find-font (font-spec :name "Iosevka"))
     (progn
       (setq
        buffer-face-mode-face '(:family "Iosevka"))
       (buffer-face-mode)))))

;; ^ `Iosevka` is an open-source font, designed for code.
;;
;; See
;;     - https://be5invis.github.io/Iosevka/
;;

; [Font Test] char and monospace:
; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-fonts)