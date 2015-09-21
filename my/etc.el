(provide 'etc)

; http://emacswiki.org/emacs/ElispCookbook#toc3

;(with-temp-buffer (insert "abcdefg") (buffer-substring 2 4))
; ==> "bc"

(defun cons! (x xs) ; ~ add-to-list
  (setq xs (cons x xs)))

(defun key (key act)                    ;deprecate 
  (global-set-key key act))

(defun configuration ()
 (pp (current-frame-configuration)))

;; (defun current-line ()
;;  (let ((line (thing-at-point 'line)))
;;   (with-temp-buffer
;;    (insert (substring-no-properties line 0 (length line)))
;;    (delete-trailing-whitespace)
;;     ; to strip final whitespace
;;    (buffer-substring 0 (- (length (buffer-string)) 1))
;;    )))

(defun current-line ()
 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun string/ends-with (string suffix)
      "Return t if STRING ends with SUFFIX."
      (and (string-match (rx-to-string `(: ,suffix eos) t)
                         string)
           t))

(defun my/ends-with (string suffix)
 (let
  ((n (length string))
   (k (length suffix)))
  (string-equal suffix (substring string (- n k) n))))

(defun string/starts-with (s begins)
      "Return non-nil if string S starts with BEGINS."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))
