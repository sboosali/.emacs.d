;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; .
;; 
;; • 
;; • 
;;
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl)
(require 'pcase)
(require 'seq)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defun simpleclip-get-contents ()
  "Return the contents of the system clipboard as a string."
  (condition-case nil
      (cond
       (simpleclip-custom-content-provider
        (shell-command-to-string simpleclip-custom-content-provider))
       ((fboundp 'ns-get-pasteboard)
        (ns-get-pasteboard))
       ((fboundp 'w32-get-clipboard-data)
        (or (w32-get-clipboard-data)
            simpleclip-contents))
       ((and (featurep 'mac)
             (fboundp 'gui-get-selection))
        (gui-get-selection 'CLIPBOARD 'NSStringPboardType))
       ((and (featurep 'mac)
             (fboundp 'x-get-selection))
        (x-get-selection 'CLIPBOARD 'NSStringPboardType))
       ;; todo, this should try more than one request type, as in gui--selection-value-internal
       ((fboundp 'gui-get-selection)
        (gui-get-selection 'CLIPBOARD (or x-select-request-type 'UTF8_STRING)))
       ;; todo, this should try more than one request type, as in gui--selection-value-internal
       ((fboundp 'x-get-selection)
        (x-get-selection 'CLIPBOARD (or x-select-request-type 'UTF8_STRING)))
       (t
        (error "Clipboard support not available")))
    (error
     (condition-case nil
         (cond
          ((eq system-type 'darwin)
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "/usr/bin/pbpaste" nil t nil "-Prefer" "txt"))))
          ((eq system-type 'cygwin)
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "getclip" nil t nil))))
          ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "xsel" nil t nil "--clipboard" "--output"))))
          (t
           (error "Clipboard support not available")))
       (error
        (error "Clipboard support not available"))))))

;;----------------------------------------------;;

;;;###autoload
(defun simpleclip-set-contents (str-val)
  "Set the contents of the system clipboard to STR-VAL."
  (callf or str-val "")
  (assert (stringp str-val) nil "STR-VAL must be a string or nil")
  (condition-case nil
      (cond
        ((fboundp 'ns-set-pasteboard)
         (ns-set-pasteboard str-val))
        ((fboundp 'w32-set-clipboard-data)
         (w32-set-clipboard-data str-val)
         (setq simpleclip-contents str-val))
        ((fboundp 'gui-set-selection)
         (gui-set-selection 'CLIPBOARD str-val))
        ((fboundp 'x-set-selection)
         (x-set-selection 'CLIPBOARD str-val))
        (t
         (error "Clipboard support not available")))
    (error
     (condition-case nil
         (cond
           ((eq system-type 'darwin)
            (with-temp-buffer
              (insert str-val)
              (call-process-region (point-min) (point-max) "/usr/bin/pbcopy")))
           ((eq system-type 'cygwin)
            (with-temp-buffer
              (insert str-val)
              (call-process-region (point-min) (point-max) "putclip")))
           ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
            (with-temp-buffer
              (insert str-val)
              (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input")))
           (t
            (error "Clipboard support not available")))
       (error
        (error "Clipboard support not available"))))))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;==============================================;;
(provide 'sboo-clipboard)