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

(require 'cl-lib)
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
(defun sboo-clipboard-get-contents ()
  "Return the contents of the system clipboard as a string."
  (condition-case nil
      (cond
       (sboo-clipboard-custom-content-provider
        (shell-command-to-string sboo-clipboard-custom-content-provider))
       ((fboundp 'ns-get-pasteboard)
        (ns-get-pasteboard))
       ((fboundp 'w32-get-clipboard-data)
        (or (w32-get-clipboard-data)
            sboo-clipboard-contents))
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
(defun sboo-clipboard-set-contents (str-val)
  "Set the contents of the system clipboard to STR-VAL."
  (callf or str-val "")
  (assert (stringp str-val) nil "STR-VAL must be a string or nil")
  (condition-case nil
      (cond
        ((fboundp 'ns-set-pasteboard)
         (ns-set-pasteboard str-val))
        ((fboundp 'w32-set-clipboard-data)
         (w32-set-clipboard-data str-val)
         (setq sboo-clipboard-contents str-val))
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

(cl-defun sboo-clipboard-dwim (&key (predicate #'sboo-case--text-string-p) )

  "Do-What-I-Mean — get a valid string from the user.

Inputs:

• PROMPT    — a `stringp'.
• PREDICATE — a `functionp'. 
  takes one `stringp' and gives a `booleanp'.
  PREDICATE defines a “valid” string.

Output:

• a `stringp' or nil. try (in order) each of:

    • the region             (if a valid string is highlighted).
    • the current sentence   (if valid).
    • the current word       (if valid).
    • the clipboard contents (if valid).
    • user input             (valid or invalid).

Links:

• URL `https://docs.microsoft.com/en-us/windows/desktop/dataxchg/standard-clipboard-formats'
• URL `'"

  (let* (
         )

    (or (condition-case _

            (when (use-region-p)
              (filter-buffer-substring (region-beginning) (region-end)))

          ;; ^ Try the current selection.

          (error nil))

         (condition-case _

             (let* ((SENTENCE (thing-at-point 'sentence))
                    (TEXT-P   (and SENTENCE
                                   (funcall predicate SENTENCE)))
                    )
               (if TEXT-P
                   SENTENCE

                 (let* ((LINE   (thing-at-point 'line))
                        (TEXT-P (and LINE
                                     (funcall predicate LINE)))
                        )
                   (if TEXT-P
                       LINE

                     nil))))

          ;; ^ Try the current sentence.

          (error nil))

        (condition-case _

            (let* ((CLIPBOARD-CONTENTS (car kill-ring))
                   (TEXT-P             (and CLIPBOARD-CONTENTS
                                            (funcall predicate CLIPBOARD-CONTENTS)))
                   )
              (if TEXT-P
                  (substring CLIPBOARD-CONTENTS 0 1)))

          ;; ^ Try the clipboard.

          (error nil))

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