;;; xref-conf.el --- Xref configuration for more modes

;;; Commentary:
;; Provide configuration for xref.el introduced in Emacs 25.1, for modes
;; that are not supported by default.
;;
;; Support Status:
;; [TODO] Python:
;; - [DONE] defs - symbol at point
;; - [DONE] refs - symbol at point
;; - [TODO] defs - any symbol (pycscope?)
;; - [TODO] refs - any symbol (pycscope?)
;; - [TODO] apropos - any symbol (pycscope?)
;; - [TODO] apropos - symbol at point (pycscope?)
;; [TODO] C

;;; Code:

(require 'anaconda-mode)
(require 'xcscope)
(require 'xref)

;;; Python:

(defun xref-conf-install-python ()
  "Install my `xref' config for `python-mode'."
  (interactive)
  (add-hook 'python-mode-hook #'xref-conf-install-python-local))

(defun xref-conf-install-python-local ()
  "Install my `xref' config for `python-mode' in the current buffer."
  (interactive)
  (setq-local xref-find-function #'python-xref-find)
  (setq-local xref-identifier-completion-table-function
	      ;; ugly hack
	      ;; bug: user gets 2 prompts - first `read-string', then
	      ;; `completing-read' with previous input as only candidate
	      (lambda () (list (read-string "Find: ")))))

(defun python-xref-find (action id)
  "Get a list of xref obejcts.
ACTION determines what to look for: defintions, references or apropos.
ID is the identifier being searched."
  (pcase action
    (`definitions
      (python--xref-find-definitions id))
    (`references
     (python--xref-find-references id))
    (`apropos
     nil)))

(defun python--xref-find-definitions (identifier)
  "Find definitions of IDENTIFIER.
Only works if IDENTIFIER is the symbol under the cursor."
  (or (python--xref-ac-find-definitions)
      (python--xref-xcscope-find-definitions identifier)))

(defun python--xref-find-references (identifier)
  "Find references of IDENTIFIER.
Only works if IDENTIFIER is the symbol under the cursor."
  (python--xref-ac-find-references))

;; python + anaconda

(defun python--xref-ac-find-definitions ()
  "Find definitions of thing at point using `anaconda'."
  (mapcar #'python--anaconda-reference-to-xref
	  (or (anaconda-mode-call "goto_definitions")
	      (anaconda-mode-call "goto_assignments"))))

(defun python--xref-ac-find-references ()
  "Find references of IDENTIFIER with `anaconda'."
  (mapcar #'python--anaconda-reference-to-xref
	  (anaconda-mode-call "usages")))

(defun python--anaconda-reference-to-xref (ac-ref)
  "Convert anaconda reference AC-REF to a `xref' object."
  (xref-make (plist-get ac-ref :description)
	     (xref-make-file-location (plist-get ac-ref :path)
				      (plist-get ac-ref :line)
				      (plist-get ac-ref :column))))

;; python + xcscope

(defun python--xref-xcscope-find-definitions (identifier)
  "Find definitions for IDENTIFIER using `xcscope'.
Requires a cscope DB (can be generated with pycscope)."
  (let ((buffer (get-buffer-create "*xref-cscope*")))
    (with-current-buffer buffer
      (erase-buffer))
    (when (zerop (call-process cscope-program nil buffer nil "-d" "-L" "-1" identifier))
      (with-current-buffer buffer
	(goto-char (point-min))
	(cl-loop until (= (point) (point-max))
		 for line = (buffer-substring-no-properties (point-at-bol) (point-at-eol))
		 collect (let ((fields (split-string line nil t)))
			   (xref-make (mapconcat #'identity (nthcdr 3 fields) " ")
				      (xref-make-file-location (nth 0 fields)
							       (string-to-number (nth 2 fields))
							       0)))
		 do (forward-line))))))

(provide 'xref-conf)
;;; xref-conf.el ends here
