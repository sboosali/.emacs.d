
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst spdx-version

  "0.1"

  "The version of this package (`spdx').")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup spdx nil
  "Completion for SPDX licenses."

  :group  'tools
  :prefix "spdx-"
  :link   '(url-link :tag "GitHub Repository" "https://github.com/sboosali/Emacs-SPDX")
  :tag    "SPDX")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom spdx-concise

  nil

  "Whether to hide (the hundreds of) obscure SPDX licenses."
  
  :type  'bool
  :safe  t
  :group 'spdx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom spdx-licenses-osi

  '(
    )
  
  "All OSI-compliant licenses."
  :type  'list                          ;TODO
  :safe  t
  :group 'spdx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom spdx-licenses-all

  (append spdx-licenses-osi '())
  
  "All SPDX-recognized licenses (OSI-compliant or not)."
  
  :type  'list                          ;TODO
  :safe  t
  :group 'spdx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defcustom spdx-licenses-concise

;;   (list
;;    (spdx-define-rule ))
  
;;   "."
;;   :type  'list                          ;TODO
;;   :safe  t
;;   :group 'spdx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst spdx-license-list-version

  "3.2"

  "The SPDX License List version (used by package).")

;; Copied from http://hackage.haskell.org/package/Cabal-2.4.0.0/docs/src/Distribution.SPDX.LicenseId.html#licenseId (mostly).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spdx-complete (Prompt)
  ""

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spdx-define-license ()
 "

 e.g. 
     (spdx-define-license :id   ""
                          :name ""
                          :osi  True)
 "

                          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'spdx)