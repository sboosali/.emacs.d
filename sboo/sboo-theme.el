;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Primary Colors: red, green, blue

(defvar sboo-inverted-red "Cyan"
  "The color « red » is (additively-)inverted « cyan ».")

(defvar sboo-inverted-green "Magenta"
  "The color « green » is (additively-)inverted « magenta ».")

(defvar sboo-inverted-blue "Orange"
  "The color « blue » is (additively-)inverted « orange ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Secondary Colors: yellow, cyan, magenta

(defvar sboo-inverted-yellow "Blue"
  "The color « yellow » is (additively-)inverted « blue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common Colors

(defvar sboo-inverted-purple "Chocolate"
  "The color « purple » is (additively-)inverted « chocolate ».")

(defvar sboo-inverted-orange "DeepSkyBlue"
  "The color « orange » is (additively-)inverted « DeepSkyBlue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hot Colors (i.e. Neon, that "pop" against a dark background)

(defvar sboo-inverted-pink "Lime"
  "The color « hot pink » is (additively-)inverted « lime ».")

(defvar sboo-inverted-lime "Fuchsia"
  "The color « lime green » is (additively-)inverted « Fuchsia ».")

(defvar sboo-inverted-hot-orange "DeepSkyBlue"
  "The color « hot orange » is (additively-)inverted « DeepSkyBlue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Soft Colors

(defvar sboo-inverted-olive "DarkSlateBlue"
  "The color « olive » is (additively-)inverted « Dark Slate Blue ».")

(defvar sboo-inverted-aqua "Crimson"
  "The color « aqua » is (additively-)inverted « crimson ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup sboo-theme nil

  "My themes' configuration."

  :group 'theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sboo-default-theme 'sboo-high-contrast-sboo-inverted-colors

  "The theme which is enabled by default."

  :type  'symbol
  :safe  t
  :group 'sboo-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces: Haskell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-keyword-face

  '((t ( 
        )))

  "Custom face for `haskell-keyword-face'."
  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-operator-face

  `((t (:bold       t
        :foreground ,sboo-inverted-purple)
        ))
  "Custom face for `haskell-operator-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-type-face

  '((t ( 
        )))
  "Custom face for `haskell-type-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-constructor-face

  '((t ( 
        )))
  "Custom face for `haskell-constructor-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-definition-face

  '((t ( 
        )))
  "Custom face for `haskell-definition-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-literate-comment-face

  '((t ( 
        )))
  "Custom face for `haskell-literate-comment-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-pragma-face

  '((t ( 
        )))
  "Custom face for `haskell-pragma-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sboo-haskell-liquid-haskell-annotation-face

  '((t ( 
        )))
  "Custom face for `haskell-liquid-haskell-annotation-face'."

  :group 'sboo-haskell-appearence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-theme-set! (&optional THEME)

  "(Re)load `THEME'.

By default, `THEME' is `sboo-default-theme'."

  (interactive (list sboo-default-theme))

  (let ((THEME (or THEME sboo-default-theme))
        )

    (progn
      (load-theme   THEME :no-confirm)
      (enable-theme THEME)
      ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-theme-haskell-config! (&optional STYLE)

  "Configure `haskell-mode' faces.
"

  (interactive (list nil))

  ;;(setq haskell-operator-face 'sboo-haskell-operator-face)

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `haskell-mode' faces' defaults:
;; 
;; * haskell-keyword-face          -> font-lock-keyword-face
;; * haskell-type-face             -> font-lock-type-face
;; * haskell-constructor-face      -> font-lock-type-face
;; *
;; * haskell-definition-face       -> font-lock-function-name-face
;; * haskell-operator-face         -> font-lock-variable-name-face
;;
;; * ∅                             -> font-lock-doc-face
;; * haskell-pragma-face           -> font-lock-preprocessor-face
;; * haskell-literate-comment-face -> font-lock-doc-face
;; *
;; * haskell-default-face          -> nil
;; *
;; * haskell-error-face            -> error-face
;; * haskell-warning-face          -> warning-face
;; * haskell-hole-face             -> warning-face
;;

;; NOTE `font-lock-doc-face' is for Haddocks,
;;      `font-lock-comment-face' is for other comments.

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-theme)
