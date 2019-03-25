;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Anathema is a theme for « xrandr-invert-colors »
;;
;; i.e. colors that are normal-looking when inverted.
;;
;; • 
;; • 
;;
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Themes --------------------------------------;;
;;----------------------------------------------;;

(deftheme anathema "The Anathema color theme for « xrandr-invert-colors ».")

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

;; (require 'cl-lib)
;; (require 'pcase)
;; (require 'seq)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables: Customizable ---------------------;;
;;----------------------------------------------;;

(defgroup anathema-theme nil

  "Anathema theme."

  :group 'faces
  :prefix "anathema-theme-"

  :link '(url-link :tag "GitHub" "http://github.com/sboosali/anathema-theme")
  :tag "Anathema theme")

;;----------------------------------------------;;

;;;###autoload
(defcustom anathema-override-colors-alist

  '()

  "Override `anathema-theme's default colors.

Associates (variable) names with hex colors."

  :type '(alist :key-type   (string :tag "Name")
                :value-type (string :tag " Hex"))

  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-use-variable-pitch nil

  "Whether to display (some) headings and titles in a variable-pitch face.

Jargon:

• variable-pitch — proportional font.
• fixed-pitch    — monospaced font."

  :type '(choice (const nil :tag "   Fixed-Pitch")
                 (const t   :tag "Variable-Pitch"))
  ;; :type 'boolean
  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-height-minus-1 0.8

  "Font size « *-1 »."

  :type '(number :tag "Scaling Factor")
  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-height-plus-1 1.1

  "Font size « *+1 »."

  :type '(number :tag "Scaling Factor")
  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-height-plus-2 1.15

  "Font size « *+2 »."

  :type '(number :tag "Scaling Factor")
  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-height-plus-3 1.2

  "Font size « *+3 »."

  :type '(number :tag "Scaling Factor")
  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-height-plus-4 1.3

  "Font size « *+4 »."

  :type '(number :tag "Scaling Factor")
  :group 'anathema-theme)

;;----------------------------------------------;;

(defcustom anathema-scale-outline-headlines nil

  "Whether `outline-mode' headlines should be scaled."

  :type '(choice (const nil :tag "Don't Scale (✓)")
                 (const t   :tag "   Do Scale (❌)"))
  :group 'anathema-theme)

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(defvar anathema-default-colors-alist

  '(
    ("anathema-fg+1"     . "#FFFFEF")
    ("anathema-fg"       . "#DCDCCC")
    ("anathema-fg-1"     . "#656555")
    ("anathema-bg-2"     . "#000000")
    ("anathema-bg-1"     . "#2B2B2B")
    ("anathema-bg-05"    . "#383838")
    ("anathema-bg"       . "#3F3F3F")
    ("anathema-bg+05"    . "#494949")
    ("anathema-bg+1"     . "#4F4F4F")
    ("anathema-bg+2"     . "#5F5F5F")
    ("anathema-bg+3"     . "#6F6F6F")
    ("anathema-red+2"    . "#ECB3B3")
    ("anathema-red+1"    . "#DCA3A3")
    ("anathema-red"      . "#CC9393")
    ("anathema-red-1"    . "#BC8383")
    ("anathema-red-2"    . "#AC7373")
    ("anathema-red-3"    . "#9C6363")
    ("anathema-red-4"    . "#73acac")   ; #8C5353 #73acac
    ("anathema-red-5"    . "#7C4343")
    ("anathema-red-6"    . "#6C3333")
    ("anathema-orange"   . "#DFAF8F")
    ("anathema-yellow"   . "#F0DFAF")
    ("anathema-yellow-1" . "#E0CF9F")
    ("anathema-yellow-2" . "#D0BF8F")
    ("anathema-green-5"  . "#2F4F2F")
    ("anathema-green-4"  . "#3F5F3F")
    ("anathema-green-3"  . "#4F6F4F")
    ("anathema-green-2"  . "#5F7F5F")
    ("anathema-green-1"  . "#6F8F6F")
    ("anathema-green"    . "#7F9F7F")
    ("anathema-green+1"  . "#8FB28F")
    ("anathema-green+2"  . "#9FC59F")
    ("anathema-green+3"  . "#AFD8AF")
    ("anathema-green+4"  . "#BFEBBF")
    ("anathema-cyan"     . "#93E0E3")
    ("anathema-blue+3"   . "#BDE0F3")
    ("anathema-blue+2"   . "#ACE0E3")
    ("anathema-blue+1"   . "#94BFF3")
    ("anathema-blue"     . "#8CD0D3")
    ("anathema-blue-1"   . "#7CB8BB")
    ("anathema-blue-2"   . "#6CA0A3")
    ("anathema-blue-3"   . "#5C888B")
    ("anathema-blue-4"   . "#4C7073")
    ("anathema-blue-5"   . "#366060")
    ("anathema-magenta"  . "#DC8CC3")
    )

  "Color Palette (for the `anathema' theme).

Type (Haskell):

• « (String, Color) »

Form:

• « (NAME . HEX) »

Naming:

• `+N' suffix — indicate a color is lighter.
• `-N' suffix — indicate a color is darker.")

;;----------------------------------------------;;

(defmacro anathema-with-color-variables (&rest body)

  "`let' bind all colors defined in `anathema-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89)).

Links:

• URL `https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el'"

  (declare (indent 0))

  (let* ((CONS-TO-BINDING (lambda (cons)
                            (list (intern (car cons)) (cdr cons))))
         (COLORS-ALIST    (append anathema-default-colors-alist
                                  anathema-override-colors-alist))
         (COLOR-BINDINGS  (mapcar CONS-TO-BINDING
                                  COLORS-ALIST))                  
         )

    `(let ((class '((class      color)
                    (min-colors 89)
                    ))

           (z-variable-pitch (if anathema-use-variable-pitch
                                 'variable-pitch
                               'default))

           ,@COLOR-BINDINGS
           )

    ,@body)))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables: Theme ----------------------------;;
;;----------------------------------------------;;

(anathema-with-color-variables

 (custom-theme-set-variables

  'anathema

   ;; ansi-color

   `(ansi-color-names-vector [,anathema-bg ,anathema-red ,anathema-green ,anathema-yellow ,anathema-blue ,anathema-magenta ,anathema-cyan ,anathema-fg])))

;;----------------------------------------------;;
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

;; Internal (/ Built-in) Faces...

(anathema-with-color-variables

  (custom-theme-set-faces

   'anathema

   ;; basic coloring

   `(button                             ((t (:underline t))))
   `(link                               ((t (:foreground ,anathema-yellow :underline t :weight bold))))
   `(link-visited                       ((t (:foreground ,anathema-yellow-2 :underline t :weight normal))))
   `(default                            ((t (:foreground ,anathema-fg :background ,anathema-bg))))
   `(cursor                             ((t (:foreground ,anathema-fg :background ,anathema-fg+1))))
   `(widget-field                       ((t (:foreground ,anathema-fg :background ,anathema-bg+3))))
   `(escape-glyph                       ((t (:foreground ,anathema-yellow :weight bold))))
   `(fringe                             ((t (:foreground ,anathema-fg :background ,anathema-bg+1))))
   `(header-line                        ((t (:foreground ,anathema-yellow :background ,anathema-bg-1 :box  (:line-width -1 :style released-button)))))
   `(highlight                          ((t (:background ,anathema-bg-05))))
   `(success                            ((t (:foreground ,anathema-green :weight bold))))
   `(warning                            ((t (:foreground ,anathema-orange :weight bold))))
   `(tooltip                            ((t (:foreground ,anathema-fg :background ,anathema-bg+1))))

   ;; font lock

   `(font-lock-builtin-face               ((t (:foreground ,anathema-fg :weight bold))))
   `(font-lock-comment-face               ((t (:foreground ,anathema-green))))
   `(font-lock-comment-delimiter-face     ((t (:foreground ,anathema-green-2))))
   `(font-lock-constant-face              ((t (:foreground ,anathema-green+4))))
   `(font-lock-doc-face                   ((t (:foreground ,anathema-green+2))))
   `(font-lock-function-name-face         ((t (:foreground ,anathema-cyan))))
   `(font-lock-keyword-face               ((t (:foreground ,anathema-yellow :weight bold))))
   `(font-lock-negation-char-face         ((t (:foreground ,anathema-yellow :weight bold))))
   `(font-lock-preprocessor-face          ((t (:foreground ,anathema-blue+1))))
   `(font-lock-regexp-grouping-construct  ((t (:foreground ,anathema-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash  ((t (:foreground ,anathema-green :weight bold))))
   `(font-lock-string-face                ((t (:foreground ,anathema-red))))
   `(font-lock-type-face                  ((t (:foreground ,anathema-blue-1))))
   `(font-lock-variable-name-face         ((t (:foreground ,anathema-orange))))
   `(font-lock-warning-face               ((t (:foreground ,anathema-yellow-2 :weight bold))))

   ;; compilation

   `(compilation-column-face ((t (:foreground ,anathema-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,anathema-green))))
   `(compilation-error-face ((t (:foreground ,anathema-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,anathema-fg))))
   `(compilation-info-face ((t (:foreground ,anathema-blue))))
   `(compilation-info ((t (:foreground ,anathema-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,anathema-green))))
   `(compilation-line-face ((t (:foreground ,anathema-yellow))))
   `(compilation-line-number ((t (:foreground ,anathema-yellow))))
   `(compilation-message-face ((t (:foreground ,anathema-blue))))
   `(compilation-warning-face ((t (:foreground ,anathema-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,anathema-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,anathema-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,anathema-yellow :weight bold))))

   ;; completions

   `(completions-annotations ((t (:foreground ,anathema-fg-1))))

   ;; eww

   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))

   ;; grep

   `(grep-context-face ((t (:foreground ,anathema-fg))))
   `(grep-error-face ((t (:foreground ,anathema-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,anathema-blue))))
   `(grep-match-face ((t (:foreground ,anathema-orange :weight bold))))
   `(match ((t (:background ,anathema-bg-1 :foreground ,anathema-orange :weight bold))))

   ;; hi-lock

   `(hi-blue    ((t (:background ,anathema-cyan    :foreground ,anathema-bg-1))))
   `(hi-green   ((t (:background ,anathema-green+4 :foreground ,anathema-bg-1))))
   `(hi-pink    ((t (:background ,anathema-magenta :foreground ,anathema-bg-1))))
   `(hi-yellow  ((t (:background ,anathema-yellow  :foreground ,anathema-bg-1))))
   `(hi-blue-b  ((t (:foreground ,anathema-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,anathema-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,anathema-red     :weight     bold))))

   ;; info

   `(Info-quoted ((t (:inherit font-lock-constant-face))))

   ;; isearch

   `(isearch ((t (:foreground ,anathema-yellow-2 :weight bold :background ,anathema-bg+2))))
   `(isearch-fail ((t (:foreground ,anathema-fg :background ,anathema-red-4))))
   `(lazy-highlight ((t (:foreground ,anathema-yellow-2 :weight bold :background ,anathema-bg-05))))

   `(menu ((t (:foreground ,anathema-fg :background ,anathema-bg))))
   `(minibuffer-prompt ((t (:foreground ,anathema-yellow))))
   `(mode-line
     ((,class (:foreground ,anathema-green+1
                           :background ,anathema-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,anathema-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,anathema-green-2
                      :background ,anathema-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,anathema-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,anathema-bg+2))))
   `(trailing-whitespace ((t (:background ,anathema-red))))
   `(vertical-border ((t (:foreground ,anathema-fg))))

   ;; term

   `(term-color-black ((t (:foreground ,anathema-bg
                                       :background ,anathema-bg-1))))
   `(term-color-red ((t (:foreground ,anathema-red-2
                                     :background ,anathema-red-4))))
   `(term-color-green ((t (:foreground ,anathema-green
                                       :background ,anathema-green+2))))
   `(term-color-yellow ((t (:foreground ,anathema-orange
                                        :background ,anathema-yellow))))
   `(term-color-blue ((t (:foreground ,anathema-blue-1
                                      :background ,anathema-blue-4))))
   `(term-color-magenta ((t (:foreground ,anathema-magenta
                                         :background ,anathema-red))))
   `(term-color-cyan ((t (:foreground ,anathema-cyan
                                      :background ,anathema-blue))))
   `(term-color-white ((t (:foreground ,anathema-fg
                                       :background ,anathema-fg-1))))

   ;; flymake

   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,anathema-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,anathema-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,anathema-green-2 :weight bold :underline t))))

   ))

;;----------------------------------------------;;

;; External (/ Installed) Faces...

(anathema-with-color-variables

  (custom-theme-set-faces

   'anathema

   ;; helm

   `(helm-header
     ((t (:foreground ,anathema-green
                      :background ,anathema-bg
                      :underline nil
                      :box nil))))

   `(helm-source-header
     ((t (:foreground ,anathema-yellow
                      :background ,anathema-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))

   `(helm-selection ((t (:background ,anathema-bg+1 :underline nil))))

   `(helm-selection-line ((t (:background ,anathema-bg+1))))

   `(helm-visible-mark ((t (:foreground ,anathema-bg :background ,anathema-yellow-2))))

   `(helm-candidate-number ((t (:foreground ,anathema-green+4 :background ,anathema-bg-1))))
   `(helm-separator ((t (:foreground ,anathema-red :background ,anathema-bg))))
   `(helm-time-zone-current ((t (:foreground ,anathema-green+2 :background ,anathema-bg))))
   `(helm-time-zone-home ((t (:foreground ,anathema-red :background ,anathema-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,anathema-orange :background ,anathema-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,anathema-magenta :background ,anathema-bg))))
   `(helm-bookmark-info ((t (:foreground ,anathema-green+2 :background ,anathema-bg))))
   `(helm-bookmark-man ((t (:foreground ,anathema-yellow :background ,anathema-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,anathema-magenta :background ,anathema-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,anathema-red :background ,anathema-bg))))
   `(helm-buffer-process ((t (:foreground ,anathema-cyan :background ,anathema-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,anathema-fg :background ,anathema-bg))))
   `(helm-buffer-size ((t (:foreground ,anathema-fg-1 :background ,anathema-bg))))
   `(helm-ff-directory ((t (:foreground ,anathema-cyan :background ,anathema-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,anathema-fg :background ,anathema-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,anathema-green+2 :background ,anathema-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,anathema-red :background ,anathema-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,anathema-yellow :background ,anathema-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,anathema-bg :background ,anathema-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,anathema-cyan :background ,anathema-bg))))
   `(helm-grep-file ((t (:foreground ,anathema-fg :background ,anathema-bg))))
   `(helm-grep-finish ((t (:foreground ,anathema-green+2 :background ,anathema-bg))))
   `(helm-grep-lineno ((t (:foreground ,anathema-fg-1 :background ,anathema-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,anathema-red :background ,anathema-bg))))
   `(helm-match ((t (:foreground ,anathema-orange :background ,anathema-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,anathema-cyan :background ,anathema-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,anathema-fg-1 :background ,anathema-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,anathema-fg :background ,anathema-bg))))

   ;; helm-lxc

   `(helm-lxc-face-frozen ((t (:foreground ,anathema-blue :background ,anathema-bg))))
   `(helm-lxc-face-running ((t (:foreground ,anathema-green :background ,anathema-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,anathema-red :background ,anathema-bg))))

   ;; helm-swoop

   `(helm-swoop-target-line-face ((t (:foreground ,anathema-fg :background ,anathema-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,anathema-yellow :background ,anathema-bg+2 :weight bold))))

   ;; git-gutter

   `(git-gutter:added ((t (:foreground ,anathema-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,anathema-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,anathema-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,anathema-fg :weight bold :inverse-video t))))

   ;; flycheck

   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-red-1) :inherit unspecified))
      (t (:foreground ,anathema-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-yellow) :inherit unspecified))
      (t (:foreground ,anathema-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-cyan) :inherit unspecified))
      (t (:foreground ,anathema-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,anathema-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,anathema-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,anathema-cyan :weight bold))))

   ;; flyspell

   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-orange) :inherit unspecified))
      (t (:foreground ,anathema-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,anathema-red) :inherit unspecified))
      (t (:foreground ,anathema-red-1 :weight bold :underline t))))

   ))

;;----------------------------------------------;;
;; Autoloads -----------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(ignore-errors
  (when (and load-file-name
             (boundp 'custom-theme-load-path)
             )
    (let ((LOAD-DIRECTORY-NAME (file-name-as-directory (file-name-directory load-file-name)))
          )
      (add-to-list 'custom-theme-load-path LOAD-DIRECTORY-NAME))))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;==============================================;;
(provide 'anathema-theme)