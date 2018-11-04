;;; sboo-inverted-candle-theme.el --- (Inverted) "Candle" Colors (red, orange, yellow, black).

;; Copyright (C) 2005, 2006  Xavier Maillard <zedek@gnu.org>
;; Copyright (C) 2005, 2006  Brian Palmer <bpalmer@gmail.com>
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2018 by Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; URL: https://raw.githubusercontent.com/sboosali/.emacs.d/master/sboo/themes/sboo-black-red-monochrome-inverted-colors-theme.el
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; A High-Contrast Theme that's pleasant and sensible
;; under Inverted-Colors (e.g. from `xrandr-invert-colors').
;;
;; "Pleasant" meaning that colors look good when inverted
;; (if you're viewing screenshots without inverted-colors,
;; it may look too garish 80s, or even too bland). 
;;
;; "Intuitive" meaning that, for exampple, errors are red
;; (not blue or green, as when a light-color theme with red errors
;; is inverted (i.e. seen from a screen whose colors are inverted).

;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Red Inversions:
;;
;; - Cyan inverts-to Red
;; - DeepSkyBlue(?) inverts-to Orange
;; - Blue inverts-to Yellow
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 

;; Secondary (Additive) Colors
;;
;; "A secondary color is formed by the sum of two primary colors of equal intensity: cyan is green+blue, magenta is red+blue, and yellow is red+green."

;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font Lock
;;
;; For exmaple, in the ELisp expression « (require 'sboo-theme nil :no-error) »:
;;
;; * `font-lock-keyword-face'  — colors symbol literals         (i.e. « require »).
;; * `font-lock-builtin-face'  — colors keyword-symbol literals (i.e. « :noerror »). 
;; * `font-lock-constant-face' — colors symbol literals         (i.e. « sboo-theme »).
;;

;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Development
;;
;; M-x `list-faces-display'
;; M-x `list-colors-display'
;;
;; « C-h f custom-theme-set-faces »:
;;
;; (custom-theme-set-faces THEME &rest ARGS)
;;
;; ARGS is a list, where each entry is a list of
;;
;; « (FACE SPEC [NOW [COMMENT]]) »
;;
;; 

;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Links:
;;
;; - https://en.wikipedia.org/wiki/Web_colors
;; - 
;;

;;:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Primary Colors: RGB

(defvar inverted-red "Cyan"
  "The color « red » is (additively-)inverted « cyan ».")

(defvar inverted-blue "Orange"
  "The color « blue » is (additively-)inverted « orange ».")

(defvar inverted-green "Magenta"
  "The color « green » is (additively-)inverted « magenta ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dark (Black-ish) Colors

(defvar inverted-black "White"
  "The color « black » is inverted « white ».")

(defvar inverted-darkgreen "MistyRose"
  "The color « darkgreen » is (additively-)inverted « MistyRose ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Red Colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Low-Blue (Red-ish) Colors

(defvar inverted-crimson "Aquamarine"
  "The color « crimson » is (additively-)inverted « Aquamarine ».")

(defvar inverted-yellow "Blue"
  "The color « yellow » is (additively-)inverted « blue ».")

(defvar inverted-orange "DeepSkyBlue"
  "The color « azure » (~ deep blue) is (additively-)inverted « orange ».")

(defvar inverted-sand "MidnightBlue"
  "The color « sand » (pale yellow/brown) is (additively-)inverted « Midnight Blue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Light (White-ish) Colors

(defvar inverted-lightyellow "Navy"
  "The color « light Yellow » is inverted « Navy (blue) ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheme sboo-inverted-candle
  "Black-Red Quasi-Monochrome (Oligochrome?) theme for Inverted-Colors.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-theme-set-faces

 'sboo-inverted-candle

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(default                                    ((t (:background ,inverted-black :foreground "Navy")))) ;; ,inverted-lightyellow

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Highlights

 `(region                                     ((t (:background ,inverted-crimson   :foreground "Crimson" :bold 1 :underline nil :overline nil))))
 `(secondary-selection                        ((t (:background ,inverted-white :foreground ,inverted-black :bold 1))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Fonts

 `(bold                                       ((t (:bold t))))
 `(italic                                     ((t (        :italic t))))
 `(underline                                  ((t (:bold t           :underline t))))
 `(bold-italic                                ((t (:bold t :italic t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Mouse

 `(mouse                                      ((t (:foreground ,inverted-white))))
 `(cursor                                     ((t (:background ,inverted-red))))
 `(vcursor                                    ((t (:foreground ,inverted-red :background ,inverted-red :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Haskell Mode
 ;;
 ;; NOTE Faces which `:inherit` from others must come BEFORE them.
 ;;      In particular, `haskell-*-face`s must precede `font-lock-*-face`s.
 ;;

 `(haskell-operator-face                    ((t (:foreground ,inverted-orange :bold t))))
 `(haskell-type-face                        ((t (:foreground ,inverted-yellow))))
 `(haskell-constructor-face                 ((t (:foreground ,inverted-yellow))))
 `(haskell-keyword-face                     ((t (:foreground ,inverted-orange :bold t))))
 `(haskell-definition-face                  ((t (:foreground ,inverted-orange :bold nil :underline t :overline t))))

 ;; `(haskell-quasi-quote-face                 ((t (:foreground ""))))
 ;; `(haskell-hole-face                        ((t (:foreground ""))))

 ;; `(haskell-literate-comment-face            ((t (:foreground ""))))
 `(haskell-pragma-face                      ((t (:foreground ,inverted-orange))))
 `(haskell-liquid-haskell-annotation-face   ((t (:foreground ,inverted-orange))))

 ;; `(haskell-error-face                       ((t (:foreground ""))))
 ;; `(haskell-warning-face                     ((t (:foreground ""))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; For Programs

 `(font-lock-keyword-face                     ((t (:bold t              :foreground ,inverted-yellow))))
 `(font-lock-builtin-face                     ((t (:bold t              :foreground ,inverted-yellow))))
 `(font-lock-constant-face                    ((t (:bold t :underline t :foreground ,inverted-yellow))))
 `(font-lock-string-face                      ((t (:bold t              :foreground ,inverted-orange :underline t :italic nil))))

 `(font-lock-type-face                        ((t (:bold t              :foreground ,inverted-red))))
 `(font-lock-function-name-face               ((t (:bold t              :foreground ,inverted-red))))
 `(font-lock-variable-name-face               ((t (:bold t              :foreground ,inverted-red))))

 `(font-lock-comment-face                     ((t (                     :foreground ,inverted-red))))
 `(font-lock-doc-face                         ((t (                     :foreground ,inverted-red :background ,inverted-darkgreen))))
 `(font-lock-preprocessor-face                ((t (:bold t              :foreground ,inverted-orange))))

;;TODO `(font-lock-error-face                       ((t (:bold t              :foreground ,inverted-orange))))
 `(font-lock-warning-face                     ((t (:bold t              :foreground ,inverted-orange))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Borders

 `(border                                     ((t (:foreground ,inverted-white))))
 `(fringe                                     ((t (:background ,inverted-red))))

 `(modeline                                   ((t (:background ,inverted-white :foreground ,inverted-black :bold 1))))
 `(modeline-buffer-id                         ((t (:background ,inverted-white :foreground ,inverted-black :bold 1))))
 `(modeline-mousable                          ((t (:background ,inverted-white :foreground ,inverted-black :bold 1))))
 `(modeline-mousable-minor-mode               ((t (:background ,inverted-white :foreground ,inverted-black :bold 1))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Parenthesis Matching

 `(show-paren-match-face                      ((t (:background ,inverted-red))))
 `(show-paren-mismatch-face                   ((t (:foreground ,inverted-black :background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Widgets

 `(widget-button-face                         ((t (:bold t))))
 `(widget-button-pressed-face                 ((t (:foreground ,inverted-red))))
 `(widget-documentation-face                  ((t (:foreground ,inverted-red))))
 `(widget-field-face                          ((t (:background ,inverted-red))))
 `(widget-inactive-face                       ((t (:foreground ,inverted-red))))
 `(widget-single-line-field-face              ((t (:background ,inverted-red)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Miscellaneous (Frequent)

 `(excerpt                                    ((t (:italic t))))
 `(fixed                                      ((t (:bold t))))
 `(help-highlight-face                        ((t (:underline t))))
 `(list-matching-lines-face                   ((t (:bold t))))
 `(view-highlight-face                        ((t (:background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Makefile Mode

 `(makefile-space-face                        ((t (:background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Miscellaneous Modes

 `(Man-overstrike-face                        ((t (:bold t))))
 `(Man-underline-face                         ((t (:underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(apropos-keybinding-face                    ((t (:underline t))))
 `(apropos-label-face                         ((t (:italic t))))
 `(apropos-match-face                         ((t (:background ,inverted-red))))
 `(apropos-property-face                      ((t (:bold t :italic t))))
 `(apropos-symbol-face                        ((t (:bold t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(goto-address-mail-face                     ((t (:italic t))))
 `(goto-address-mail-mouse-face               ((t (:background ,inverted-red))))
 `(goto-address-url-face                      ((t (:bold t))))
 `(goto-address-url-mouse-face                ((t (:background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(calendar-today-face                        ((t (:underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(cperl-array-face                           ((t (:foreground ,inverted-red :background ,inverted-red :bold t))))
 `(cperl-hash-face                            ((t (:foreground ,inverted-red  :background ,inverted-red :bold t :italic t))))
 `(cperl-nonoverridable-face                  ((t (:foreground ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(custom-button-face                         ((t (nil))))
 `(custom-changed-face                        ((t (:foreground ,inverted-black  :background ,inverted-red))))
 `(custom-documentation-face                  ((t (nil))))
 `(custom-face-tag-face                       ((t (                                             :underline t))))
 `(custom-group-tag-face                      ((t (:foreground ,inverted-red                           :underline t))))
 `(custom-group-tag-face-1                    ((t (:foreground ,inverted-red                            :underline t))))
 `(custom-invalid-face                        ((t (:foreground ,inverted-red :background ,inverted-red))))
 `(custom-modified-face                       ((t (:foreground ,inverted-black  :background ,inverted-red))))
 `(custom-rogue-face                          ((t (:foreground ,inverted-red   :background ,inverted-white))))
 `(custom-saved-face                          ((t (                                             :underline t))))
 `(custom-set-face                            ((t (:foreground ,inverted-red   :background ,inverted-black))))
 `(custom-state-face                          ((t (:foreground ,inverted-red))))
 `(custom-variable-button-face                ((t (                                     :bold t :underline t))))
 `(custom-variable-tag-face                   ((t (:foreground ,inverted-red                           :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(diary-face                                 ((t (:foreground ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(ediff-current-diff-face-A                  ((t (:foreground ,inverted-red :background ,inverted-red))))
 `(ediff-current-diff-face-Ancestor           ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-current-diff-face-B                  ((t (:foreground ,inverted-red :background ,inverted-red))))
 `(ediff-current-diff-face-C                  ((t (:foreground ,inverted-red :background ,inverted-red))))
 `(ediff-even-diff-face-A                     ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-even-diff-face-Ancestor              ((t (:foreground ,inverted-black :background ,inverted-red))))
 `(ediff-even-diff-face-B                     ((t (:foreground ,inverted-black :background ,inverted-red))))
 `(ediff-even-diff-face-C                     ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-fine-diff-face-A                     ((t (:foreground ,inverted-red :background ,inverted-red))))
 `(ediff-fine-diff-face-Ancestor              ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-fine-diff-face-B                     ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-fine-diff-face-C                     ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-odd-diff-face-A                      ((t (:foreground ,inverted-black :background ,inverted-red))))
 `(ediff-odd-diff-face-Ancestor               ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-odd-diff-face-B                      ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(ediff-odd-diff-face-C                      ((t (:foreground ,inverted-black :background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(eshell-ls-archive-face                     ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-backup-face                      ((t (:foreground ,inverted-red))))
 `(eshell-ls-clutter-face                     ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-directory-face                   ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-executable-face                  ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-missing-face                     ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-product-face                     ((t (:foreground ,inverted-red))))
 `(eshell-ls-readonly-face                    ((t (:foreground ,inverted-red))))
 `(eshell-ls-special-face                     ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-symlink-face                     ((t (:foreground ,inverted-red :bold t))))
 `(eshell-ls-unreadable-face                  ((t (:foreground ,inverted-red))))
 `(eshell-prompt-face                         ((t (:foreground ,inverted-red :bold t))))
 `(eshell-test-failed-face                    ((t (:foreground ,inverted-red :bold t))))
 `(eshell-test-ok-face                        ((t (:foreground ,inverted-red :bold t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(flyspell-duplicate-face                    ((t (:foreground ,inverted-red :bold t :underline t))))
 `(flyspell-incorrect-face                    ((t (:foreground ,inverted-red :bold t :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(gnus-cite-attribution-face                 ((t (:italic t))))
 `(gnus-cite-face-1                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-10                          ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-11                          ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-2                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-3                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-4                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-5                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-6                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-7                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-8                           ((t (:foreground ,inverted-red))))
 `(gnus-cite-face-9                           ((t (:foreground ,inverted-red))))
 `(gnus-emphasis-bold                         ((t (:bold t))))
 `(gnus-emphasis-bold-italic                  ((t (:bold t :italic t))))
 `(gnus-emphasis-italic                       ((t (:italic t))))
 `(gnus-emphasis-underline                    ((t (:underline t))))
 `(gnus-emphasis-underline-bold               ((t (:bold t :underline t))))
 `(gnus-emphasis-underline-bold-italic        ((t (:bold t :italic t :underline t))))
 `(gnus-emphasis-underline-italic             ((t (:italic t :underline t))))
 `(gnus-group-mail-1-empty-face               ((t (:foreground ,inverted-red))))
 `(gnus-group-mail-1-face                     ((t (:foreground ,inverted-red :bold t))))
 `(gnus-group-mail-2-empty-face               ((t (:foreground ,inverted-red))))
 `(gnus-group-mail-2-face                     ((t (:foreground ,inverted-red :bold t))))
 `(gnus-group-mail-3-empty-face               ((t (:foreground ,inverted-red))))
 `(gnus-group-mail-3-face                     ((t (:foreground ,inverted-red :bold t))))
 `(gnus-group-mail-low-empty-face             ((t (:foreground ,inverted-red))))
 `(gnus-group-mail-low-face                   ((t (:foreground ,inverted-red :bold t))))
 `(gnus-group-news-1-empty-face               ((t (:foreground ,inverted-red))))
 `(gnus-group-news-1-face                     ((t (:foreground ,inverted-red :bold t))))
 `(gnus-group-news-2-empty-face               ((t (:foreground ,inverted-red))))
 `(gnus-group-news-2-face                     ((t (:foreground ,inverted-red :bold t))))
 `(gnus-group-news-3-empty-face               ((t (nil))))
 `(gnus-group-news-3-face                     ((t (:bold t))))
 `(gnus-group-news-low-empty-face             ((t (:foreground ,inverted-red))))
 `(gnus-group-news-low-face                   ((t (:foreground ,inverted-red :bold t))))
 `(gnus-header-content-face                   ((t (:foreground ,inverted-red :italic t))))
 `(gnus-header-from-face                      ((t (:foreground ,inverted-red))))
 `(gnus-header-name-face                      ((t (:foreground ,inverted-red))))
 `(gnus-header-newsgroups-face                ((t (:foreground ,inverted-red :italic t))))
 `(gnus-header-subject-face                   ((t (:foreground ,inverted-red))))
 `(gnus-signature-face                        ((t (:italic t))))
 `(gnus-splash-face                           ((t (:foreground ,inverted-red))))
 `(gnus-summary-cancelled-face                ((t (:foreground ,inverted-red :background ,inverted-white))))
 `(gnus-summary-high-ancient-face             ((t (:foreground ,inverted-red :bold t))))
 `(gnus-summary-high-read-face                ((t (:foreground ,inverted-red :bold t))))
 `(gnus-summary-high-ticked-face              ((t (:foreground ,inverted-red :bold t))))
 `(gnus-summary-high-unread-face              ((t (:bold t))))
 `(gnus-summary-low-ancient-face              ((t (:foreground ,inverted-red :italic t))))
 `(gnus-summary-low-read-face                 ((t (:foreground ,inverted-red :italic t))))
 `(gnus-summary-low-ticked-face               ((t (:foreground ,inverted-red :italic t))))
 `(gnus-summary-low-unread-face               ((t (:italic t))))
 `(gnus-summary-normal-ancient-face           ((t (:foreground ,inverted-red))))
 `(gnus-summary-normal-read-face              ((t (:foreground ,inverted-red))))
 `(gnus-summary-normal-ticked-face            ((t (:foreground ,inverted-red))))
 `(gnus-summary-normal-unread-face            ((t (nil))))
 `(gnus-summary-selected-face                 ((t (:underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(highlight                                  ((t (:background ,inverted-white :foreground ,inverted-black :bold 1))))
 `(highlight-changes-delete-face              ((t (:foreground ,inverted-red :underline t))))
 `(highlight-changes-face                     ((t (:foreground ,inverted-red))))
 `(highline-face                              ((t (:background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(holiday-face                               ((t (:background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(info-menu-5                                ((t (:underline t :bold t))))
 `(info-node                                  ((t (:bold t))))
 `(info-xref                                  ((t (:bold t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(message-cited-text-face                    ((t (:foreground ,inverted-red))))
 `(message-header-cc-face                     ((t (:foreground ,inverted-red))))
 `(message-header-name-face                   ((t (:foreground ,inverted-red))))
 `(message-header-newsgroups-face             ((t (:foreground ,inverted-red :bold t :italic t))))
 `(message-header-other-face                  ((t (:foreground ,inverted-red))))
 `(message-header-subject-face                ((t (:foreground ,inverted-red :bold t))))
 `(message-header-to-face                     ((t (:foreground ,inverted-red :bold t))))
 `(message-header-xheader-face                ((t (:foreground ,inverted-red))))
 `(message-separator-face                     ((t (:foreground ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(speedbar-button-face                       ((t (:foreground ,inverted-red))))
 `(speedbar-directory-face                    ((t (:foreground ,inverted-red))))
 `(speedbar-file-face                         ((t (:foreground ,inverted-red))))
 `(speedbar-highlight-face                    ((t (:background ,inverted-red))))
 `(speedbar-selected-face                     ((t (:foreground ,inverted-red :underline t))))
 `(speedbar-tag-face                          ((t (:foreground ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(term-black                                 ((t (:foreground ,inverted-white))))
 `(term-blackbg                               ((t (:background ,inverted-white))))
 `(term-blue                                  ((t (:foreground ,inverted-red))))
 `(term-bluebg                                ((t (:background ,inverted-red))))
 `(term-bold                                  ((t (:bold t))))
 `(term-cyan                                  ((t (:foreground ,inverted-red))))
 `(term-cyanbg                                ((t (:background ,inverted-red))))
 `(term-default-bg                            ((t (nil))))
 `(term-default-bg-inv                        ((t (nil))))
 `(term-default-fg                            ((t (nil))))
 `(term-default-fg-inv                        ((t (nil))))
 `(term-green                                 ((t (:foreground ,inverted-red))))
 `(term-greenbg                               ((t (:background ,inverted-red))))
 `(term-invisible                             ((t (nil))))
 `(term-invisible-inv                         ((t (nil))))
 `(term-magenta                               ((t (:foreground ,inverted-red))))
 `(term-magentabg                             ((t (:background ,inverted-red))))
 `(term-red                                   ((t (:foreground ,inverted-red))))
 `(term-redbg                                 ((t (:background ,inverted-red))))
 `(term-underline                             ((t (:underline t))))
 `(term-white                                 ((t (:foreground ,inverted-black))))
 `(term-whitebg                               ((t (:background ,inverted-black))))
 `(term-yellow                                ((t (:foreground ,inverted-red))))
 `(term-yellowbg                              ((t (:background ,inverted-red))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(vhdl-font-lock-attribute-face              ((t (:foreground ,inverted-red))))
 `(vhdl-font-lock-directive-face              ((t (:foreground ,inverted-red))))
 `(vhdl-font-lock-enumvalue-face              ((t (:foreground ,inverted-red))))
 `(vhdl-font-lock-function-face               ((t (:foreground ,inverted-red))))
 `(vhdl-font-lock-prompt-face                 ((t (:foreground ,inverted-red :bold t))))
 `(vhdl-font-lock-reserved-words-face         ((t (:foreground ,inverted-red :bold t))))
 `(vhdl-font-lock-translate-off-face          ((t (:background ,inverted-red))))
 `(vhdl-speedbar-architecture-face            ((t (:foreground ,inverted-red))))
 `(vhdl-speedbar-architecture-selected-face   ((t (:foreground ,inverted-red :underline t))))
 `(vhdl-speedbar-configuration-face           ((t (:foreground ,inverted-red))))
 `(vhdl-speedbar-configuration-selected-face  ((t (:foreground ,inverted-red :underline t))))
 `(vhdl-speedbar-entity-face                  ((t (:foreground ,inverted-red))))
 `(vhdl-speedbar-entity-selected-face         ((t (:foreground ,inverted-red :underline t))))
 `(vhdl-speedbar-instantiation-face           ((t (:foreground ,inverted-red))))
 `(vhdl-speedbar-instantiation-selected-face  ((t (:foreground ,inverted-red :underline t))))
 `(vhdl-speedbar-package-face                 ((t (:foreground ,inverted-red))))
 `(vhdl-speedbar-package-selected-face        ((t (:foreground ,inverted-red :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 `(viper-minibuffer-emacs-face                ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(viper-minibuffer-insert-face               ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(viper-minibuffer-vi-face                   ((t (:foreground ,inverted-red   :background ,inverted-red))))
 `(viper-replace-overlay-face                 ((t (:foreground ,inverted-white :background ,inverted-red))))
 `(viper-search-face                          ((t (:foreground ,inverted-white :background ,inverted-red))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-theme 'sboo-inverted-candle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sboo-inverted-candle-theme.el ends here
