;;; sboo-high-contrast-inverted-colors-theme.el --- HighContrast InvertedColors.

;; Copyright (C) 2005, 2006  Xavier Maillard <zedek@gnu.org>
;; Copyright (C) 2005, 2006  Brian Palmer <bpalmer@gmail.com>
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2018 by Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; URL: https://raw.githubusercontent.com/sboosali/.emacs.d/master/sboo/themes/sboo-high-contrast-inverted-colors-theme.el
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

;; Color Inversions:
;;
;; - Blue inverts-to Yellow
;; - Purple inverts-to NeonGreen
;; -  inverts-to 
;; -  inverts-to 
;; - Firebrick inverts-to Teal
;; - DarkGreen inverts-to Fuschia
;; - Darkgoldenrod inverts-to Azure
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 
;; -  inverts-to 

;; Secondary (Additive) Colors
;;
;; "A secondary color is formed by the sum of two primary colors of equal intensity: cyan is green+blue, magenta is red+blue, and yellow is red+green."

;; Font Lock
;;
;; For exmaple, in the ELisp expression « (require 'sboo-theme nil :no-error) »:
;;
;; * `font-lock-keyword-face'  — colors symbol literals         (i.e. « require »).
;; * `font-lock-builtin-face'  — colors keyword-symbol literals (i.e. « :noerror »). 
;; * `font-lock-constant-face' — colors symbol literals         (i.e. « sboo-theme »).
;;

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

;; Links:
;;
;; - https://en.wikipedia.org/wiki/Web_colors
;; - 
;;

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

;; Secondary Colors

(defvar inverted-purple "Chartreuse"
  "The color « purple » is (additively-)inverted « green+yellow ».")
;; ^ GreenYellow, Chartreuse, Chocolate.

(defvar inverted-yellow "Blue"
  "The color « yellow » is (additively-)inverted « blue ».")

(defvar inverted-orange "DeepSkyBlue"
  "The color « orange » is (additively-)inverted « DeepSkyBlue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hot Colors (i.e. Neon, that "pop" against a dark background)

(defvar inverted-hot-pink "Lime"
  "The color « hot pink » is (additively-)inverted « lime ».")

(defvar inverted-hot-green "Fuchsia"
  "The color « hot green » is (additively-)inverted « Fuchsia ».")

(defvar inverted-hot-orange "DeepSkyBlue"
  "The color « hot orange » is (additively-)inverted « DeepSkyBlue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Soft Colors

(defvar inverted-salmon "Teal"
  "The color « salmon » is (additively-)inverted « teal ».")

(defvar inverted-soft-purple "Olive"
  "The color « soft purple » is (additively-)inverted « olive ».")

(defvar inverted-olive "DarkSlateBlue"
  "The color « olive » is (additively-)inverted « Dark Slate Blue ».")

(defvar inverted-aqua "Crimson"
  "The color « aqua » is (additively-)inverted « crimson ».")

(defvar inverted-azure "Chocolate"
  "The color « azure » (~ deep blue) is (additively-)inverted « chocolate ».")

(defvar inverted-sand "MidnightBlue"
  "The color « sand » (pale yellow/brown) is (additively-)inverted « Midnight Blue ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dark Colors

(defvar inverted-dark-purple "LightGreen"
  "The color « dark Purple » is (additively-)inverted « light Green ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Light Colors

(defvar inverted-light-green "DarkMagenta"
  "The color « light Green » is (additively-)inverted « dark Magenta ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheme sboo-high-contrast-inverted-colors
  "High-Contrast theme for Inverted-Colors.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-theme-set-faces

 'sboo-high-contrast-inverted-colors

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(default                                    ((t (:background "white" :foreground "black"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Highlights

 '(region                                     ((t (:background "black" :foreground "white" :bold 1))))
 '(secondary-selection                        ((t (:background "black" :foreground "white" :bold 1))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Fonts

 '(bold                                       ((t (:bold t))))
 '(italic                                     ((t (        :italic t))))
 '(underline                                  ((t (:bold t           :underline t))))
 '(bold-italic                                ((t (:bold t :italic t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Mouse

 '(mouse                                      ((t (:foreground "black"))))
 '(cursor                                     ((t (:background "blue"))))
 '(vcursor                                    ((t (:foreground "red" :background "orange" :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Haskell Mode
 ;;
 ;; NOTE Faces which `:inherit' from others must come BEFORE them.
 ;;      In particular, `haskell-*-face's must precede `font-lock-*-face's.
 ;;

 `(haskell-operator-face                    ((t (:foreground ,inverted-azure :bold t))))
 `(haskell-type-face                        ((t (:foreground ,inverted-yellow))))
 `(haskell-constructor-face                 ((t (:foreground ,inverted-olive))))
 `(haskell-keyword-face                     ((t (:foreground ,inverted-dark-purple))))
 `(haskell-definition-face                  ((t (:foreground ,inverted-aqua :underline nil))))

 ;; `(haskell-quasi-quote-face                 ((t (:foreground ""))))
 ;; `(haskell-hole-face                        ((t (:foreground ""))))

 ;; `(haskell-literate-comment-face            ((t (:foreground ""))))
 `(haskell-pragma-face                      ((t (:foreground ,inverted-salmon))))
 `(haskell-liquid-haskell-annotation-face   ((t (:foreground ,inverted-salmon))))

 ;; `(haskell-error-face                       ((t (:foreground ""))))
 ;; `(haskell-warning-face                     ((t (:foreground ""))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; For Programs

 `(font-lock-keyword-face                     ((t (:bold t              :foreground ,inverted-dark-purple))))
 `(font-lock-builtin-face                     ((t (:bold t              :foreground ,inverted-yellow))))
 `(font-lock-constant-face                    ((t (:bold t :underline t :foreground ,inverted-olive))))
 `(font-lock-string-face                      ((t (:bold t              :foreground ,inverted-salmon :italic t))))

 `(font-lock-type-face                        ((t (:bold t              :foreground ,inverted-yellow))))
 `(font-lock-function-name-face               ((t (:bold t              :foreground ,inverted-yellow))))
 `(font-lock-variable-name-face               ((t (:bold t              :foreground ,inverted-olive))))

 `(font-lock-comment-face                     ((t (                     :foreground ,inverted-red))))
 `(font-lock-doc-face                         ((t (                     :foreground ,inverted-red :background "MistyRose"))))
 `(font-lock-preprocessor-face                ((t (:bold t              :foreground ,inverted-olive))))

;;TODO `(font-lock-error-face                       ((t (:bold t              :foreground ,inverted-orange))))
 `(font-lock-warning-face                     ((t (:bold t              :foreground ,inverted-orange))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Borders

 '(border                                     ((t (:foreground "black"))))
 '(fringe                                     ((t (:background "grey95"))))

 '(modeline                                   ((t (:background "black" :foreground "white" :bold 1))))
 '(modeline-buffer-id                         ((t (:background "black" :foreground "white" :bold 1))))
 '(modeline-mousable                          ((t (:background "black" :foreground "white" :bold 1))))
 '(modeline-mousable-minor-mode               ((t (:background "black" :foreground "white" :bold 1))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Parenthesis Matching

 '(show-paren-match-face                      ((t (:background "purple"))))
 '(show-paren-mismatch-face                   ((t (:foreground "white" :background "turquoise"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Widgets

 '(widget-button-face                         ((t (:bold t))))
 '(widget-button-pressed-face                 ((t (:foreground "red"))))
 '(widget-documentation-face                  ((t (:foreground "dark green"))))
 '(widget-field-face                          ((t (:background "gray85"))))
 '(widget-inactive-face                       ((t (:foreground "dim gray"))))
 '(widget-single-line-field-face              ((t (:background "gray85")))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; Miscellaneous (Frequent)

 '(excerpt                                    ((t (:italic t))))
 '(fixed                                      ((t (:bold t))))
 '(help-highlight-face                        ((t (:underline t))))
 '(list-matching-lines-face                   ((t (:bold t))))
 '(view-highlight-face                        ((t (:background "darkseagreen2"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Makefile Mode

 '(makefile-space-face                        ((t (:background "hotpink"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Miscellaneous Modes

 '(Man-overstrike-face                        ((t (:bold t))))
 '(Man-underline-face                         ((t (:underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(apropos-keybinding-face                    ((t (:underline t))))
 '(apropos-label-face                         ((t (:italic t))))
 '(apropos-match-face                         ((t (:background "paleturquoise"))))
 '(apropos-property-face                      ((t (:bold t :italic t))))
 '(apropos-symbol-face                        ((t (:bold t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(goto-address-mail-face                     ((t (:italic t))))
 '(goto-address-mail-mouse-face               ((t (:background "paleturquoise"))))
 '(goto-address-url-face                      ((t (:bold t))))
 '(goto-address-url-mouse-face                ((t (:background "darkseagreen2"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(calendar-today-face                        ((t (:underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(cperl-array-face                           ((t (:foreground "Blue" :background "lightyellow2" :bold t))))
 '(cperl-hash-face                            ((t (:foreground "Red"  :background "lightyellow2" :bold t :italic t))))
 '(cperl-nonoverridable-face                  ((t (:foreground "chartreuse3"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(custom-button-face                         ((t (nil))))
 '(custom-changed-face                        ((t (:foreground "white"  :background "blue"))))
 '(custom-documentation-face                  ((t (nil))))
 '(custom-face-tag-face                       ((t (                                             :underline t))))
 '(custom-group-tag-face                      ((t (:foreground "blue"                           :underline t))))
 '(custom-group-tag-face-1                    ((t (:foreground "red"                            :underline t))))
 '(custom-invalid-face                        ((t (:foreground "yellow" :background "red"))))
 '(custom-modified-face                       ((t (:foreground "white"  :background "blue"))))
 '(custom-rogue-face                          ((t (:foreground "pink"   :background "black"))))
 '(custom-saved-face                          ((t (                                             :underline t))))
 '(custom-set-face                            ((t (:foreground "blue"   :background "white"))))
 '(custom-state-face                          ((t (:foreground "dark green"))))
 '(custom-variable-button-face                ((t (                                     :bold t :underline t))))
 '(custom-variable-tag-face                   ((t (:foreground "blue"                           :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(diary-face                                 ((t (:foreground "red"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(ediff-current-diff-face-A                  ((t (:foreground "firebrick" :background "pale green"))))
 '(ediff-current-diff-face-Ancestor           ((t (:foreground "Black" :background "VioletRed"))))
 '(ediff-current-diff-face-B                  ((t (:foreground "DarkOrchid" :background "Yellow"))))
 '(ediff-current-diff-face-C                  ((t (:foreground "Navy" :background "Pink"))))
 '(ediff-even-diff-face-A                     ((t (:foreground "Black" :background "light grey"))))
 '(ediff-even-diff-face-Ancestor              ((t (:foreground "White" :background "Grey"))))
 '(ediff-even-diff-face-B                     ((t (:foreground "White" :background "Grey"))))
 '(ediff-even-diff-face-C                     ((t (:foreground "Black" :background "light grey"))))
 '(ediff-fine-diff-face-A                     ((t (:foreground "Navy" :background "sky blue"))))
 '(ediff-fine-diff-face-Ancestor              ((t (:foreground "Black" :background "Green"))))
 '(ediff-fine-diff-face-B                     ((t (:foreground "Black" :background "cyan"))))
 '(ediff-fine-diff-face-C                     ((t (:foreground "Black" :background "Turquoise"))))
 '(ediff-odd-diff-face-A                      ((t (:foreground "White" :background "Grey"))))
 '(ediff-odd-diff-face-Ancestor               ((t (:foreground "Black" :background "light grey"))))
 '(ediff-odd-diff-face-B                      ((t (:foreground "Black" :background "light grey"))))
 '(ediff-odd-diff-face-C                      ((t (:foreground "White" :background "Grey"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(eshell-ls-archive-face                     ((t (:foreground "Orchid" :bold t))))
 '(eshell-ls-backup-face                      ((t (:foreground "OrangeRed"))))
 '(eshell-ls-clutter-face                     ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-ls-directory-face                   ((t (:foreground "Blue" :bold t))))
 '(eshell-ls-executable-face                  ((t (:foreground "ForestGreen" :bold t))))
 '(eshell-ls-missing-face                     ((t (:foreground "Red" :bold t))))
 '(eshell-ls-product-face                     ((t (:foreground "OrangeRed"))))
 '(eshell-ls-readonly-face                    ((t (:foreground "Brown"))))
 '(eshell-ls-special-face                     ((t (:foreground "Magenta" :bold t))))
 '(eshell-ls-symlink-face                     ((t (:foreground "DarkCyan" :bold t))))
 '(eshell-ls-unreadable-face                  ((t (:foreground "Grey30"))))
 '(eshell-prompt-face                         ((t (:foreground "Red" :bold t))))
 '(eshell-test-failed-face                    ((t (:foreground "OrangeRed" :bold t))))
 '(eshell-test-ok-face                        ((t (:foreground "Green" :bold t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(flyspell-duplicate-face                    ((t (:foreground "Gold3" :bold t :underline t))))
 '(flyspell-incorrect-face                    ((t (:foreground "OrangeRed" :bold t :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(gnus-cite-attribution-face                 ((t (:italic t))))
 '(gnus-cite-face-1                           ((t (:foreground "MidnightBlue"))))
 '(gnus-cite-face-10                          ((t (:foreground "medium purple"))))
 '(gnus-cite-face-11                          ((t (:foreground "turquoise"))))
 '(gnus-cite-face-2                           ((t (:foreground "firebrick"))))
 '(gnus-cite-face-3                           ((t (:foreground "dark green"))))
 '(gnus-cite-face-4                           ((t (:foreground "OrangeRed"))))
 '(gnus-cite-face-5                           ((t (:foreground "dark khaki"))))
 '(gnus-cite-face-6                           ((t (:foreground "dark violet"))))
 '(gnus-cite-face-7                           ((t (:foreground "SteelBlue4"))))
 '(gnus-cite-face-8                           ((t (:foreground "magenta"))))
 '(gnus-cite-face-9                           ((t (:foreground "violet"))))
 '(gnus-emphasis-bold                         ((t (:bold t))))
 '(gnus-emphasis-bold-italic                  ((t (:bold t :italic t))))
 '(gnus-emphasis-italic                       ((t (:italic t))))
 '(gnus-emphasis-underline                    ((t (:underline t))))
 '(gnus-emphasis-underline-bold               ((t (:bold t :underline t))))
 '(gnus-emphasis-underline-bold-italic        ((t (:bold t :italic t :underline t))))
 '(gnus-emphasis-underline-italic             ((t (:italic t :underline t))))
 '(gnus-group-mail-1-empty-face               ((t (:foreground "DeepPink3"))))
 '(gnus-group-mail-1-face                     ((t (:foreground "DeepPink3" :bold t))))
 '(gnus-group-mail-2-empty-face               ((t (:foreground "HotPink3"))))
 '(gnus-group-mail-2-face                     ((t (:foreground "HotPink3" :bold t))))
 '(gnus-group-mail-3-empty-face               ((t (:foreground "magenta4"))))
 '(gnus-group-mail-3-face                     ((t (:foreground "magenta4" :bold t))))
 '(gnus-group-mail-low-empty-face             ((t (:foreground "DeepPink4"))))
 '(gnus-group-mail-low-face                   ((t (:foreground "DeepPink4" :bold t))))
 '(gnus-group-news-1-empty-face               ((t (:foreground "ForestGreen"))))
 '(gnus-group-news-1-face                     ((t (:foreground "ForestGreen" :bold t))))
 '(gnus-group-news-2-empty-face               ((t (:foreground "CadetBlue4"))))
 '(gnus-group-news-2-face                     ((t (:foreground "CadetBlue4" :bold t))))
 '(gnus-group-news-3-empty-face               ((t (nil))))
 '(gnus-group-news-3-face                     ((t (:bold t))))
 '(gnus-group-news-low-empty-face             ((t (:foreground "DarkGreen"))))
 '(gnus-group-news-low-face                   ((t (:foreground "DarkGreen" :bold t))))
 '(gnus-header-content-face                   ((t (:foreground "indianred4" :italic t))))
 '(gnus-header-from-face                      ((t (:foreground "red3"))))
 '(gnus-header-name-face                      ((t (:foreground "maroon"))))
 '(gnus-header-newsgroups-face                ((t (:foreground "MidnightBlue" :italic t))))
 '(gnus-header-subject-face                   ((t (:foreground "red4"))))
 '(gnus-signature-face                        ((t (:italic t))))
 '(gnus-splash-face                           ((t (:foreground "ForestGreen"))))
 '(gnus-summary-cancelled-face                ((t (:foreground "yellow" :background "black"))))
 '(gnus-summary-high-ancient-face             ((t (:foreground "RoyalBlue" :bold t))))
 '(gnus-summary-high-read-face                ((t (:foreground "DarkGreen" :bold t))))
 '(gnus-summary-high-ticked-face              ((t (:foreground "firebrick" :bold t))))
 '(gnus-summary-high-unread-face              ((t (:bold t))))
 '(gnus-summary-low-ancient-face              ((t (:foreground "RoyalBlue" :italic t))))
 '(gnus-summary-low-read-face                 ((t (:foreground "DarkGreen" :italic t))))
 '(gnus-summary-low-ticked-face               ((t (:foreground "firebrick" :italic t))))
 '(gnus-summary-low-unread-face               ((t (:italic t))))
 '(gnus-summary-normal-ancient-face           ((t (:foreground "RoyalBlue"))))
 '(gnus-summary-normal-read-face              ((t (:foreground "DarkGreen"))))
 '(gnus-summary-normal-ticked-face            ((t (:foreground "firebrick"))))
 '(gnus-summary-normal-unread-face            ((t (nil))))
 '(gnus-summary-selected-face                 ((t (:underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(highlight                                  ((t (:background "black" :foreground "white" :bold 1))))
 '(highlight-changes-delete-face              ((t (:foreground "red" :underline t))))
 '(highlight-changes-face                     ((t (:foreground "red"))))
 '(highline-face                              ((t (:background "paleturquoise"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(holiday-face                               ((t (:background "pink"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(info-menu-5                                ((t (:underline t :bold t))))
 '(info-node                                  ((t (:bold t))))
 '(info-xref                                  ((t (:bold t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(message-cited-text-face                    ((t (:foreground "red"))))
 '(message-header-cc-face                     ((t (:foreground "MidnightBlue"))))
 '(message-header-name-face                   ((t (:foreground "cornflower blue"))))
 '(message-header-newsgroups-face             ((t (:foreground "blue4" :bold t :italic t))))
 '(message-header-other-face                  ((t (:foreground "steel blue"))))
 '(message-header-subject-face                ((t (:foreground "navy blue" :bold t))))
 '(message-header-to-face                     ((t (:foreground "MidnightBlue" :bold t))))
 '(message-header-xheader-face                ((t (:foreground "blue"))))
 '(message-separator-face                     ((t (:foreground "brown"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(speedbar-button-face                       ((t (:foreground "green4"))))
 '(speedbar-directory-face                    ((t (:foreground "blue4"))))
 '(speedbar-file-face                         ((t (:foreground "cyan4"))))
 '(speedbar-highlight-face                    ((t (:background "green"))))
 '(speedbar-selected-face                     ((t (:foreground "red" :underline t))))
 '(speedbar-tag-face                          ((t (:foreground "brown"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(term-black                                 ((t (:foreground "black"))))
 '(term-blackbg                               ((t (:background "black"))))
 '(term-blue                                  ((t (:foreground "blue"))))
 '(term-bluebg                                ((t (:background "blue"))))
 '(term-bold                                  ((t (:bold t))))
 '(term-cyan                                  ((t (:foreground "cyan"))))
 '(term-cyanbg                                ((t (:background "cyan"))))
 '(term-default-bg                            ((t (nil))))
 '(term-default-bg-inv                        ((t (nil))))
 '(term-default-fg                            ((t (nil))))
 '(term-default-fg-inv                        ((t (nil))))
 '(term-green                                 ((t (:foreground "green"))))
 '(term-greenbg                               ((t (:background "green"))))
 '(term-invisible                             ((t (nil))))
 '(term-invisible-inv                         ((t (nil))))
 '(term-magenta                               ((t (:foreground "magenta"))))
 '(term-magentabg                             ((t (:background "magenta"))))
 '(term-red                                   ((t (:foreground "red"))))
 '(term-redbg                                 ((t (:background "red"))))
 '(term-underline                             ((t (:underline t))))
 '(term-white                                 ((t (:foreground "white"))))
 '(term-whitebg                               ((t (:background "white"))))
 '(term-yellow                                ((t (:foreground "yellow"))))
 '(term-yellowbg                              ((t (:background "yellow"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(vhdl-font-lock-attribute-face              ((t (:foreground "Orchid"))))
 '(vhdl-font-lock-directive-face              ((t (:foreground "CadetBlue"))))
 '(vhdl-font-lock-enumvalue-face              ((t (:foreground "Gold4"))))
 '(vhdl-font-lock-function-face               ((t (:foreground "Orchid4"))))
 '(vhdl-font-lock-prompt-face                 ((t (:foreground "Red" :bold t))))
 '(vhdl-font-lock-reserved-words-face         ((t (:foreground "Orange" :bold t))))
 '(vhdl-font-lock-translate-off-face          ((t (:background "LightGray"))))
 '(vhdl-speedbar-architecture-face            ((t (:foreground "Blue"))))
 '(vhdl-speedbar-architecture-selected-face   ((t (:foreground "Blue" :underline t))))
 '(vhdl-speedbar-configuration-face           ((t (:foreground "DarkGoldenrod"))))
 '(vhdl-speedbar-configuration-selected-face  ((t (:foreground "DarkGoldenrod" :underline t))))
 '(vhdl-speedbar-entity-face                  ((t (:foreground "ForestGreen"))))
 '(vhdl-speedbar-entity-selected-face         ((t (:foreground "ForestGreen" :underline t))))
 '(vhdl-speedbar-instantiation-face           ((t (:foreground "Brown"))))
 '(vhdl-speedbar-instantiation-selected-face  ((t (:foreground "Brown" :underline t))))
 '(vhdl-speedbar-package-face                 ((t (:foreground "Grey50"))))
 '(vhdl-speedbar-package-selected-face        ((t (:foreground "Grey50" :underline t))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 '(viper-minibuffer-emacs-face                ((t (:foreground "Black" :background "darkseagreen2"))))
 '(viper-minibuffer-insert-face               ((t (:foreground "Black" :background "pink"))))
 '(viper-minibuffer-vi-face                   ((t (:foreground "DarkGreen" :background "grey"))))
 '(viper-replace-overlay-face                 ((t (:foreground "Black" :background "darkseagreen2"))))
 '(viper-search-face                          ((t (:foreground "Black" :background "khaki"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-theme 'sboo-high-contrast-inverted-colors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sboo-high-contrast-inverted-colors-theme.el ends here
