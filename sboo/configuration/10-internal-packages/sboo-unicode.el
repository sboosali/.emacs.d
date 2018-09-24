;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (progn
;;   (define-abbrev-table 'global-abbrev-table '( ;;TODO
;;     ("alpha" "α")
;;     ("inf" "∞")
;;     ("ar" "→")
;;     ))
;;   (abbrev-mode 1)) ; turn `abbrev-mode' on.

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; # Unicode characters for the Keyboard.
;;
;; ## Modifiers:
;; 
;; - « ⌃ »: the Control key (a.k.a "ctrl")
;; - « ⌥ »: the Option/Alt key (a.k.a "alt")
;; - « ⇧ »: the Shift key (a.k.a "shift")
;; - « ❖ »: the Win key (Windows Logo)
;; - « ⌘ »: the Command key (Apple keyboards)
;; - « 🄰 »: the Capslock key
;; - « ◆ »: the Meta key (on LISP Machine keyboards)
;;
;; ## Non-Printable & Whitespace Keys:
;; 
;; - « ⏎ »: the Return key.
;; - « ⌫ »: the Backspace key.
;; - « ⭾ »: the Tab key.
;; - « ↑ »: the Up-Arrow key.
;; - « ↓ »: the Down-Arrow key.
;; - « ← »: the Right-Arrow key.
;; - « → »: the Left-Arrow key.
;; - « ⎋ »: the Escape key.
;; - « ␣ »: the Space key.
;;
;; ## Hardware Keys:
;;
;; - « 🔇 »: the Mute speaker-key.
;; - « 🔉 »: the Increase-Volume speaker-key.
;; - « 🔊 »: the Decrease-Volume speaker-key.
;; - « 🔅 »: the Decrease-Brightness screen-key.
;; - « 🔆 »: the Increase-Brightness screen-key.
;;
;; ## Shortcut Keys:
;;
;; - «  »: the Copy shortcut-key.
;; - « ✂ »: the Cut shortcut-key.
;; - « 📋 »: the Paste shortcut-key (a.k.a. the Clipboard glyph).
;; - « ⎌ »: the Undo shortcut-key.
;; - « 🔍 »: the Search shortcut-key.
;; - « ↺ »: the Reload/Refresh shortcut-key.
;; - « ⎙ »: the Print shortcut-key.
;; - « ❔ »: the Help shortcut-key.
;; - « ⇞ »: the PageUp shortcut-key.
;; - « ⇟ »: the PageDown shortcut-key.
;; - « ⎗ »: the PreviousPage shortcut-key.
;; - « ⎘ »: the NextPage shortcut-key.
;;
;; ## Media Keys:
;;
;; - « ◼ »: the Stop media-key.
;; - « ⏯ »: the Toggle-Play-Pause media-key.
;; - « ⏪ »: the Previous-Track media-key.
;; - « ⏩ »: the Next-Track media-key.
;;

;; ## Textual Unicode characters.
;;
;; - « ‘...’ »: single-quotation marks.
;; - « “...” »: double-quotation marks.
;; - « ‹...› »: single-angle left-pointing/right-pointing quotation.
;; - « «...» »: left-pointing/right-pointing double-angle quotation.
;; - « 【...】 »: black lenticular brackets.
;; - « 〈...〉 »: angle brackets.
;; - « 《...》 »: double-angle brackets.
;;

;; ## Unicode numerals.
;;
;; sans-serif Circled Numbers:
;;
;; - « 🄋 »
;; - « ➀ »
;; - « ➁ »
;; - « ➂ »
;; - « ➃ »
;; - « ➄ »
;; - « ➅ »
;; - « ➆ »
;; - « ➇ »
;; - « ➈ »
;; - « ➉ »
;;

;; ## Unicode characters for Mathematical Symbols (or in Mathematical Fonts).
;;
;; Numbers:
Sets:
;;
;; - « ℤ »: integers
;; - « ℕ »: natural numbers.
;; - « ℙ »: primes.
;; - « ℚ »: rationals.
;; - « ℝ »: reals.
;; - « ℂ »: complex numbers.
;;
;; imaginary numbers: « ⅈ », « ⅉ ».
;;
;; Superscripts: ⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁺ ⁻ ⁼ ⁽ ⁾ ⁿ ⁱ
;;
;; Subscripts: ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉ ₊ ₋ ₌ ₍ ₎ ₐ ₑ ₕ ᵢ ⱼ ₖ ₗ ₘ ₙ ₒ ₚ ᵣ ₛ ₜ ᵤ ᵥ ₓ ₔ
;;
;; Operators:
;;
;; - « √ »: SquareRoot.
;;
;; Sets:
;;
;; - « ∅ »: the EmptySet.
;; - « ∈ »: "is-element-of".
;; - « ∋ »: "has-as-element".
;; - « ∉ »: "isn't-element-of".
;; - « ∌ »: "hasn't-as-element".
;; - « ⊂ »: "contained-by".
;; - « ⊃ »: "contains".
;; - « ⊆ »: "is-subset-of".
;; - « ⊇ »: "is-superset-of".
;; - « ⊈ »: "isn't-subset-of".
;; - « ⊉ »: "isn't-superset-of".
;; - « ∪ »: Set-Union.
;; - « ∩ »: Set-Intersection.
;;
;; Equalities:
;;
;; - « ≡ »: "is-identical-to".
;; - « ≠ »: "isn't-equal-to".
;; - « ≈ »: "is-almost-equal-to".
;; - « ≝ »: "is-equal-to-by-definition".
;; - « ∹ »: "entails" ("is-implied-by"?).
;; - « ∝ »: "is-proportional-to".
;;
;; Logic:
;;
;; - « ¬ »: LogicalNegation.
;; - « ∀ »: "for-all" Quantifier.
;; - « ∃ »: "there-exists" Quantifier.
;; - « ∄ »: "there-doesn't-exist" Quantifier.
;;
;; Inequalities:
;;
;; - « < »: "is-less-than".
;; - « > »: "is-greater-than".
;; - « ≮ »: "isn't-less-than".
;; - « ≯ »: "isn't-greater-than".
;; - « ≤ »: "is-less-than-or-equal-to".
;; - « ≥ »: "is-greater-than-or-equal-to".
;;
;; Functionals (maps, mapping, transform):
;;
;; - « ⊶ »: the Original-Of (?) Relation.
;; - « ⊷ »: the Image-Of Relation.
;; - « ⊸ »: the Multi-Map Relation.
;; - « ⟜ »: the Left Multi-Map Relation. 
;;
;; Functions:
;;
;; - « ∿ »: the SineWave function.
;; 
;; Miscellaneous Symbols:
;;
;;  


;; ## Miscellaneous Unicode characters.
;;
;; - « ✓ »: CheckMark.
;; - « • »: Bullet.
;; - « ❌ »: CrossMark.
;;

;; Notes: platform-specific keys.
;;
;; The Windows Logo key and the Apple Command key both send the same USB scancode.
;;
;; On Linux, by convention, the Windows logo key is called “Super”. (not to be confused with a real Super key on Lisp Machine keyboards. 
;;
;; « Alt » on PC keyboard and « ⌥ option » on Apple Keyboards send the same USB scancode.
;;
;; the Menu/App key « 𝌆 » (invented by Microsoft) is meant to be a application-specific key.

;; Notes: miscellaneous.
;;
;; Most keyboards use “Esc” as label.
;;
;;

;; See:
;;     - http://ergoemacs.org/emacs/emacs_n_unicode.html
;;     - http://xahlee.info/comp/unicode_matching_brackets.html
;;     - http://xahlee.info/comp/unicode_common_symbols.html
;;     - http://xahlee.info/comp/unicode_index.html
;;     - http://xahlee.info/comp/unicode_computing_symbols.html
;;     - http://xahlee.info/comp/unicode_math_operators.html
;;     - 
;;     - 
;;
;;     - https://stackoverflow.com/questions/13535172/list-of-all-unicodes-open-close-brackets
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-unicode)