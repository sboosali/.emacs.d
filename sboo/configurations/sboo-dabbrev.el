;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dabbrev'
;;
;; i.e. Dynamic ABBREViations
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DynamicExpansion of most Variables
;;
;; define a "word", for `dabbrev', to be a sequence of **any** non-whitespace characters.
;; 
;; by default, for example, the "-" separator (i.e. a hyphen) is "part-of-a-word", while the "_" separator (i.e. an underscore) is not. 
;; 
;; 

;; `dabbrev-abbrev-char-regexp`



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DynamicExpansion of FilePaths
;;

;; quotient: relative-filepaths with absolute-filepaths with "unprefixed"-filepaths.
;;
;; e.g. typing this:
;;
;;     et
;;
;; should match these three tokens (if any are in the buffer):
;; 
;;     etc/
;;     ./etc/
;;     /etc/
;; 
;; 
;;
;; modes:
;; shell-mode
;; nix-mode
;; bash-mode
;;

(setq
 dabbrev-abbrev-skip-leading-regexp "\\sw") ;;TODO? "[\./*]"

;; ^ `dabbrev-abbrev-skip-leading-regexp':
;;
;; buffer-local (?)
;;
;; a regular expression
;;
;; matches the optional prefix that `dabbrev` should ignore. 
;;
;; e.g. in shell scripts and makefiles, a variable name is sometimes prefixed with ‘$’, and sometimes not (e.g. prefixed for reference, not prefixed for assignment).
;;
;; `nil', the default, means no quotioned prefixes.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq

;; dabbrev-abbrev-char-regexp 

 ;; ^ is a regex.
 ;;
 ;; if non-`nil', controls which characters are considered, for the purpose of dynamic expansion, to be part-of-a-word.
 ;; 
 ;; the regex must match at-most *one* character (never two or more).
 ;;
 ;; the regex `"\\sw"', i.e.
 ;;    \sw
 ;; means "any TODO".
 ;; 
 ;; 

 dabbrev-case-replace nil

  ;; ^ 
  ;; `t`: 
  ;; `nil`: insert the expansion verbatim (i.e. ignore the word-at-point's current capitalization).

  dabbrev-case-fold-search nil)

  ;; ^ 
  ;; `t`: matching is case-insensitive;
  ;; `nil`: matching is case-sensitive (i.e. skip words with different capitalization).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; dabbrev-expand versus dabbrev-completion
;;
;; dabbrev-expand: find the "next" possible expansion, and insert it (one by one).
;;
;; dabbrev-completion: find all possible exceptions, then insert their **longest common prefix**.
;;

;; See
;;    - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html
;;    - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dabbrev-Customization.html
;;    - https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dabbrev)