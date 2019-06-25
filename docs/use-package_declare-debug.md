



TODO declare edebug for use-package macro's keywords

i.e. add:

``` emacs-lisp
(defmacro use-package
  "..."

  (declare (indent ...) (debug ...))

  ...)
```

benefits include:

* *Edebug* Integration (as per the namesake)
* Better Indentation — 
* Better Syntax-Highlighting — docstrings are gray like comments, rather than red like other strings, for example. (TODO: check)
* Better Linting — `flycheck` (via `bytecode-compile`) should recognize which symbols are variables and which are functions. currently, for example, for bound functions `f` and `g` and `h`, `:commands (f g h)` warns that (paraphrasing) *‘g’ and ‘h’ aren't bound*, because they look (lexically) like variables. (TODO: check)

positional arguments: 

* 1st - symbol.

keyword arguments:

* config: form(s).
* init: form(s).
* preface: form(s).

* commands — function symbol or list thereof.
* functions — function symbol or list thereof.
* defines — variable symbol or list thereof.

* bind / bind* / bind-keymap / :bind-keymap* — cons of string-or-vector & function symbol; or list thereof (i.e. an alist); or list of `:map` then a variable symbol, then  a list thereof (and so on).

* hook — symbol; cons of a variable symbol and a function symbol; 

cons of string-or-vector & function symbol; or list thereof (i.e. an alist); or list of `:map` then a variable symbol, then  a list thereof (and so on).

* custom — one-or-more (i.e. `&rest`) of: a list (a "tuple") of a variable symbol and a form and an optional *docstring* string.
* custom-face — one-or-more (i.e. `&rest`) of: a list (a "tuple") of a variable symbol and a form and an optional *docstring* string.

* mode — string; cons string function; list thereof.
* interpreter — string; cons string function; list thereof.

:magic           
:magic-fallback      

* after: symbol or sexp (each list/sublist may start with `:all` or `:any` or nothing.)
* load-path: string or list thereof (or form)

* delight: (nothing); function symbol; list (tuple) with function symbol then string; one-or-more of the above; 
* diminish: 

>A specification list is required for an Edebug specification if some arguments of a macro call are evaluated while others are not.

See:

* <https://www.gnu.org/software/emacs/manual/html_node/elisp/Specification-List.html>
* <https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html>

Related:

* `use-package-keywords`: the way I intend to implement this issue, the keywords are static. perhaps, extension authors can register their own *Edebug Specification Lists* by writing their own `declare-function`s statements, which (afaict) appends these lists, rather than overriding the first-party one.




(use-package ace-jump-mode
  :hook prog-mode)

(use-package ace-jump-mode
  :hook (prog-mode . ace-jump-mode))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (add-hook 'prog-mode-hook #'ace-jump-mode))

:;

(use-package ace-jump-mode
  :hook (prog-mode text-mode))

(use-package ace-jump-mode
  :hook ((prog-mode text-mode) . ace-jump-mode))

(use-package ace-jump-mode
  :hook ((prog-mode . ace-jump-mode)
         (text-mode . ace-jump-mode)))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (add-hook 'prog-mode-hook #'ace-jump-mode)
  (add-hook 'text-mode-hook #'ace-jump-mode))



 '(:disabled
    :load-path
    :requires
    :defines
    :functions
    :preface
    :if :when :unless
    :no-require
    :catch
    :after
    :custom
    :custom-face
    :bind
    :bind*
    :bind-keymap
    :bind-keymap*
    :interpreter
    :mode
    :magic
    :magic-fallback
    :hook
    ;; Any other keyword that also declares commands to be autoloaded (such as
    ;; :bind) must appear before this keyword.
    :commands
    :init
    :defer
    :demand
    :load
    ;; This must occur almost last; the only forms which should appear after
    ;; are those that must happen directly after the config forms.
    :config)









