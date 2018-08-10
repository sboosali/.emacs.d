;;; Compiled snippets and support files for `yasnippet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'yasnippet-mode
                     '(("def" "# -*- mode: snippet -*-\n#\n# key         : ${1}\n# name        : a `.yasnippet`\n# condition   : (= (length \"${1}\") (current-column))\n# expand-env  : ((yas-indent-line 'fixed) (yas-wrap-around-region 'nil))\n# contributor : Spiros Boosalis\n#\n# --\n" "`Default` instance and a constant."
                        (=
                         (length "def")
                         (current-column))
                        nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/sboo/.emacs.d/sboo/snippets/yasnippet-mode/yasnippet.yasnippet" nil nil)))


;;; Do not edit! File generated at Thu Aug  9 16:47:30 2018
