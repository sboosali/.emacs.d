;;; Compiled snippets and support files for `yasnippet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'yasnippet-mode
                     '(("yas" "# -*- mode: snippet -*-\n#\n# key         : ${1}\n# name        : [sboo] \n# condition   : (= (length \"${1}\") (current-column))\n# expand-env  : ((yas-indent-line 'fixed) (yas-wrap-around-region 'nil))\n# contributor : Spiros Boosalis\n#\n# --\n" "[sboo] the `.yasnippet` header, with several directives"
                        (=
                         (length "yas")
                         (current-column))
                        nil nil "/home/sboo/.emacs.d/sboo/snippets/yasnippet-mode/yasnippet.yasnippet" nil nil)))


;;; Do not edit! File generated at Tue Aug 14 02:36:53 2018
