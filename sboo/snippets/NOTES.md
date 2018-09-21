# YASnippet

## <emacs-lisp-mode/defalias.yasnippet>

```
#
# The regex « "[-/]+" » means: one-or-more hyphens and/or slashes.
# 
# (defun sboo-abbreviate (STRING) (let ((*separators* "[-/]+") (*omit-nulls* t)) (apply #'string (mapcar #'string-to-char (split-string STRING *separators* *omit-nulls*)))))
# 
# >>> (sboo-abbreviate "some-command")
# "sc"
```

## 