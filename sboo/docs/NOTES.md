# NOTES

## (templates)

```elisp
```

## `haskell-decl-scan.el`

```elisp
(defvar haskell-ds-start-keywords-re

  (concat "\\(\\<"
          "class\\|data\\|i\\(mport\\|n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|"
          "module\\|primitive\\|type\\|newtype"
          "\\)\\>")

  "Keywords that may start a declaration.")
```

```elisp
(defvar haskell-ds-syntax-table

  (let ((table (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?\' "w" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\\ "_" table)
    table)

  "Syntax table used for Haskell declaration scanning.")
```

```elisp
(defun haskell-ds-move-to-decl (direction bird-literate fix)

  "General function for moving to the start of a declaration,
either forwards or backwards from point, with normal or with Bird-style
literate scripts.  If DIRECTION is t, then forward, else backward.  If
BIRD-LITERATE is t, then treat as Bird-style literate scripts, else
normal scripts.  Returns point if point is left at the start of a
declaration, and nil otherwise, ie. because point is at the beginning
or end of the buffer and no declaration starts there.  If FIX is t,
then point does not move if already at the start of a declaration."

  ...)
```

e.g. `haskell-ds-move-to-decl`:

```elisp
```


```elisp
```



## 