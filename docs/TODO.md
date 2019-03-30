# TODO

## 

    ‘graphic’, ‘graph’
     matches graphic characters--everything except whitespace, ASCII
     and non-ASCII control characters, surrogates, and codepoints
     unassigned by Unicode.

## 

```
(defvar rx-constituents              ;Not `const' because some modes extend it.
  '(
    (submatch-n		. (rx-submatch-n 2 nil))
    (group-n		. submatch-n)
   ))

(defun rx-submatch-n (form)

  "Parse and produce code from FORM, which is `(submatch-n N ...)'."

  (let ((n (nth 1 form)))

    (concat "\\(?" (number-to-string n) ":"
	    (if (= 3 (length form))
		;; Only one sub-form.
		(rx-form (nth 2 form))
	      ;; Several sub-forms implicitly concatenated.
	      (mapconcat (lambda (re) (rx-form re ':)) (cddr form) nil))
	    "\\)")))
```

## theme

https://github.com/kunalb/poet/blob/master/poet-theme.el#L245

```

```

## 

```

```

## 

```

```

## 

```

```

## 

```

```

## 

```

```

## 

```

```

## 

```

```

## 

```

```

## 

