(provide 'my-abbrev)


;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")

;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(setq save-abbrevs t)

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; keep it always on
;(setq default-abbrev-mode t)

(defun after-abbrev-expand-hook ()
  (when (looking-back "\"\"\\|''\\|()\\|\\[\\]\\|{}")
    (backward-char 1))
  t)

(put 'after-abbrev-expand-hook 'no-self-insert t)

