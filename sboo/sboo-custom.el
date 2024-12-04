(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-raise-tool-bar-buttons t t nil "visually raise a Tool Bar Item when the Mouse hovers over it.")
 '(auto-resize-tool-bars t t nil "do grow the Tool Bar when enough Menu Items are added.")
 '(column-number-mode t nil nil "show Column Numbers in the Modeline (by default, only Row Numbers are shown).")
 '(cua-keep-region-after-copy t nil nil "standard Windows behavior.")
 '(custom-theme-directory "/home/sboosali/.emacs.d/sboo/themes/" nil nil "Personal themes.")
 '(echo-keystrokes 1 nil nil "Wait this many seconds, then echo the currently-pressed key sub-sequence.")
 '(enable-local-variables :safe nil nil "set only Safe Variables (don't query for unsafe ones).")
 '(enable-recursive-minibuffers t nil nil "so you can: ① press « M-x » within a « M-x »; ② search through (via a second « C-s ») the minibuffer of a search command for the (non-mini) buffer (having pressed the first « C-s »).")
 '(eshell-destroy-buffer-when-process-dies t t nil "“To get rid of those lingering buffers.”")
 '(eval-expression-print-length nil nil nil "‘nil’ means ‘eval-expression’ prints arbitrarily-long expressions.")
 '(eval-expression-print-level nil nil nil "‘nil’ means ‘eval-expression’ prints arbitrarily-deep expressions.")
 '(grep-find-template
   "find -L <D> <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} +" nil nil "`find-grep' for `nix'. « -L » traverses symlinks (emacs packages installed via « nix » are symlinks).")
 '(grep-first-column 1 t nil "One-based columns (≡ 1) or Zero-based columns (≡ 0).")
 '(grep-highlight-matches 'always nil nil "grep ‘--color=’ (`auto' or `always')")
 '(grep-save-buffers t nil nil "Don't ask (just save all buffers).")
 '(grep-scroll-output t nil nil "Jump to result (`point' at end of output window).")
 '(help-at-pt-display-when-idle t nil (help-at-pt) "automatically show Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point) on point-over (i.e. when `point' moves there, not when the `cursor' hovers over).")
 '(help-at-pt-timer-delay 3 nil nil "Wait this many seconds before showing Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point)")
 '(history-length 10000 nil nil "increase Minibuffer History.")
 '(indent-tabs-mode nil nil nil "no Extraneous Tabs.")
 '(inhibit-startup-screen t nil nil "fewer Startup Buffers.")
 '(initial-scratch-message nil nil nil "fewer Startup Buffers.")
 '(kill-whole-line t nil nil "« C-k » eats newlines (a.k.a. it kills the whole line). By enabling `kill-whole-line', we can type just « C-k » where before we typed « C-k C-k ».")
 '(linum-format 'dynamic t nil "")
 '(lisp-indent-function 'common-lisp-indent-function nil nil "format Property Lists correctly.")
 '(message-truncate-lines t t nil "don't resize Echo Area for long messages (instead, truncate the message).")
 '(minibuffer-depth-indicate-mode t nil nil "e.g. minibuffer displays « M-x [2] » when you've (often accidentally) double-« M-x »'d.")
 '(mode-require-final-newline nil nil nil "no Automatic Insertion of a Final Newline.")
 '(mouse-1-click-follows-link t nil nil "Left-Click a HyperLink to open it.")
 '(mouse-wheel-follow-mouse t nil nil "scroll Window under Mouse (by default, scroll `selected-window' always).")
 '(mouse-wheel-mode t nil nil "enable the Mouse-Wheel.")
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)) nil nil "`1' means: Mouse-Wheel scrolls one line per flick (by default, it's 5).")
 '(mouse-yank-at-point t nil nil "don't move `point' when pasting with the mouse.")
 '(next-screen-context-lines 0 nil nil "")
 '(package-selected-packages
   '(helm-config yasnippet wrap-region treemacs tabspaces solarized-theme selected real-auto-save projectile nix-mode markdown-mode marginalia magit json-mode helm haskell-mode flycheck f expand-region embark consult company ahk-mode))
 '(print-length nil t nil "‘nil’ means print arbitrarily-long expressions.")
 '(print-level nil t nil "‘nil’ means print arbitrarily-deep expressions.")
 '(print-quoted t t nil "‘t’ means print « (quote foo) » as « 'foo » and « (function foo) » as « #'foo ».")
 '(redisplay-dont-pause t t nil "“Peeking” behavior when scrolling.")
 '(require-final-newline nil nil nil "no Automatic Insertion of a Final Newline.")
 '(ring-bell-function 'ignore nil nil "disable Blinking and Flashing.")
 '(scroll-bar-height 64 t nil "Enlarge the Scrollbar, in Pixels (Defaults to 8px tall).")
 '(scroll-bar-width 64 t nil "Enlarge the Scrollbar, in Pixels (Defaults to 8px wide).")
 '(scroll-conservatively 10000 nil nil "Scroll smoothly.")
 '(scroll-margin 0 nil nil "Scroll smoothly.")
 '(scroll-preserve-screen-position 'always nil nil "`t' means that Emacs adjusts point to keep the cursor at the same screen position whenever a scroll command moves it off-window, rather than moving it to the topmost or bottommost line.")
 '(scroll-step 1 nil nil "")
 '(select-enable-clipboard t nil nil "Non-nil means: cutting and pasting uses the clipboard.")
 '(sentence-end-double-space nil nil nil "sentences end with a Single Period.")
 '(set-mark-command-repeat-pop t nil nil "If you set set-mark-command-repeat-pop to non- nil , then immediately after you type C-u C-<SPC> , you can type C-<SPC> instead of C-u C-<SPC> to cycle through the mark ring.")
 '(show-paren-delay 0 nil nil "no Delay.")
 '(show-paren-style 'mixed nil nil "highlight the Parenthesized Expression, unless the Matching Parenthesis is visible (not just the Parenthesis).")
 '(size-indication-mode t nil nil "show Buffer Size in the Modeline.")
 '(tool-bar-position 'left t nil "the Tool Bar is on the left.")
 '(truncate-lines nil nil nil "enable Continuation Lines.")
 '(undo-limit 20000000 nil nil "maximize Undo History.")
 '(undo-strong-limit 40000000 nil nil "maximize Undo History.")
 '(use-dialog-box nil nil nil "`nil' replaces Dialog Boxes with `yes-or-no' prompts.")
 '(user-full-name "Spiros Boosalis" nil nil "")
 '(user-mail-address "SamBoosalis@gmail.com" nil nil "")
 '(vertical-scroll-bars 'right t nil "Enable and Position the Scrollbar.")
 '(view-read-only t nil nil "`t' means: always launch read-only buffers/files with `view-mode'.")
 '(visible-bell t nil nil "flash a Black Square onto the screen on User Errors (instead of honking loudly through the speakers).")
 '(x-underline-at-descent-line t nil nil "put the Underline below the Font Bottomline (instead of the Font Baseline)."))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
