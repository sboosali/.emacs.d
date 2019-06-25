(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "/home/sboo/.emacs.d/sboo/dabbrev/abbrev_defs.el")
 '(auto-raise-tool-bar-buttons t t nil "visually raise a Tool Bar Item when the Mouse hovers over it.")
 '(auto-resize-tool-bars t t nil "do grow the Tool Bar when enough Menu Items are added.")
 '(awesome-tab-background-color "#fdf6e3")
 '(awesome-tab-label-fixed-length 14)
 '(awesome-tab-mode t nil (awesome-tab))
 '(awesome-tab-style "alternate")
 '(bm-buffer-persistence t t)
 '(browse-url-browser-function 'browse-url-chrome)
 '(calendar-week-start-day 0)
 '(centaur-tabs-bar-height 28)
 '(centaur-tabs-close-button "❌" t)
 '(centaur-tabs-common-group-name "?")
 '(centaur-tabs-cycle-scope 'groups)
 '(centaur-tabs-height 22)
 '(centaur-tabs-set-close-button t)
 '(checkdoc-arguments-in-order-flag t)
 '(column-number-mode t nil nil "show Column Numbers in the Modeline (by default, only Row Numbers are shown).")
 '(comint-buffer-maximum-size 65536)
 '(comint-scroll-to-bottom-on-input 'this)
 '(comint-scroll-to-bottom-on-output 'others)
 '(company-dabbrev-downcase nil)
 '(company-ispell-dictionary "/home/sboo/.emacs.d/sboo/data/english-words.txt")
 '(company-minimum-prefix-length 1)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(csv-separators '("," ";" "|" " ") t)
 '(cua-keep-region-after-copy t nil nil "standard Windows behavior.")
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c00320dfccb03311ca47cff0db3bed3cb05585eaa6663a9d28a824e03ab9d227" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" default))
 '(custom-theme-directory "/home/sboo/.emacs.d/sboo/themes/" nil nil "Personal themes.")
 '(dired-auto-revert-buffer t nil nil "Auto-Refresh.")
 '(dired-listing-switches
   "-l --recursive --almost-all --ignore-backups --human-readable --group-directories-first")
 '(dired-recursive-deletes 'top)
 '(dumb-jump-default-project "~/haskell")
 '(dumb-jump-selector 'helm)
 '(echo-keystrokes 1 nil nil "Wait this many seconds, then echo the currently-pressed key sub-sequence.")
 '(ediff-window-setup-function 'ediff-setup-windows-plain t)
 '(edit-indirect-guess-mode-function 'sboo-edit-indirect-guess-mode)
 '(enable-local-variables :safe nil nil "set only Safe Variables (don't query for unsafe ones).")
 '(enable-recursive-minibuffers t nil nil "so you can: ① press « M-x » within a « M-x »; ② search through (via a second « C-s ») the minibuffer of a search command for the (non-mini) buffer (having pressed the first « C-s »).")
 '(eshell-destroy-buffer-when-process-dies t t nil "“To get rid of those lingering buffers.”")
 '(eval-expression-print-length nil nil nil "‘nil’ means ‘eval-expression’ prints arbitrarily-long expressions.")
 '(eval-expression-print-level nil nil nil "‘nil’ means ‘eval-expression’ prints arbitrarily-deep expressions.")
 '(explicit-shell-file-name "/bin/bash")
 '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
 '(google-this-browse-url-function 'sboo-browse-uri-chrome)
 '(grep-find-template
   "find -L <D> <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} +" nil nil "`find-grep' for `nix'. « -L » traverses symlinks (emacs packages installed via « nix » are symlinks).")
 '(grep-first-column 1 t nil "One-based columns (≡ 1) or Zero-based columns (≡ 0).")
 '(grep-highlight-matches 'always nil nil "grep ‘--color=’ (`auto' or `always')")
 '(grep-save-buffers t nil nil "Don't ask (just save all buffers).")
 '(grep-scroll-output t nil nil "Jump to result (`point' at end of output window).")
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'cabal-new-repl)
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(helm-allow-mouse t)
 '(helm-autoresize-max-height 60)
 '(helm-autoresize-min-height 20)
 '(helm-boring-file-regexp-list
   '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$"))
 '(helm-buffers-fuzzy-matching t)
 '(helm-command-prefix-key "M-q")
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-echo-input-in-header-line t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-full-frame t)
 '(helm-mode-fuzzy-match t)
 '(helm-move-to-line-cycle-in-source nil)
 '(helm-register-max-offset 10000)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(helm-swoop-speed-or-color nil)
 '(helm-swoop-split-direction 'split-window-horizontally)
 '(helm-swoop-use-fuzzy-match t)
 '(help-at-pt-display-when-idle t nil (help-at-pt) "automatically show Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point) on point-over (i.e. when `point' moves there, not when the `cursor' hovers over).")
 '(help-at-pt-timer-delay 3 nil nil "Wait this many seconds before showing Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point)")
 '(history-length 10000 nil nil "increase Minibuffer History.")
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan nil)
 '(imenu-list-auto-resize nil t)
 '(imenu-list-focus-after-activation t t)
 '(imenu-list-position 'left t)
 '(indent-tabs-mode nil nil nil "no Extraneous Tabs.")
 '(inhibit-startup-screen t nil nil "fewer Startup Buffers.")
 '(initial-scratch-message nil nil nil "fewer Startup Buffers.")
 '(ispell-list-command "--list" t)
 '(ispell-personal-dictionary "~/configuration/data/aspell/en.pws")
 '(ispell-program-name "aspell")
 '(ispell-really-aspell t t)
 '(ispell-silently-savep t)
 '(kill-whole-line t nil nil "« C-k » eats newlines (a.k.a. it kills the whole line). By enabling `kill-whole-line', we can type just « C-k » where before we typed « C-k C-k ».")
 '(linum-format 'dynamic nil nil "")
 '(lisp-indent-function 'common-lisp-indent-function nil nil "format Property Lists correctly.")
 '(magit-save-repository-buffers 'dontask)
 '(markdown-command "multimarkdown")
 '(message-truncate-lines t t nil "don't resize Echo Area for long messages (instead, truncate the message).")
 '(minibuffer-depth-indicate-mode t nil nil "e.g. minibuffer displays « M-x [2] » when you've (often accidentally) double-« M-x »'d.")
 '(mode-require-final-newline nil nil nil "no Automatic Insertion of a Final Newline.")
 '(mouse-1-click-follows-link t nil nil "Left-Click a HyperLink to open it.")
 '(mouse-wheel-follow-mouse t nil nil "scroll Window under Mouse (by default, scroll `selected-window' always).")
 '(mouse-wheel-mode t nil nil "enable the Mouse-Wheel.")
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5)) nil nil "`1' means: Mouse-Wheel scrolls one line per flick (by default, it's 5).")
 '(mouse-yank-at-point t nil nil "don't move `point' when pasting with the mouse.")
 '(next-screen-context-lines 0 nil nil "")
 '(peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(print-length nil t nil "‘nil’ means print arbitrarily-long expressions.")
 '(print-level nil t nil "‘nil’ means print arbitrarily-deep expressions.")
 '(print-quoted t t nil "‘t’ means print « (quote foo) » as « 'foo » and « (function foo) » as « #'foo ».")
 '(recentf-max-menu-items 15)
 '(recentf-max-saved-items 1024)
 '(recentf-save-file "/home/sboo/.local/share/emacs/recentf/recentf.el")
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
 '(synosaurus-choose-method 'popup t)
 '(tool-bar-position 'left nil nil "the Tool Bar is on the left.")
 '(tool-bar-style 'both nil nil "each Icon of the Tool Bar has both Image (above) and Label (below).")
 '(truncate-lines nil nil nil "enable Continuation Lines.")
 '(undo-limit 20000000 nil nil "maximize Undo History.")
 '(undo-strong-limit 40000000 nil nil "maximize Undo History.")
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(url-configuration-directory "/home/sboo/.local/share/emacs/eww")
 '(use-dialog-box nil nil nil "`nil' replaces Dialog Boxes with `yes-or-no' prompts.")
 '(user-full-name "Spiros Boosalis" nil nil "")
 '(user-mail-address "SamBoosalis@gmail.com" nil nil "")
 '(vc-follow-symlinks t)
 '(vertical-scroll-bars 'right t nil "Enable and Position the Scrollbar.")
 '(view-read-only t nil nil "`t' means: always launch read-only buffers/files with `view-mode'.")
 '(visible-bell t nil nil "flash a Black Square onto the screen on User Errors (instead of honking loudly through the speakers).")
 '(wdired-allow-to-change-permissions t)
 '(wdired-allow-to-redirect-links t)
 '(wdired-use-dired-vertical-movement 'sometimes)
 '(which-key-idle-delay 1.0)
 '(which-key-idle-secondary-delay 0.25)
 '(x-underline-at-descent-line t nil nil "put the Underline below the Font Bottomline (instead of the Font Baseline).")
 '(yas-indent-line 'fixed)
 '(yas-new-snippet-default
   "# -*- mode: snippet -*-
#
# key         : $1
# name        : [sboo] a « $2 ».
#
# type        : snippet
# condition   : (let ((KEY ")
 '(yas-snippet-dirs '("/home/sboo/.emacs.d/sboo/snippets/"))
 '(yas-trigger-symbol "↣")
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(yas-field-highlight-face ((t (:inherit 'region :slant italic)))))
