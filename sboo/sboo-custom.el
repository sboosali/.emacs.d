(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-raise-tool-bar-buttons t t nil "visually raise a Tool Bar Item when the Mouse hovers over it.")
 '(auto-resize-tool-bars t t nil "do grow the Tool Bar when enough Menu Items are added.")
 '(comint-buffer-maximum-size 2000 nil nil "Increase.")
 '(cua-keep-region-after-copy t nil nil "standard Windows behavior.")
 '(custom-theme-directory "/home/sboo/.emacs.d/sboo/themes/" nil nil "Personal themes.")
 '(dired-auto-revert-buffer t nil nil "Auto-Refresh.")
 '(echo-keystrokes 1 nil nil "Wait this many seconds, then echo the currently-pressed key sub-sequence.")
 '(enable-local-variables :safe nil nil "set only Safe Variables (don't query for unsafe ones).")
 '(enable-recursive-minibuffers t nil nil "so you can: ① press « M-x » within a « M-x »; ② search through (via a second « C-s ») the minibuffer of a search command for the (non-mini) buffer (having pressed the first « C-s »).")
 '(eshell-destroy-buffer-when-process-dies t t nil "“To get rid of those lingering buffers.”")
 '(grep-find-template
   "find -L <D> <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} +" nil nil "`find-grep' for `nix'. « -L » traverses symlinks (emacs packages installed via « nix » are symlinks).")
 '(grep-first-column 1 t nil "One-based columns (≡ 1) or Zero-based columns (≡ 0).")
 '(grep-save-buffers t nil nil "Don't ask (just save all buffers).")
 '(grep-scroll-output t nil nil "Jump to result (`point' at end of output window).")
 '(help-at-pt-display-when-idle t nil (help-at-pt) "automatically show Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point) on point-over (i.e. when `point' moves there, not when the `cursor' hovers over).")
 '(help-at-pt-timer-delay 3 nil nil "Wait this many seconds before showing Local Help (i.e. the ‘kbd-help’ or ‘help-echo’ Text Property of the character-at-point)")
 '(history-length 10000 nil nil "increase Minibuffer History.")
 '(indent-tabs-mode nil nil nil "no Extraneous Tabs.")
 '(inhibit-startup-screen t nil nil "fewer Startup Buffers.")
 '(initial-scratch-message nil nil nil "fewer Startup Buffers.")
 '(kill-whole-line t nil nil "« C-k » eats newlines (a.k.a. it kills the whole line). By enabling `kill-whole-line', we can type just « C-k » where before we typed « C-k C-k ».")
 '(message-truncate-lines t t nil "don't resize Echo Area for long messages (instead, truncate the message).")
 '(minibuffer-depth-indicate-mode t nil nil "e.g. minibuffer displays « M-x [2] » when you've (often accidentally) double-« M-x »'d.")
 '(mode-require-final-newline nil nil nil "no Automatic Insertion of a Final Newline.")
 '(mouse-1-click-follows-link t nil nil "Left-Click a HyperLink to open it.")
 '(package-selected-packages
   (quote
    (all-the-icons yasnippet yaml-mode wrap-region window-purpose which-key webpaste volatile-highlights vlf telephone-line smooth-scrolling simpleclip shackle sed-mode rg rainbow-mode rainbow-blocks quack pinentry peep-dired pandoc-mode page-break-lines ov nix-mode neotree multi-term move-text modalka magit-annex lsp-ui lsp-haskell lcr json-mode intero imgbb hungry-delete highlight-quoted highlight-numbers highlight-escape-sequences helm-swoop helm-make helm-hayoo helm-dash expand-region epc elnode elfeed eimp edit-indirect direnv dired-sidebar dired-rainbow dired-open dired-filter dired-collapse diminish desktop-environment delight deft company-web company-restclient company-ghci company-ghc company-cabal company-anaconda color-theme centered-cursor-mode bm bind-key anzu)))
 '(redisplay-dont-pause t t nil "“Peeking” behavior when scrolling.")
 '(require-final-newline nil nil nil "no Automatic Insertion of a Final Newline.")
 '(ring-bell-function ignore nil nil "disable Blinking and Flashing.")
 '(scroll-preserve-screen-position 1 nil nil "")
 '(scroll-step 1 nil nil "")
 '(select-enable-clipboard t nil nil "Non-nil means: cutting and pasting uses the clipboard.")
 '(sentence-end-double-space nil nil nil "sentences end with a Single Period.")
 '(show-paren-delay 0 nil nil "no Delay.")
 '(tool-bar-style (quote both) nil nil "each Icon of the Tool Bar has both Image (above) and Label (below).")
 '(truncate-lines nil nil nil "enable Continuation Lines.")
 '(undo-limit 20000000 nil nil "maximize Undo History.")
 '(undo-strong-limit 40000000 nil nil "maximize Undo History.")
 '(use-dialog-box nil nil nil "`nil' replaces Dialog Boxes with `yes-or-no' prompts.")
 '(user-full-name "Spiros Boosalis" nil nil "")
 '(user-mail-address "SamBoosalis@gmail.com" nil nil "")
 '(visible-bell t nil nil "flash a Black Square onto the screen on User Errors (instead of honking loudly through the speakers).")
 '(x-underline-at-descent-line t nil nil "put the Underline below the Font Bottomline (instead of the Font Baseline)."))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(yas-field-highlight-face ((t (:inherit (quote region) :slant italic)))))
