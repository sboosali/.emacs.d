# StdOut

## `emacs -batch`

```
$ emacs -batch  -f batch-byte-compile  -L ~/.emacs.d/sboo/*.el  ~/.emacs.d/sboo/*.el

Loading /nix/store/mf0fmp8khbp83f038wgcri7szf579giw-emacs-26.1/share/emacs/site-lisp/site-start.el (source)...

In toplevel form:

    ./sboo/sboo-aliases.el:21:1:Warning: cl package required at runtime
    ./sboo/sboo-aliases.el:27:1:Error: Cannot open load file: No such file or directory, sboo-utilities

In sboo-autosave-init!:

    ./sboo/sboo-autosave.el:12:9:Warning: `auto-save-visited-file-name'
    is an obsolete variable (as of Emacs 26.1); use `auto-save-visited-mode'
    instead.

In toplevel form:

    ./sboo/sboo-bookmark.el:21:1:Warning: cl package required at runtime

In end of data:

    ./sboo/sboo-bookmark.el:75:25:Warning: the function `sboo-xdg-data'
    is not known to be defined.

In toplevel form:

    ./sboo/sboo-commands.el:21:1:Error: Cannot open load file: No such file or directory, sboo-conditions

In toplevel form:

    ./sboo/sboo-company.el:16:7:Warning: assignment to free variable
    `sboo-company-backends'

In end of data:

    ./sboo/sboo-company.el:206:24:Warning: the following functions are
    not known to be defined: company-anaconda, tern-mode, company-tern

In toplevel form:

    ./sboo/sboo-compilation.el:21:1:Warning: cl package required at
    runtime

In toplevel form:

    ./sboo/sboo-completion.el:18:1:Warning: cl package required at
    runtime

In toplevel form:

    ./sboo/sboo-conditions.el:24:1:Warning: cl package required at
    runtime
    ./sboo/sboo-conditions.el:283:1:Warning: global/dynamic var
    `platform' lacks a prefix

In toplevel form:

    ./sboo/sboo-custom.el:1:1:Error: End of file during parsing

In toplevel form:

    ./sboo/sboo-definitions.el:11:1:Warning: cl package required at
    runtime
    ./sboo/sboo-definitions.el:69:1:Warning: Unused lexical variable
    `message'
Compiler-macro error for add-to-list: (wrong-number-of-arguments (3 . 5) 2)
    ./sboo/sboo-definitions.el:415:1:Warning: Unused lexical variable
    `Directory'

In add-to-theme-path!:

    ./sboo/sboo-definitions.el:437:4:Warning: add-to-list called with 1
    argument, but requires 2-4

In toplevel form:

    ./sboo/sboo-desktop.el:20:1:Warning: cl package required at runtime
    ./sboo/sboo-desktop.el:43:5:Warning: reference to free variable
    `emacs-directory'

In end of data:

    ./sboo/sboo-desktop.el:319:24:Warning: the function `sboo-xdg-data'
    is not known to be defined.

In toplevel form:

    ./sboo/sboo-dired.el:34:9:Warning: assignment to free variable
    `wdired-create-parent-directories'

In sboo/string->symbol:

    ./sboo/sboo-environment-variables.el:17:8:Warning: probable `"'
    without `\' in doc string of sboo/string->symbol
    ./sboo/sboo-environment-variables.el:38:4:Warning: misplaced
    interactive spec: `(interactive)'
    ./sboo/sboo-environment-variables.el:24:52:Warning: reference to free
    variable `unidiomatic'
    ./sboo/sboo-environment-variables.el:28:29:Warning: reference to free
    variable `foo-bar'
    ./sboo/sboo-environment-variables.el:31:29:Warning: reference to free
    variable `foo'
    ./sboo/sboo-environment-variables.el:31:34:Warning: reference to free
    variable `bar!'

In end of data:

    ./sboo/sboo-environment-variables.el:140:38:Warning: the function `,'
    is not known to be defined.

In toplevel form:

    ./sboo/sboo-evil.el:10:14:Warning: reference to free variable `evil'
    ./sboo/sboo-evil.el:13:9:Warning: assignment to free variable
    `evil-want-C-u-scroll'
    ./sboo/sboo-evil.el:14:9:Warning: assignment to free variable
    `evil-want-integration'
    ./sboo/sboo-evil.el:15:9:Warning: assignment to free variable
    `evil-want-Y-yank-to-eol'
    ./sboo/sboo-evil.el:16:9:Warning: assignment to free variable
    `evil-cross-lines'
    ./sboo/sboo-evil.el:17:9:Warning: assignment to free variable
    `evil-search-module'
    ./sboo/sboo-evil.el:18:9:Warning: assignment to free variable
    `evil-ex-search-case'
    ./sboo/sboo-evil.el:19:9:Warning: assignment to free variable
    `evil-ex-search-vim-style-regexp'
    ./sboo/sboo-evil.el:36:14:Warning: reference to free variable
    `evil-surround'

In end of data:

    ./sboo/sboo-evil.el:49:21:Warning: the following functions are not
    known to be defined: use-package, evil-mode, evil-insert-state,
    general-nmap, evil-window-split, evil-window-vsplit, evil-window-down,
    evil-window-up, evil-window-left, evil-window-right,
    evil-unimpaired/paste-above, evil-unimpaired/paste-below,
    global-evil-surround-mode

In toplevel form:

    ./sboo/sboo-fonts.el:3:1:Error: Cannot open load file: No such file or directory, sboo-utilities

In toplevel form:

    ./sboo/sboo-ghc.el:24:1:Warning: cl package required at runtime
    ./sboo/sboo-ghc.el:33:1:Error: Duplicate slots named • in sboo-haskell-symbols

In end of data:

    ./sboo/sboo-haskell.el:209:24:Warning: the following functions are
    not known to be defined: dante-type-at, dante-repl-by-file, dante-restart,
    dante-mode

In sboo-init-helm!:

    ./sboo/sboo-helm.el:16:12:Warning: assignment to free variable
    `helm-mode-fuzzy-match'
    ./sboo/sboo-helm.el:17:12:Warning: assignment to free variable
    `helm-completion-in-region-fuzzy-match'
    ./sboo/sboo-helm.el:28:12:Warning: assignment to free variable
    `helm-allow-mouse'

In end of data:

    ./sboo/sboo-helm.el:142:21:Warning: the function `helm-mode' is not
    known to be defined.

In toplevel form:

    ./sboo/sboo-init.el:22:1:Warning: cl package required at runtime
Compiler-macro error for add-to-list: (wrong-number-of-arguments (3 . 5) 2)
    ./sboo/sboo-init.el:397:1:Error: Wrong type argument: listp, sboo-insert-open-parenthesis

In toplevel form:

    ./sboo/sboo-init-helm.el:7:9:Warning: assignment to free variable
    `helm-mode-fuzzy-match'
    ./sboo/sboo-init-helm.el:8:9:Warning: assignment to free variable
    `helm-completion-in-region-fuzzy-match'
    ./sboo/sboo-init-helm.el:9:9:Warning: assignment to free variable
    `helm-allow-mouse'
    ./sboo/sboo-init-helm.el:11:9:Warning: assignment to free variable
    `helm-split-window-in-side-p'
    ./sboo/sboo-init-helm.el:12:9:Warning: assignment to free variable
    `helm-move-to-line-cycle-in-source'
    ./sboo/sboo-init-helm.el:13:9:Warning: assignment to free variable
    `helm-scroll-amount'
    ./sboo/sboo-init-helm.el:14:9:Warning: assignment to free variable
    `helm-ff-file-name-history-use-recentf'
    ./sboo/sboo-init-helm.el:15:9:Warning: assignment to free variable
    `helm-echo-input-in-header-line'
    ./sboo/sboo-init-helm.el:18:11:Warning: assignment to free variable
    `helm-google-suggest-use-curl-p'

In toplevel form:

    ./sboo/sboo-init-real-auto-save.el:7:7:Warning: assignment to free
    variable `real-auto-save-interval'
    ./sboo/sboo-init-real-auto-save.el:35:1:Warning: Unused lexical
    argument `PrefixArgument'

In sboo-init-real-auto-save-unload-function:

    ./sboo/sboo-init-real-auto-save.el:53:11:Warning: assignment to free
    variable `real-auto-save-interval'

In end of data:

    ./sboo/sboo-init-real-auto-save.el:95:1:Warning: the function
    `real-auto-save-mode' is not known to be defined.

In toplevel form:

    ./sboo/sboo-use-package-init-2.el:24:9:Warning: assignment to free
    variable `use-package-verbose'

In end of data:

    ./sboo/sboo-keybindings-2.el:529:30:Warning: the following functions
    are not known to be defined: sboo-launch-shell, sboo-launch-term,
    sboo-split-window-left-right, sboo-insert-angle-quote-left,
    sboo-insert-angle-quote-right, sboo-insert-triple-equals-sign,
    sboo-search, sboo-buffers-list, sboo-toggle-buffer, xah-prior-user-buffer,
    xah-next-user-buffer, sboo-find-file

In end of data:

    ./sboo/sboo-keybindings.el:468:28:Warning: the following functions
    are not known to be defined: flycheck-list-errors, helm-swoop,
    mark-whole-buffer-buffer, maximize-frame,
    sboo-kmacro-insert-counter-letter, sboo-split-window-left-right,
    sboo-launch-shell, sboo-launch-term, list-flycheck-errors,
    sboo-insert-angle-quote-left, sboo-insert-angle-quote-right,
    sboo-insert-dash, sboo-insert-triple-equals-sign, sboo-insert-null

In toplevel form:

    ./sboo/sboo-keymap.el:23:2:Warning: global-set-key called with 1
    argument, but requires 2
    ./sboo/sboo-keymap.el:29:1:Error: End of file during parsing

In toplevel form:

    ./sboo/sboo-packages-2.el:87:33:Warning: reference to free variable
    `sboo-cloned-package-directory'

In toplevel form:

    ./sboo/sboo-prog.el:50:1:Warning: defcustom for
    `sboo-comment-keywords' fails to specify type
    ./sboo/sboo-prog.el:50:1:Warning: defcustom for
    `sboo-comment-keywords' fails to specify containing group
    ./sboo/sboo-prog.el:50:1:Warning: defcustom for
    `sboo-comment-keywords' fails to specify type
    ./sboo/sboo-prog.el:50:1:Warning: defcustom for
    `sboo-comment-keywords' fails to specify containing group
    ./sboo/sboo-prog.el:63:1:Warning: defcustom for
    `sboo-comment-keywords-haskell' fails to specify type
    ./sboo/sboo-prog.el:63:1:Warning: defcustom for
    `sboo-comment-keywords-haskell' fails to specify containing group
    ./sboo/sboo-prog.el:63:1:Warning: defcustom for
    `sboo-comment-keywords-haskell' fails to specify type
    ./sboo/sboo-prog.el:63:1:Warning: defcustom for
    `sboo-comment-keywords-haskell' fails to specify containing group

In end of data:

    ./sboo/sboo-prog.el:158:21:Warning: the following functions are not
    known to be defined: s-wrap, s-join

In toplevel form:

    ./sboo/sboo-projectile.el:24:1:Warning: cl package required at
    runtime
    ./sboo/sboo-projectile.el:33:7:Warning: assignment to free variable
    `sboo-excluded--global--directories'
    ./sboo/sboo-projectile.el:34:7:Warning: assignment to free variable
    `sboo-excluded--global--file-extensions'
    ./sboo/sboo-projectile.el:35:7:Warning: assignment to free variable
    `sboo-excluded--global--file-names'
    ./sboo/sboo-projectile.el:39:7:Warning: assignment to free variable
    `sboo-excluded--haskell--directories'
    ./sboo/sboo-projectile.el:40:7:Warning: assignment to free variable
    `sboo-excluded--haskell--file-extensions'
    ./sboo/sboo-projectile.el:41:7:Warning: assignment to free variable
    `sboo-excluded--haskell--file-names'
    ./sboo/sboo-projectile.el:45:7:Warning: assignment to free variable
    `sboo-excluded--emacs--directories'
    ./sboo/sboo-projectile.el:46:7:Warning: assignment to free variable
    `sboo-excluded--emacs--file-extensions'
    ./sboo/sboo-projectile.el:47:7:Warning: assignment to free variable
    `sboo-excluded--emacs--file-names'
    ./sboo/sboo-projectile.el:53:7:Warning: assignment to free variable
    `sboo-excluded--nix--directories'
    ./sboo/sboo-projectile.el:54:7:Warning: assignment to free variable
    `sboo-excluded--nix--file-extensions'
    ./sboo/sboo-projectile.el:55:7:Warning: assignment to free variable
    `sboo-excluded--nix--file-names'
    ./sboo/sboo-projectile.el:64:9:Warning: reference to free variable
    `sboo-excluded--global--directories'
    ./sboo/sboo-projectile.el:65:9:Warning: reference to free variable
    `sboo-excluded--haskell--directories'
    ./sboo/sboo-projectile.el:66:9:Warning: reference to free variable
    `sboo-excluded--emacs--directories'
    ./sboo/sboo-projectile.el:67:9:Warning: reference to free variable
    `sboo-excluded--nix--directories'
    ./sboo/sboo-projectile.el:76:9:Warning: reference to free variable
    `sboo-excluded--global--file-names'
    ./sboo/sboo-projectile.el:77:9:Warning: reference to free variable
    `sboo-excluded--haskell--file-names'
    ./sboo/sboo-projectile.el:78:9:Warning: reference to free variable
    `sboo-excluded--emacs--file-names'
    ./sboo/sboo-projectile.el:79:9:Warning: reference to free variable
    `sboo-excluded--nix--file-names'
    ./sboo/sboo-projectile.el:88:9:Warning: reference to free variable
    `sboo-excluded--global--file-extensions'
    ./sboo/sboo-projectile.el:89:9:Warning: reference to free variable
    `sboo-excluded--haskell--file-extensions'
    ./sboo/sboo-projectile.el:90:9:Warning: reference to free variable
    `sboo-excluded--emacs--file-extensions'
    ./sboo/sboo-projectile.el:91:9:Warning: reference to free variable
    `sboo-excluded--nix--file-extensions'

In toplevel form:

    ./sboo/sboo-settings.el:32:7:Warning: assignment to free variable
    `cua-keep-region-after-copy'
    ./sboo/sboo-settings.el:63:7:Warning: assignment to free variable
    `show-paren-delay'
    ./sboo/sboo-settings.el:64:7:Warning: assignment to free variable
    `show-paren-style'
    ./sboo/sboo-settings.el:74:7:Warning: `redisplay-dont-pause' is an
    obsolete variable (as of 24.5).
    ./sboo/sboo-settings.el:180:7:Warning: assignment to free variable
    `ediff-window-setup-function'

In sboo-proced-settings:

    ./sboo/sboo-settings.el:224:9:Warning: assignment to free variable
    `proced-auto-update-interval'

In end of data:

    ./sboo/sboo-settings.el:298:25:Warning: the function
    `proced-toggle-auto-update' is not known to be defined.

In toplevel form:

    ./sboo/sboo-theme.el:65:1:Warning: defgroup for `sboo-group' fails to
    specify containing group

In toplevel form:

    ./sboo/sboo-unicode.el:3:1:Warning: cl package required at runtime

In toplevel form:

    ./sboo/sboo-utilities.el:21:1:Warning: cl package required at runtime

In get-last-message:

    ./sboo/sboo-utilities.el:120:8:Warning: Use `with-current-buffer'
    rather than save-excursion+set-buffer
    ./sboo/sboo-utilities.el:140:1:Warning: Unused lexical argument
    `SUFFIX'

In toplevel form:

    ./sboo/sboo-xdg.el:24:1:Warning: cl package required at runtime

In end of data:

    ./sboo/sboo-yasnippets.el:50:27:Warning: the following functions are
    not known to be defined: yas-recompile-all, yas-reload-all

```

##

