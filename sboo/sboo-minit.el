;;; minit.el --- sboosali's “micro init.el” -*- coding: utf-8; lexical-binding: t -*-

;;; Code:

(defun sboo-find-sboo-file (file)
  "
M-: (sboo-find-sboo-file \"sboo-init-keybindings\")
  ; \"/home/sboosali/.emacs.d/sboo/sboo-init-keybindings.el\"
"
  (file-truename (concat (file-name-as-directory (concat (or user-emacs-directory "~/.emacs.d/") "sboo")) (concat file ".el"))))

(defun sboo-load-sboo-file (file)
  (load (sboo-find-sboo-file file) nil nil t t))

;;

(with-demoted-errors "[sboo core] %S"
  (sboo-load-sboo-file "sboo-utilities")
  (sboo-load-sboo-file "sboo-macros")
  (sboo-load-sboo-file "sboo-init-keybindings")
  (sboo-load-sboo-file "sboo-init-settings")
  (sboo-load-sboo-file "sboo-init-aliases")
  (sboo-load-sboo-file "sboo-custom")
  (sboo-load-sboo-file "sboo-commands")
  (sboo-load-sboo-file "sboo-dwim")
  ())

(with-demoted-errors "[sboo modal-configs] %S"
  (sboo-load-sboo-file "sboo-")
  ;; sboo-ui.el
  ;; sboo-ui-graphical.el
  ;; sboo-ui-terminal.el

  (when (sboo-load-sboo-file "sboo-ui-graphical")
    (sboo-ui-graphical-configure))

  ;; sboo-os.el
  ;; sboo-os-linux.el
  ;; sboo-os-macos.el
  ;; sboo-os-windows.el

  (when (sboo-load-sboo-file "sboo-os-windows")
    (sboo-windows-configure-modifiers))

  ;; sboo-arch.el
  ;; sboo-arch-arm-64.el
  ;; sboo-arch-intel-64.el

  ;; sboo-dev.el
  ;; sboo-dev-chromebook.el
  ;; sboo-dev-delltouch.el
  ;; sboo-dev-galaxy.el

  (when (sboo-load-sboo-file "sboo-dev-galaxy")
    (sboo-galaxy-configure))

  ())

(with-demoted-errors "[sboo builtin-packages] %S"

 ;; sboo-align.el
 ;; sboo-appearence.el
 ;; sboo-auto-mode.el
 ;; sboo-autosave.el
 ;; sboo-bookmark.el
 ;; sboo-buffers.el
 ;; sboo-case.el
 ;; sboo-clipboard.el
 ;; sboo-color.el
 ;; sboo-comment.el
 ;; sboo-compilation.el
 ;; sboo-completion.el
 ;; sboo-conditions.el
 ;; sboo-custom.el
 ;; sboo-definitions.el
 ;; sboo-desktop.el
 ;; sboo-dired.el
 ;; sboo-english.el
 ;; sboo-evil.el
 ;; sboo-faces.el
 ;; sboo-flycheck.el
 ;; sboo-fonts.el
 ;; sboo-frames.el
 ;; sboo-html.el
 ;; sboo-icons.el
 ;; sboo-install.el
 ;; sboo-inverse-theme.el
 ;; sboo-keymaps.el
 ;; sboo-kmacro.el
 ;; sboo-lisp.el
 ;; sboo-make.el
 ;; sboo-markdown.el
 ;; sboo-menubar.el
 ;; sboo-mode.el
 ;; sboo-path.el
 ;; sboo-private.el
 ;; sboo-prog.el
 ;; sboo-prose.el
 ;; sboo-rgb-mode.el
 ;; sboo-server.el
 ;; sboo-shell.el
 ;; sboo-spellcheck.el
 ;; sboo-sql.el
 ;; sboo-syntax.el
 ;; sboo-text.el
 ;; sboo-theme.el
 ;; sboo-toolbar.el
 ;; sboo-unicode.el
 ;; sboo-widgets.el

 (sboo-load-sboo-file "sboo-align")
 (sboo-load-sboo-file "sboo-appearence")
 (sboo-load-sboo-file "sboo-auto-mode")
 (sboo-load-sboo-file "sboo-autosave")
 (sboo-load-sboo-file "sboo-bookmark")
 (sboo-load-sboo-file "sboo-buffers")
 (sboo-load-sboo-file "sboo-case")
 (sboo-load-sboo-file "sboo-clipboard")
 (sboo-load-sboo-file "sboo-color")
 (sboo-load-sboo-file "sboo-comment")
 (sboo-load-sboo-file "sboo-compilation")
 (sboo-load-sboo-file "sboo-completion")
 (sboo-load-sboo-file "sboo-conditions")
 (sboo-load-sboo-file "sboo-custom")
 (sboo-load-sboo-file "sboo-definitions")
 (sboo-load-sboo-file "sboo-desktop")
 (sboo-load-sboo-file "sboo-dired")
 (sboo-load-sboo-file "sboo-english")
 (sboo-load-sboo-file "sboo-faces")
 (sboo-load-sboo-file "sboo-flycheck")
 (sboo-load-sboo-file "sboo-fonts")
 (sboo-load-sboo-file "sboo-frames")
 (sboo-load-sboo-file "sboo-html")
 (sboo-load-sboo-file "sboo-icons")
 (sboo-load-sboo-file "sboo-install")
 (sboo-load-sboo-file "sboo-inverse-theme")
 (sboo-load-sboo-file "sboo-keymaps")
 (sboo-load-sboo-file "sboo-kmacro")
 (sboo-load-sboo-file "sboo-lisp")
 (sboo-load-sboo-file "sboo-make")
 (sboo-load-sboo-file "sboo-markdown")
 (sboo-load-sboo-file "sboo-menubar")
 (sboo-load-sboo-file "sboo-mode")
 (sboo-load-sboo-file "sboo-path")
 (sboo-load-sboo-file "sboo-private")
 (sboo-load-sboo-file "sboo-prog")
 (sboo-load-sboo-file "sboo-prose")
 (sboo-load-sboo-file "sboo-rgb-mode")
 (sboo-load-sboo-file "sboo-server")
 (sboo-load-sboo-file "sboo-shell")
 (sboo-load-sboo-file "sboo-spellcheck")
 (sboo-load-sboo-file "sboo-sql")
 (sboo-load-sboo-file "sboo-syntax")
 (sboo-load-sboo-file "sboo-text")
 (sboo-load-sboo-file "sboo-theme")
 (sboo-load-sboo-file "sboo-toolbar")
 (sboo-load-sboo-file "sboo-unicode")
 (sboo-load-sboo-file "sboo-widgets")

 ())

(with-demoted-errors "[sboo vendored-packages] %S"
  (sboo-load-sboo-file "real-auto-save")

  ;; sboo-packages.el
  ;; sboo-packages-by-installing.el
  ;; sboo-packages-by-vendoring.el
  ())

(provide 'sboo-minit)