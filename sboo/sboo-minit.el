;;; minit.el --- sboosali's “micro init.el” -*- coding: utf-8; lexical-binding: t -*-

;;; Code:

(defun sboo-find-sboo-path (&optional path)
  "
M-: (sboo-find-sboo-path \"lisp\")
  ; \"/home/sboosali/.emacs.d/sboo/lisp\""
  (file-truename (concat (file-name-as-directory (concat (or user-emacs-directory "~/.emacs.d/") "sboo"))
                         (concat path))))

(defun sboo-load-sboo-file (file)
  (load (sboo-find-sboo-path file) nil nil nil nil))

;;

(with-demoted-errors "[sboo core] %S"
  (add-to-list 'load-path (sboo-find-sboo-path) nil)

  (sboo-load-sboo-file "sboo-commands")
  (sboo-load-sboo-file "sboo-keymaps")

  (sboo-load-sboo-file "sboo-init-keybindings")
  (sboo-load-sboo-file "sboo-init-settings")
  (sboo-load-sboo-file "sboo-init-aliases")
  (sboo-load-sboo-file "sboo-custom")

  ;; (sboo-load-sboo-file "sboo-dwim")
  ;; (sboo-load-sboo-file "sboo-utilities")
  ;; (sboo-load-sboo-file "sboo-macros")
  ())

(with-demoted-errors "[sboo modal-configs] %S"

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
 (sboo-load-sboo-file "sboo-install")
 (sboo-load-sboo-file "sboo-inverse-theme")
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
 (sboo-load-sboo-file "sboo-xah")

 ())

(with-demoted-errors "[sboo vendored-packages] %S"
;;(add-to-list 'load-path (sboo-find-sboo-path "lisp") t)
  (sboo-load-sboo-file "real-auto-save")
  ())

(with-demoted-errors "[sboo installed-packages] %S"
  (when (sboo-load-sboo-file "sboo-packages-by-installing")
    (sboo-package-installables-configure)
    ;;(sboo-package-installables-initialize)
    (sboo-package-installables-initialize)
    ())
  ;; sboo-packages.el
  ;; sboo-packages-by-installing.el
  ;; sboo-packages-by-vendoring.el
  ())

(with-demoted-errors "[sboo] %S"
  (find-file (sboo-find-sboo-path "sboo-minit.el")))

(provide 'sboo-minit)