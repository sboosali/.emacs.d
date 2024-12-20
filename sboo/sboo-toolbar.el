;;; sboo-toolbar.el --- Personal Too-bar configuration -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 08 May 2019
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Customize the appearence/behavior of the Too-bar.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'tool-bar))

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  (defmacro sboo-toolbar (menu-items &optional toolbar-map)

    "toolbar-map.

Inputs:

• MENU-ITEMS — an (unquoted) symbol.
• TOOLBAR-MAP — a `keymapp' (unquoted).

Output:

• an expression.
  one `tool-bar-add-item' per `menu-item' in MENU-ITEMS.

Example:

• M-: (pp-macroexpand-expression (sboo-toolbar ()))
    ⇒ 

Related:

• `tool-bar-add-item'"

    (declare (indent 2))

    (let* ()
      `(with-demoted-errors "[Warning] %s"

         (list ,menu-items ,toolbar-map)))))

;;   (new-file menu-item
;;             "Visit New File..."
;;             find-file
;;             :label     "New File"
;;             :help      "Specify a new file's name, to edit the file"
;;             :enable    (menu-bar-non-minibuffer-window-p)
;;             :vert-only t

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-find-xpm-image (name)

  "Find « NAME.xmp », an X PixMap image.

Inputs:

• NAME — a `stringp'. the name of the image, relative to `image-load-path'. 

Output:

• a `plistp'.

Example:

• M-: (sboo-find-xpm-image \"mpc/play\")
    ⇒ '(image :type xpm :file \"/nix/store/*-emacs-26.1/share/emacs/26.1/etc/images/mpc/play.xpm\")

• M-: (sboo-find-xpm-image \"mpc/play.xpm\")
    ⇒ '(image :type xpm :file \"/nix/store/*-emacs-26.1/share/emacs/26.1/etc/images/mpc/play.xpm\")

Links:

• URL `'

Related:

• `find-image'"

  (let* ((NAME (format "%s.xpm"
                           (file-name-sans-extension name)))
         )

    (find-image `((:type xpm :file ,NAME)))))

;; M-: (sboo-find-xpm-image "mpc/play.xyz")

;; e.g.                  (file-name-sans-extension "mpc/play")
;; e.g.                  (file-name-sans-extension "mpc/play.xpm")
;; e.g. (format "%s.xpm" (file-name-sans-extension "mpc/play.xpm"))

;;----------------------------------------------;;

(defun sboo-pop-local-mark ()

  "Jump back to the last “position” (in the current buffer).

Related:

• `cua-set-mark'
• `pop-global-mark'"

  (let* (
         )
    (cua-set-mark 4)))

;; • (`cua-set-mark' 4) — 「C-u C-SPC」jumps back locally (in the buffer).
;; • `pop-global-mark' — 「C-x C-SPC」jumps back globally (across buffers).
;; • `push-mark' — 「C-SPC」`mark's the current `point'.

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-toolbar-setup ()

  "Extend `tool-bar-map'.

Additions of personal Toolbar-Items include: 

• “Compile” — runs `compile'.

Deletions of default Toolbar-Items include: 

• “Save” — manual saving is unnecessary; the
  Autosave Feature (via `sboo-autosave' / `auto-save-visited-mode')
  provides automatic saving."

  ;; Deletions:

  (let* ((MAP tool-bar-map)
         )

    ;; 'save

    MAP)

  ;; Modifications:

  (let* ((MAP tool-bar-map)
         )

    ;; 'isearch-forward
    ;; (defalias 'sboo-search #'helm-swoop)

    MAP)

  ;; Additions:

  (let* ((MAP tool-bar-map)
         )

      (tool-bar-local-item "mpc/play.pbm"
                           #'compile
                           'compile
                           MAP
                           :label "Compile"
                           :help "« M-x compile »"
                           :visible (sboo-toolbar-compile-visibile-p))

      (tool-bar-local-item "stop.xpm"
                           #'keyboard-quit
                           'stop
                           MAP
                           :label "Stop"
                           :help "« M-x keyboard-quit »"
                           :visible t)

      (tool-bar-local-item "browse-url.xpm"
                           #'helm-M-x
                           'M-x
                           MAP
                           :label "M-x"
                           :help "« M-x … »"
                           :visible t)

      ;; (tool-bar-local-item "spell" #'sboo-spellcheck)

      ;; (tool-bar-local-item "checkmark.xpm")

      ;; (tool-bar-local-item "important")

      ;; (tool-bar-local-item "run")

      ;; (tool-bar-local-item "")

      ;; (tool-bar-local-item "info")

      ;; (tool-bar-local-item "emacs.png")

      ;; (tool-bar-local-item "left-arrow"
      ;;                      #'pop-global-mark
      ;;                      'pop-global-mark
      ;;                      MAP
      ;;                      :label  "Go Back"
      ;;                      :help   "« M-x pop-global-mark »"
      ;;                      :rtl    "right-arrow"
      ;;                      )

      MAP))

;;----------------------------------------------;;

(defun sboo-toolbar-teardown ()

  "Invert `sboo-toolbar-setup' (restore `tool-bar-map')."

  'TODO)

;;----------------------------------------------;;

(defun sboo-toolbar-compile-visibile-p ()

  "Whether to enable the ‘compile’ Tool Item.

Enable the only for either: 

❶ non-default `compile-command's; or
❷ `prog-mode' Major Modes."

  (or (equal (default-value 'compile-command) (symbol-value 'compile-command))
      (derived-mode-p 'prog-mode)))

;;----------------------------------------------;;
;; Effects: ‘sboo-mode’ ------------------------;;
;;----------------------------------------------;;

;; (define-key global-map [tool-bar sboo-mode]

;;   '(menu-item "SBoo Mode"

;;               sboo-global-mode

;;               :help    "Toggle SBoo Mode (globally)."

;;               :image   sboo-image

;;               :button  (:toggle . sboo-global-mode)

;;    ;;         :visible (not (sboo-global-mode))

;;               :keys    "\\[sboo-global-mode]"))

;;----------------------------------------------;;
;; Effects -------------------------------------;;
;;----------------------------------------------;;

;; (define-key global-map [tool-bar sboo-mode]

;;   '(menu-item "SBoo Mode"

;;               sboo-global-mode

;;               :help    "Toggle SBoo Mode (globally)."

;;               :image   sboo-image

;;               :button  (:toggle . sboo-global-mode)

;;    ;;         :visible (not (sboo-global-mode))

;;     :keys    "\\[sboo-global-mode]"))

;; (progn
;;   (define-key global-map [tool-bar shell]
;;     '(menu-item "Shell" shell
;;       :image (image :type xpm :file "shell.xpm")))
;;   (define-key global-map [tool-bar S-shell] #'example/shell))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `tool-bar-local-item':
;; 
;;     (tool-bar-local-item ICON-STRING KEY-DEFINITION KEY-SYMBOL TOOLBAR-KEYMAP &key help enable rtl)
;;
;; > ICON is the base name of a file containing the image to use.  The
;; > function will first try to use low-color/ICON.xpm if ‘display-color-cells’
;; > is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
;; > ICON.xbm, using ‘find-image’.
;;
;; > KEY-DEFINITION is the key definition.
;; > KEY-SYMBOL is a symbol for the fake function key in the menu keymap.  
;;

;; e.g. `find-image':
;;
;; M-: (find-image '((:type xpm :file "mpc/play.xpm")))
;;   ⇒ '(image :type xpm :file "/nix/store/*-emacs-26.1/share/emacs/26.1/etc/images/mpc/play.xpm")
;;
;; PBM vs XBM vs XPM:
;;
;; • XPM (X PixMap) has superseded XBM (X BitMap).
;;

;; e.g. a `tool-bar-map' menu-item:
;;
;;   (new-file menu-item
;;             "Visit New File..."
;;             find-file
;;             :label     "New File"
;;             :help      "Specify a new file's name, to edit the file"
;;             :enable    (menu-bar-non-minibuffer-window-p)
;;             :vert-only t
;;             :image (find-image
;;                      (cond
;;                        ((not
;;                          (display-color-p))
;;                         '((:type pbm :file "new.pbm" :foreground "black" :background "grey75")
;;                           (:type xbm :file "new.xbm" :foreground "black" :background "grey75")
;;                           (:type xpm :file "low-color/new.xpm")
;;                           (:type xpm :file "new.xpm")))
;;                        ((<
;;                          (display-color-cells)
;;                          256)
;;                         '((:type xpm :file "low-color/new.xpm")
;;                           (:type xpm :file "new.xpm")
;;                           (:type pbm :file "new.pbm" :foreground "black" :background "grey75")
;;                           (:type xbm :file "new.xbm" :foreground "black" :background "grey75")))
;;                        (t
;;                         '((:type xpm :file "new.xpm")
;;                           (:type pbm :file "new.pbm" :foreground "black" :background "grey75")
;;                           (:type xbm :file "new.xbm" :foreground "black" :background "grey75"))))))
;;
;;

;; `tool-bar-add-item':
;;
;; M-: (tool-bar-add-item "mpc/play" #'compile 'compile :label "Compile" :help "Run « compile »")
;;
;; M-: (tool-bar-local-item "mpc/play" #'compile 'compile tool-bar-map :label "Compile" :help "Run « compile »" :enable t)
;;

;; "jump back":
;;
;; • (`cua-set-mark' 4) — 「C-u C-SPC」jumps back locally (in the buffer).
;; • `pop-global-mark' — 「C-x C-SPC」jumps back globally (across buffers).
;;

;; TODO
;;
;; Undo
;; ⤼
;;
;; Redo
;; ⤽
;;
;; Back
;; 🔙
;;
;; Top
;; 🔝
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-toolbar)

;;; sboo-toolbar.el ends here