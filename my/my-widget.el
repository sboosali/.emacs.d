(provide 'my-widget) 
(require 'my-svg)
;(require 'my-functions)
(require 'my-haskell)

;
;(global-set-key (kbd "<tool-bar> <open-file>") 'ido-find-file)
;(global-set-key (kbd "<menu-bar> <file> <open-file>") 'ido-find-file)

(setq tool-bar-position 'right) ;;TODO Doesn't work

; (tool-bar-add-item ICON FUNCTION BUTTON &rest PROPS)
(tool-bar-add-item "play" 'electric-buffer-list 'play-button)
(tool-bar-add-item "sort-criteria" 'flycheck-next-error 'error-button)
(tool-bar-add-item "refresh" 'refresh-iimages 'refresh-button)
(tool-bar-add-item "connect" 'compile-haskell 'run-button)
;; right-arrow up-node sort-descending

;; C-u C-x C-e (car image-load-path)
;; /nix/store/1cd096wyql5dylxxay257qa1118l3wag-emacs-25.1/share/emacs/25.1/etc/images/
;; attach.xpm
;; back-arrow.xpm
;; bookmark_add.xpm
;; cancel.xpm
;; checked.xpm
;; close.xpm
;; connect.xpm
;; contact.xpm
;; copy.xpm
;; cut.xpm
;; data-save.xpm
;; delete.xpm
;; describe.xpm
;; diropen.xpm
;; disconnect.xpm
;; exit.xpm
;; fwd-arrow.xpm
;; help.xpm
;; home.xpm
;; index.xpm
;; info.xpm
;; jump-to.xpm
;; left-arrow.xpm
;; letter.xpm
;; lock-broken.xpm
;; lock-ok.xpm
;; lock.xpm
;; mh-logo.xpm
;; new.xpm
;; next-node.xpm
;; next-page.xpm
;; open.xpm
;; paste.xpm
;; preferences.xpm
;; prev-node.xpm
;; print.xpm
;; redo.xpm
;; refresh.xpm
;; right-arrow.xpm
;; saveas.xpm
;; save.xpm
;; search-replace.xpm
;; search.xpm
;; separator.xpm
;; show.xpm
;; sort-ascending.xpm
;; sort-column-ascending.xpm
;; sort-criteria.xpm
;; sort-descending.xpm
;; sort-row-ascending.xpm
;; spell.xpm
;; splash.xpm
;; unchecked.xpm
;; undo.xpm
;; up-arrow.xpm
;; up-node.xpm
;; zoom-in.xpm
;; zoom-out.xpm


; view .xpm with image-toggle-display

;(global-set-key (kbd "<tool-bar> <run>") 'Doesnt-work)

;; "icon" ; seeks run.{xpm,pbm,xbm} in image-load-path

;; (tool-bar-add-item ICON DEF KEY &rest PROPS)

;; Add an item to the tool bar.
;; ICON names the image, DEF is the key definition and KEY is a symbol
;; for the fake function key in the menu keymap.  Remaining arguments
;; PROPS are additional items to add to the menu item specification.  See
;; Info node ‘(elisp)Tool Bar’.  Items are added from left to right.

;; ICON is the base name of a file containing the image to use.  The
;; function will first try to use low-color/ICON.xpm if ‘display-color-cells’
;; is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
;; ICON.xbm, using ‘find-image’.

