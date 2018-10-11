;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ToolBar Widget Configuration
;;
;; (e.g. the icons/labels of "Open", "Copy", "Paste", "Undo", etc)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ToolBar Settings

(setq tool-bar-style 'both)

;; ^ `tool-bar-style':
;;
;; - `text` means "show text only";
;; - `both` means "show both image and text, with text below image"
;; - `text-image-horiz` means "show both image and text, with any text to the left of the image";
;;
;; NOTE but only `both` does show both;
;; the others (e.g. `text-image-horiz`, `both-horiz`, etc) show text-only for most buttons.
;;

(setq auto-resize-tool-bars t)
;; ^ `auto-resize-tool-bars':
;;
;; If this variable is non-nil, the tool bar automatically resizes to show all defined tool bar items (but not larger than a quarter of the frame's height).

(setq
 auto-raise-tool-bar-buttons t)
;; ^ `auto-raise-tool-bar-buttons':
;;
;; If this variable is non-nil, tool bar items display in raised form when the mouse moves over them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ToolBar Items

(tool-bar-add-item "play" 'compile 'play-button)

;; ^ `tool-bar-add-item':
;;
;; (tool-bar-add-item ICON FUNCTION BUTTON &rest PROPS)
;;
;; `tool-bar-add-item` extends `tool-bar-map`,
;; kinda like `add-to-list`, but more like `global-set-key`.
;;

;; Adding a ToolBarItem
;; 
;; TODO-doesnt-work
;; e.g. 
;; ---
;;     (define-key global-map [tool-bar shell] '(menu-item "Shell" shell :image (image :type xpm :file "shell.xpm")))
;; ---

;; Removing a ToolBarItem
;; 
;; [1] Press `C-h k`.
;;
;;     (a.k.a. `M-x describe-key`)
;;
;; [2] Click on the ToolBarItem to be removed.
;;
;;     (e.g. the one labeled "Open Directory")
;;
;; [3] Call `global-unset-key` on that ToolBarItem's name, under the `tool-bar` (pseudo-)keymap.
;;
;;     e.g. Since `describe-key` identified the "Open Directory" button as `"<tool-bar> <dired>"`, call:
;;     ---
;;        M-: (global-unset-key [tool-bar dired])
;;     ---
;;     Or if you're reading this within Emacs, place the cursor at the closing parenthesis of the expression above,
;;     and type `C-x C-e` (a.k.a `eval-last-sexp`).
;;     If the ToolBarItem doesn't immediately disappears, try pressing `C-l`, or just `RET` in a buffer.
;;     If the ToolBarItem reappears (e.g. after a `switch-buffer`), then TODO.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ^ 
;; (tool-bar-add-item ICON FUNCTION BUTTON &rest PROPS)
;; 
;; ICON :: String
;; e.g. 
;; 
;; FUNCTION :: (Symbol | () -> IO ())
;; e.g. 
;; 
;; BUTTON :: Symbol
;; e.g. 
;; 
;; PROPS :: 
;; e.g. 
;;
;;

;; `tool-bar-map`
;;
;;  (keymap
;;    ( new-file
;;      menu-item
;;      "Visit New File..."
;;      find-file
;;      :label              "New File"
;;      :vert-only          t
;;      :enable             #6=(menu-bar-non-minibuffer-window-p)
;;      :help               "Specify a new file's name, to edit the file"
;;      :image              (find-image
;;                            (cond
;;                             (#7=(not (display-color-p))
;;                                 '(#4=(:type pbm :file "new.pbm" . #1=(:foreground #8="black" :background #9="grey75"))
;;                                      #5=(:type xbm :file "new.xbm" . #1#)
;;                                      #2=(:type xpm :file "low-color/new.xpm")
;;                                      #3=(:type xpm :file "new.xpm")))
;;                             (#11=(< (display-color-cells) 256)
;;                                  '(#2# #3# #4# #5#))
;;                             (t
;;                              '(#3# #4# #5#)))))
;;    ( open-file
;;      ... ))
;;
;; 

;; tool-bar-style
;;
;; `nil` by default
;;
;; an enum (symbols), one of:
;;
;; - image            - show images only
;; - text             - show text only
;; - both             - show both, text below image
;; - both-horiz       - show text to the right of the image
;; - text-image-horiz - show text to the left of the image
;; - any other        - use system default or image if no system default.
;;
;; only affects GTK+ Emacs.

;; 
;; C-u C-x C-e (car image-load-path)
;; /nix/store/1cd096wyql5dylxxay257qa1118l3wag-emacs-25.1/share/emacs/25.1/etc/images/
;;
;; $ find ___/emacs-26.1/share/emacs/26.1/etc/images/
;; ...
;; ./play.xpm
;; ...
;;
;; 
;;

;; Modifers & ToolBarItems:
;;
;; You can define a special meaning for clicking on a tool bar item with the shift, control, meta, etc., modifiers. You do this by setting up additional items that relate to the original item through the fake function keys.
;; For example, if the original item was defined this way,
;; ---
;;      (define-key global-map [tool-bar shell]
;;        '(menu-item "Shell" shell
;;                    :image (image :type xpm :file "shell.xpm")))
;; ---
;; then here is how you can define clicking on the same tool bar image with the shift modifier:
;; ---
;;      (define-key global-map [tool-bar S-shell] 'some-command)
;; ---

;; Links:
;;
;; See
;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Tool-Bar.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-widgets)