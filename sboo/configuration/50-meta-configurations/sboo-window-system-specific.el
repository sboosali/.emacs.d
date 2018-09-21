;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sboo-gui-only
  :if     window-system)
(use-package sboo-cli-only
  :unless window-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes

;; ^ 
;; 
;; > Lisp programs can condition on `display-*-p` functions adopt their behavior to the display's capabilities.
;; > For example, a program that would use a popup-menu, could instead use the minibuffer when popup-menus are not supported.
;; > The optional argument display in these functions specifies which display to ask the question about. It can be a display name, a frame (which designates the display that frame is on), or nil (which refers to the selected frame's display, see Input Focus).
;;
;; NOTE:
;; > Do not use window-system and initial-window-system as predicates or boolean flag variables, if you want to write code that works differently on text terminals and graphic displays. That is because window-system is not a good indicator of Emacs capabilities on a given display type. Instead, use display-graphic-p or any of the other display-*-p predicates

;; ------------------------------
;; `display-graphic-p`
;; Whether the display is a graphic display capable of displaying several frames and several different fonts at once.
;; This is true for displays that use a window system such as X, and false for text terminals.
;; ---
;; `display-mouse-p`
;; Whether the display has a mouse available.
;; ---
;; `display-color-p`
;; Whether the screen is a color screen.
;; ---
;; `display-selections-p`
;; Whether the display supports selections. Windowed displays normally support selections, but they may also be supported in some other cases.
;; ------------------------------
;; `x-server-version`
;; This function returns the list of version numbers of the GUI window system running on display, such as the X server on GNU and Unix systems. The value is a list of three integers: the major and minor version numbers of the protocol, and the distributor-specific release number of the window system software itself.
;; On GNU and Unix systems, these are normally the version of the X protocol and the distributor-specific release number of the X server software. On MS-Windows, this is the version of the Windows OS.
;; ---
;; `display-mm-height` / `display-mm-width`
;; This function returns the height/width of the screen in millimeters, or nil if Emacs cannot get that information.
;; For graphical terminals, note that on multi-monitor setups this refers to the height/width for all physical monitors associated with display. 
;; ---
;; `display-mm-dimensions-alist`
;; This variable allows the user to specify the dimensions of graphical displays returned by display-mm-height and display-mm-width in case the system provides incorrect values.
;; ---
;; `display-screens`
;; This function returns the number of screens associated with the display.
;; ------------------------------

;; See
;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
;;     - https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-window-system-specific)