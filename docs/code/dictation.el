;;; dictation --- enable voice dictation within emacs

;; Copyright (C) 2000 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created:  7 Jan 2000
;; Version: 0.4
;; Keywords: comm
;; X-URL: http://www.gci-net.com/users/j/johnw/emacs.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this package, change to the buffer you want to enter voice
;; dictation in.  Then run `dictation-toggle'.  Now, whatever you
;; speak will appear in the buffer.  Use the word "newline" to
;; simulate pressing the RETURN key.

;;; History:

;; 2000-01-07:
;;    First pre-release, which works with only IBM's ViaVoice SDK for
;;    Linux.  In the future, support for multiple software packages on
;;    varying platforms would be good.

;;; Code:

(defconst dictation-version "0.4"
  "This version of dictation.")

(defgroup dictation nil
  "Enable voice dictation within Emacs."
  :group 'applications)

;;; User Variables:

(defcustom dictation-load-hook nil
  "*A hook that gets run after \"dictation.el\" has been loaded."
  :type 'hook
  :group 'dictation)

(defcustom dictation-debug nil
  "*Non-nil if the debugging code should be enabled."
  :type 'boolean
  :group 'dictation)

;;; Internal Variables:

(defvar dictation-buffer nil)
(defvar dictation-process nil)

;;; User Functions:

(defun dictation-sentinel (proc string))

(defun dictation-filter (proc string)
  (with-current-buffer dictation-buffer
    (if (string-match "<< Reco Text = '\\([^~]+\\)'$" string)
	(let ((chars (string-to-list (match-string 1 string))))
	  (while chars
	    (if (eq (car chars) ?\n)
		(call-interactively (key-binding [(control ?m)]))
	      (insert (car chars)))
	    (setq chars (cdr chars))))))
  (if dictation-debug
      (with-current-buffer (get-buffer "*ViaVoice output*")
	(insert string))))

(defun dictation-send-command (proc string)
  (process-send-string proc (concat string "\n"))
  (accept-process-output proc 1)
  (sit-for 1))

; Testing with fifth
;;;###autoload
(defun dictation-toggle ()
  (interactive)
  (if dictation-process
      (progn
	(dictation-send-command dictation-process "F")
	(dictation-send-command dictation-process "Q")
	(while (not (eq (process-status dictation-process) 'exit))
	  (accept-process-output dictation-process 1))
	(setq dictation-process nil)
	(message "Dictation is OFF"))
    (setq dictation-process
	  (start-process-shell-command
	   "viavoice" (get-buffer-create "*ViaVoice output*")
	   "/usr/lib/ViaVoice/samples/dwplus/runasc"))
    (setq dictation-buffer (current-buffer))
    (set-process-sentinel dictation-process 'dictation-sentinel)
    (set-process-filter dictation-process 'dictation-filter)
    (while (not (eq (process-status dictation-process) 'run))
      (accept-process-output dictation-process 1))
    (sit-for 1)
    (dictation-send-command dictation-process "N")
    (message "Dictation is ON")))

;;; Internal Functions:

(provide 'dictation)

(run-hooks 'dictation-load-hook)

;;; dictation.el ends here
