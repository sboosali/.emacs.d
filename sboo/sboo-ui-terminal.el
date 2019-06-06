;;; sboo-ui-terminal.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
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

;; .
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins (internal):

;;----------------------------------------------;;

;; packages (external):

(eval-when-compile 
  (require 'use-package))

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

(use-package icons-in-terminal
  :load-path "submodules/icons-in-terminal"

  :config

  ())

;; ^ Links:
;;
;;   • URL `https://github.com/sebastiencs/icons-in-terminal'
;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; these Keysequences are identical (and thus, can't be bound separately)
;; in any (conformant) terminal emulator.
;;
;; • « C-i » — « TAB »
;; • « C-m » — « RET »
;; • « C-[ » — « ESC »
;; • « C-d » — « DEL »
;;
;; Links:
;;
;;   • URL `https://unix.stackexchange.com/questions/116629/how-do-keyboard-input-and-text-output-work'
;;   • URL `https://unix.stackexchange.com/questions/203418/bind-c-i-and-tab-keys-to-different-commands-in-terminal-applications-via-inputr'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-ui-terminal)

;;; sboo-ui-terminal.el ends here