;;; sboo-spellcheck.el --- Configure Spell-Checking -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 16 Jun 2019
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

;; Configure Spell-Checking via `flyspell' and `helm'.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'flyspell)
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-spellcheck nil

  "Customize Spell-Checking."

  :prefix 'sboo
  :group 'sboo)

;;==============================================;;

;;TODO: nix
" sboo.nix:

{
  services.dictd.enable = true;
}

 = with pkgs; [
  dict
  (with dictdDBs; [ 
    wiktionary 
    wordnet
  ])
];

$ sudo apt-get install dictd dict dict-{wn,vera,jargon,devil,gcide,foldoc}
$ sudo systemctl enable dictd
"

(use-package ispell

  )

;;TODO: nix

" sboo.nix:

{
  services.aseplld.enable = true;
}

 = with pkgs; [
  aspell
  (with aspellDicts; [ 
    en            # English
    en-computers  # (jargon)
    en-science    # (jargon)
    el            # Greek
  ])
];

$ cat << EOF >> ~/.aspell.conf

~~~~
master en_US
extra-dicts en-computers.rws
add-extra-dicts en_US-science.rws
~~~~

# check:

$ aspell -a
$ aspell dump config | grep -vE '^(#|$)'
"

(use-package flyspell

  )

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-spellcheck)

;;; sboo-spellcheck.el ends here