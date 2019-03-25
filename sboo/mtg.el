;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Completion for writing custom MagicTheGathering cards.
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl)
(require 'pcase)
(require 'json)

;; Internal:

(require 'mtg-data)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct (mtg-card
               (:constructor mtg-card-create)
               (:copier      nil))

  name cost types supertypes subtypes colors oracle power-toughness ;TODO power-toughness or strength
  cmc color-identity
  image flavor frame layout rarity typeline language artist
  rulings legality
  scryfall)

;; color cmc supertypes subtypes layout watermark collector language

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun mtg-insert-card (card)

  "Insert an MTG card name, from `mtg-card-names'."

  (interactive (list (mtg-read-card-name)))

  (insert card))                        ;TODO different styles, versions (old, new), name-only or full-text or image, surround with backticks, etc. 

;;----------------------------------------------;;

(cl-defun mtg-read-card-name (&key )

  "Read an MTG card name, from `mtg-card-names'."

  (interactive)

  (when (require 'mtg-data nil :no-error)

    (let ((PROMPT (format "%s: "
                          "Card name"))

          (PREDICATE     nil)
          (REQUIRE-MATCH t)
          (DEFAULT       nil)

          (CANDIDATES (mtg-card-names))
          )

      (let* ((STRING
              (let ((helm-mode-fuzzy-match nil))
                (completing-read PROMPT CANDIDATES PREDICATE REQUIRE-MATCH nil nil DEFAULT)))
             )

        STRING))))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 

;; M-: (progn (require 'json) (message "%S" (json-read-file "./colornames.json")))

;; e.g. « Ancestral Recall » (one-line):
;;
;; {"object":"card","id":"d392392e-6c36-44e4-a037-2b788a088891","oracle_id":"550c74d4-1fcb-406a-b02a-639a760a4380","multiverse_ids":[],"name":"Ancestral Recall","lang":"en","released_at":"2018-05-04","uri":"https://api.scryfall.com/cards/d392392e-6c36-44e4-a037-2b788a088891","scryfall_uri":"https://scryfall.com/card/ovnt/2018/ancestral-recall?utm_source=api","layout":"normal","highres_image":true,"image_uris":{"small":"https://img.scryfall.com/cards/small/en/ovnt/2018.jpg?1523193176","normal":"https://img.scryfall.com/cards/normal/en/ovnt/2018.jpg?1523193176","large":"https://img.scryfall.com/cards/large/en/ovnt/2018.jpg?1523193176","png":"https://img.scryfall.com/cards/png/en/ovnt/2018.png?1523193176","art_crop":"https://img.scryfall.com/cards/art_crop/en/ovnt/2018.jpg?1523193176","border_crop":"https://img.scryfall.com/cards/border_crop/en/ovnt/2018.jpg?1523193176"},"mana_cost":"{U}","cmc":1.0,"type_line":"Instant","oracle_text":"Target player draws three cards.","colors":["U"],"color_identity":["U"],"legalities":{"standard":"not_legal","future":"not_legal","frontier":"not_legal","modern":"not_legal","legacy":"banned","pauper":"not_legal","vintage":"restricted","penny":"not_legal","commander":"banned","duel":"banned","oldschool":"not_legal"},"games":[],"reserved":true,"foil":false,"nonfoil":true,"oversized":true,"promo":false,"reprint":true,"set":"ovnt","set_name":"Vintage Championship","set_uri":"https://api.scryfall.com/sets/c6a6b61b-143a-43f2-b74d-b140f3d93490","set_search_uri":"https://api.scryfall.com/cards/search?order=set\u0026q=e%3Aovnt\u0026unique=prints","scryfall_set_uri":"https://scryfall.com/sets/ovnt?utm_source=api","rulings_uri":"https://api.scryfall.com/cards/d392392e-6c36-44e4-a037-2b788a088891/rulings","prints_search_uri":"https://api.scryfall.com/cards/search?order=released\u0026q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380\u0026unique=prints","collector_number":"2018","digital":false,"rarity":"mythic","flavor_text":"2018 Europe Vintage Champion","illustration_id":"5c4784d2-425f-439a-b931-595096744d2c","artist":"Raoul Vitale","border_color":"black","frame":"2015","frame_effect":"","full_art":false,"story_spotlight":false,"edhrec_rank":16999,"related_uris":{"tcgplayer_decks":"https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall\u0026page=1\u0026partner=Scryfall\u0026utm_campaign=affiliate\u0026utm_medium=scryfall\u0026utm_source=scryfall","edhrec":"http://edhrec.com/route/?cc=Ancestral+Recall","mtgtop8":"http://mtgtop8.com/search?MD_check=1\u0026SB_check=1\u0026cards=Ancestral+Recall"}}
;;

;; e.g. « Ancestral Recall » (pretty-printed):
;;
;; {
;;     "object": "card",
;;     "id": "d392392e-6c36-44e4-a037-2b788a088891",
;;     "oracle_id": "550c74d4-1fcb-406a-b02a-639a760a4380",
;;     "multiverse_ids": [],
;;     "name": "Ancestral Recall",
;;     "lang": "en",
;;     "released_at": "2018-05-04",
;;     "uri": "https://api.scryfall.com/cards/d392392e-6c36-44e4-a037-2b788a088891",
;;     "scryfall_uri": "https://scryfall.com/card/ovnt/2018/ancestral-recall?utm_source=api",
;;     "layout": "normal",
;;     "highres_image": true,
;;     "image_uris": {
;;       "small": "https://img.scryfall.com/cards/small/en/ovnt/2018.jpg?1523193176",
;;       "normal": "https://img.scryfall.com/cards/normal/en/ovnt/2018.jpg?1523193176",
;;       "large": "https://img.scryfall.com/cards/large/en/ovnt/2018.jpg?1523193176",
;;       "png": "https://img.scryfall.com/cards/png/en/ovnt/2018.png?1523193176",
;;       "art_crop": "https://img.scryfall.com/cards/art_crop/en/ovnt/2018.jpg?1523193176",
;;       "border_crop": "https://img.scryfall.com/cards/border_crop/en/ovnt/2018.jpg?1523193176"
;;     },
;;     "mana_cost": "{U}",
;;     "cmc": 1,
;;     "type_line": "Instant",
;;     "oracle_text": "Target player draws three cards.",
;;     "colors": [
;;       "U"
;;     ],
;;     "color_identity": [
;;       "U"
;;     ],
;;     "legalities": {
;;       "standard": "not_legal",
;;       "future": "not_legal",
;;       "frontier": "not_legal",
;;       "modern": "not_legal",
;;       "legacy": "banned",
;;       "pauper": "not_legal",
;;       "vintage": "restricted",
;;       "penny": "not_legal",
;;       "commander": "banned",
;;       "duel": "banned",
;;       "oldschool": "not_legal"
;;     },
;;     "games": [],
;;     "reserved": true,
;;     "foil": false,
;;     "nonfoil": true,
;;     "oversized": true,
;;     "promo": false,
;;     "reprint": true,
;;     "set": "ovnt",
;;     "set_name": "Vintage Championship",
;;     "set_uri": "https://api.scryfall.com/sets/c6a6b61b-143a-43f2-b74d-b140f3d93490",
;;     "set_search_uri": "https://api.scryfall.com/cards/search?order=set&q=e%3Aovnt&unique=prints",
;;     "scryfall_set_uri": "https://scryfall.com/sets/ovnt?utm_source=api",
;;     "rulings_uri": "https://api.scryfall.com/cards/d392392e-6c36-44e4-a037-2b788a088891/rulings",
;;     "prints_search_uri": "https://api.scryfall.com/cards/search?order=released&q=oracleid%3A550c74d4-1fcb-406a-b02a-639a760a4380&unique=prints",
;;     "collector_number": "2018",
;;     "digital": false,
;;     "rarity": "mythic",
;;     "flavor_text": "2018 Europe Vintage Champion",
;;     "illustration_id": "5c4784d2-425f-439a-b931-595096744d2c",
;;     "artist": "Raoul Vitale",
;;     "border_color": "black",
;;     "frame": "2015",
;;     "frame_effect": "",
;;     "full_art": false,
;;     "story_spotlight": false,
;;     "edhrec_rank": 16999,
;;     "related_uris": {
;;       "tcgplayer_decks": "https://decks.tcgplayer.com/magic/deck/search?contains=Ancestral+Recall&page=1&partner=Scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall",
;;       "edhrec": "http://edhrec.com/route/?cc=Ancestral+Recall",
;;       "mtgtop8": "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Ancestral+Recall"
;;     }
;; }
;; 
;;
;; 
;;

;;TODO....
;;    "name"              : "Ancestral Recall",

;;     "lang"              : "en",

;;     "scryfall_uri"      : "https://scryfall.com/card/ovnt/2018/ancestral-recall?utm_source=api",

;; image_uris.png             : "https://img.scryfall.com/cards/png/en/ovnt/2018.png?1523193176",
;; image_uris.art_crop        : "https://img.scryfall.com/cards/art_crop/en/ovnt/2018.jpg?1523193176",

;;     "mana_cost"         : "{U}",

;;     "cmc"               : 1,

;;     "type_line"         : "Instant",

;;     "oracle_text"       : "Target player draws three cards.",

;;     "colors"            : [ "U" ],

;;     "color_identity"    : [ "U" ],

;;       "legalities.standard"        : "not_legal",
;;       "legalities.vintage"         : "restricted",

;;     "rulings_uri"       : "https://api.scryfall.com/cards/d392392e-6c36-44e4-a037-2b788a088891/rulings",

;;     "rarity"            : "mythic",

;;     "artist"            : "Raoul Vitale",

;;     "border_color"      : "black",

;;     "frame"             : "2015",

;;----------------------------------------------;;
(provide 'mtg)