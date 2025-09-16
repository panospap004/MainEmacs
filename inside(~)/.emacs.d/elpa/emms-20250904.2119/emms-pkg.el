;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "emms" "20250904.2119"
  "The Emacs Multimedia System."
  '((cl-lib  "0.5")
    (nadvice "0.3")
    (seq     "0"))
  :url "https://www.gnu.org/software/emms/"
  :commit "43b3204d22b0a80aeb278c70b36aea81cb77a880"
  :revdesc "43b3204d22b0"
  :keywords '("emms" "mp3" "ogg" "flac" "music" "mpeg" "video" "multimedia")
  :authors '(("Jorgen Schäfer" . "forcer@forcix.cx"))
  :maintainers '(("Yoni Rabkin" . "yrk@gnu.org")))
