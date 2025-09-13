;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "company-anaconda" "20230821.2126"
  "Anaconda backend for company-mode."
  '((emacs         "25.1")
    (company       "0.8.0")
    (anaconda-mode "0.1.1")
    (cl-lib        "0.5.0")
    (dash          "2.6.0")
    (s             "1.9"))
  :url "https://github.com/proofit404/anaconda-mode"
  :commit "14867265e474f7a919120bbac74870c3256cbacf"
  :revdesc "14867265e474"
  :keywords '("convenience" "company" "anaconda")
  :authors '(("Artem Malyshev" . "proofit404@gmail.com"))
  :maintainers '(("Artem Malyshev" . "proofit404@gmail.com")))
