;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250331.1412"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.2.0")
    (llama         "0.6.1")
    (magit-section "4.3.1")
    (seq           "2.24")
    (transient     "0.8.5")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "993cdf0aa4e4fee150aa583d937a9f4d2dde2928"
  :revdesc "993cdf0aa4e4"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
