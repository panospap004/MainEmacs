;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-projectile" "20230817.851"
  "Repository todo capture and management for org-mode with projectile."
  '((projectile           "2.3.0")
    (dash                 "2.10.0")
    (org-project-capture  "3.0.1")
    (org-category-capture "3.0.1"))
  :url "https://github.com/colonelpanic8/org-project-capture"
  :commit "4ca2667d498fa259772e46ff5e101285446d70b6"
  :revdesc "4ca2667d498f"
  :keywords '("org-mode" "projectile" "todo" "tools" "outlines" "project" "capture")
  :authors '(("Ivan Malison" . "IvanMalison@gmail.com"))
  :maintainers '(("Ivan Malison" . "IvanMalison@gmail.com")))
