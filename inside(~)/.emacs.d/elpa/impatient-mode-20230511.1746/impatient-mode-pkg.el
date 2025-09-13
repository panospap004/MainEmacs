;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "impatient-mode" "20230511.1746"
  "Serve buffers live over HTTP."
  '((emacs        "24.3")
    (simple-httpd "1.5.0")
    (htmlize      "1.40"))
  :url "https://github.com/netguy204/imp.el"
  :commit "a4e4e12852840996b027cb8e9fb2b809c37a0ee3"
  :revdesc "a4e4e1285284"
  :authors '(("Brian Taylor" . "el.wubo@gmail.com"))
  :maintainers '(("Brian Taylor" . "el.wubo@gmail.com")))
