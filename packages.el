(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
        ("elpa"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil)
(use-package evil-collection)
(use-package general)

(use-package projectile)
(use-package eglot)
(use-package yasnippet-snippets)
(use-package eat)

(use-package org)
(use-package toc-org)
(use-package org-superstar)
(use-package org-tempo :ensure nil :after org)

(use-package nerd-icons)
(use-package nerd-icons-dired)
(use-package nerd-icons-ibuffer)
(use-package magit)
(use-package diff-hl)
(use-package doom-modeline)

(use-package corfu)
(use-package nerd-icons-corfu)
(use-package cape)
(use-package orderless)
(use-package vertico)
(use-package marginalia)
(use-package nerd-icons-completion)
(use-package consult)

(use-package diminish)
(use-package rainbow-delimiters)
(use-package which-key)

(provide 'packages)
