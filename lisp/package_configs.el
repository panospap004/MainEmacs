;;; package_configs.el --- Package configuration settings

;; ================================
;; Appearance Packages
;; ================================

;; Gruvbox Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name
  (doom-modeline-persp-icon t)) ;; Adds folder icon next to perspective name

;; ================================
;; Development Packages
;; ================================

;; Projectile – Project interaction library
(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1))))

;; Eglot – Language Server Protocol support (built-in in recent Emacs)
(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode lua-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil))

;; Yasnippet – Snippet system and collection
(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

;; ================================
;; Terminal Package
;; ================================

;; Eat – Terminal emulator inside Emacs
(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

;; ================================
;; Other Packages
;; ================================

;; Nerd Icons – Enhanced icons for UI elements
(use-package nerd-icons
  :if (display-graphic-p))
(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Git-related
(use-package magit
  :commands magit-status)
(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

;; Completion Framework: Corfu and companions
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-mode t)
  (corfu-popupinfo-delay 0.5)
  (corfu-separator ?\s)
  (completion-ignore-case t)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  :init
  (global-corfu-mode))
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Orderless – Flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Vertico and Marginalia – Vertical completion and annotations
(use-package vertico
  :init
  (vertico-mode))
(savehist-mode)
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

;; Consult – Enhanced searching and navigation
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

;; Diminish – Hide minor-mode lighters
(use-package diminish)

;; Rainbow Delimiters – Colorize matching brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-Key – Show available keybindings
(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-idle-delay 0.8)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil))

;;; package_configs.el ends here

(provide 'package_configs)
