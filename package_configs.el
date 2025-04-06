(use-package gruvbox-theme)
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))
(use-package catppuccin-theme 
 :config
 (load-theme 'catppuccin t)
 (setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha, or 'frappe
 (catppuccin-reload))
(use-package doom-themes
  ;; :ensure t
  ;; :config
  ;; Global settings (defaults)
  ;; (load-theme 'doom-moonlight t)
)

;; (use-package doom-modeline
    ;;   :init (doom-modeline-mode 1)
    ;;   :custom
    ;;   (doom-modeline-height 25)
    ;;   (doom-modeline-bar-width 5)
    ;;   (doom-modeline-persp-name t)
    ;;   (doom-modeline-persp-icon t))
(use-package doom-modeline
  :ensure t
  :init
  ;; Basic settings
  (setq doom-modeline-height 25
        doom-modeline-bar-width 6
        doom-modeline-github nil  ;; Disable GitHub integration
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-time t  ;; Enable time display [[3]][[7]]
        doom-modeline-display-default-persp-name nil)

  ;; Git integration
  (setq doom-modeline-git t  ;; Show Git branch [[3]][[7]]
        doom-modeline-buffer-file-name-style 'relative)

  ;; Diagnostics (Flycheck)
  (setq doom-modeline-checker t)  ;; Show error/warning counts [[3]][[7]]

  ;; Copilot status segment
  (setq doom-modeline-additional-segments
        '(("copilot" (:eval (if (bound-and-true-p copilot-mode)
                                (propertize "  " 'face '(:foreground "#81B29A"))
                              (propertize "  " 'face '(:foreground "#81B29A")))))))

  :config
  (doom-modeline-mode 1)

  ;; Fix display issues [[4]][[8]]
  (setq-default mode-line-format
                (append mode-line-format
                        '((:eval (doom-modeline-format--main)))))

  ;; Font configuration (adjust based on your setup) [[5]][[9]]
  (when (display-graphic-p)
    (set-face-attribute 'doom-modeline-buffer-path nil :font "JetBrains Mono 10")
    (set-face-attribute 'doom-modeline-buffer-file nil :font "JetBrains Mono 10"))

  ;; Git diff counts (might need additional setup)
  (setq doom-modeline-git-show-details t)
  (setq doom-modeline-git-show-count t))

(use-package projectile
  :init (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1))))

(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode lua-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil))

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

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
  :init (global-corfu-mode))

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

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init (vertico-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode)
  :hook ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

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

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode 1)
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

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars t)
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("●" "○" "■" "●" "○" "■"))) ;; replace the * with this symbols
  (setq org-hide-emphasis-markers t);; hide the * + _ ~ etc when you use them
  (setq org-agenda-files '(
    "~/.config/MainEmacs/Files-org/TODO.org"
  ))
  (defun my/org-mode-header-font-setup () ;; the next 15 lines starting in this one make the headers larger
    "Configure fonts and sizes for Org mode headers."
(dolist (face-height '((org-level-1 . 1.2)
                     (org-level-2 . 1.1)
                     (org-level-3 . 1.05)
                     (org-level-4 . 1.0)
                     (org-level-5 . 1.1)
                     (org-level-6 . 1.1)
                     (org-level-7 . 1.1)
                     (org-level-8 . 1.1)))
            (set-face-attribute (car face-height) nil
                    :font "MonaspiceRn Nerd Font"
                    :weight 'bold
                    :height (cdr face-height))))

  (add-hook 'org-mode-hook #'my/org-mode-header-font-setup)

(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 120))  ; Adjust 80 to your preferred text width

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4)
  :hook (org-mode . org-indent-mode))

(with-eval-after-load 'org
  ;; No need for (require 'org-tempo) in Org 9.2+
  (add-to-list 'org-structure-template-alist '("ct" . "src shell"))
  (add-to-list 'org-structure-template-alist '("ce" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cp" . "src cpp")))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t) ;; C adds support for c cpp and d if you have its compiler
    )
  )
  (push '("conf-unix" . conf-unix) org-src-lang-modes) ;; this is to highlighte .conf unix files
)



(use-package dashboard
  :ensure t
  :config
  ;; Center dashboard content
  (setq dashboard-center-content t
        dashboard-vertically-center-content t)

  ;; Function to load a random ASCII banner from your ASCII.txt file
  (defun my/dashboard-set-random-banner ()
    "Set a random ASCII banner for dashboard from ASCII.txt."
    (let* ((ascii-file (expand-file-name "~/.config/MainEmacs/ASCII.txt"))
           (content (with-temp-buffer
                      (insert-file-contents ascii-file)
                      (buffer-string)))
           (banners (split-string content "\n---\n" t)))
      (when banners
        (let* ((banner (nth (random (length banners)) banners))
               (tmp-banner-file (make-temp-file "dashboard-banner-" nil ".txt")))
          (with-temp-file tmp-banner-file
            (insert banner))
          (setq dashboard-startup-banner tmp-banner-file)))))

  ;; Set banner font and prevent stretching
  (set-face-attribute 'dashboard-text-banner nil 
                      :family "Monospace") ; Maintain character proportions [[9]]

  ;; Add margin adjustment and recentering after banner insertion
  (defun my/adjust-banner-layout ()
    "Fix centering and margins after banner insertion."
    (setq-local left-margin-width 8)   ; Adjust based on your art's width [[5]]
    (setq-local right-margin-width 8)
    (recenter-top-bottom))             ; Force vertical recentering [[1]][[10]]

  ;; Hook layout adjustment after banner insertion
  (advice-add 'dashboard-insert-banner :after #'my/adjust-banner-layout)

  ;; Advise dashboard-insert-banner to run our randomization each time
  (advice-add 'dashboard-insert-banner :before #'my/dashboard-set-random-banner)

  ;; Set the banner title (separate from banner text)
  (setq dashboard-banner-logo-title "")

  ;; Define dashboard items
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)))

  ;; Set up the dashboard
  (dashboard-setup-startup-hook))

;; Ensure that when Emacs starts (or when using emacsclient without a file),
;; the dashboard is shown.
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;; Function to refresh the dashboard buffer in new frames.
(defun my/refresh-dashboard-on-new-frame (frame)
  "Refresh the dashboard buffer in FRAME if it's already open."
  (with-selected-frame frame
    (when (get-buffer dashboard-buffer-name)
      (with-current-buffer dashboard-buffer-name
        (dashboard-refresh-buffer)))))

;; Hook to refresh the dashboard when a new frame is created.
(add-hook 'after-make-frame-functions #'my/refresh-dashboard-on-new-frame)

;; Function to open the dashboard in new frames if the current buffer is *scratch*.
(defun my/open-dashboard-if-default-buffer (frame)
  "In FRAME, if the current buffer is *scratch* and no file is open, open the dashboard."
  (with-selected-frame frame
    (when (and (string= (buffer-name) "*scratch*")
               (not buffer-file-name))
      (dashboard-open))))

;; Hook to open the dashboard in new frames when appropriate.
(add-hook 'after-make-frame-functions #'my/open-dashboard-if-default-buffer)

;; undo-tree setup
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1) ;; Enable undo-tree globally
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.config/MainEmacs/undo-history/"))) ;; Set directory for undo history files
  (setq undo-tree-auto-save-history t)) ;; Auto-save undo history

;; undo-fu setup
(use-package undo-fu
  :ensure t
  :init
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo) ;; Example: Bind undo to 'u' in Evil mode
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)) ;; Example: Bind redo to 'Ctrl-r' in Evil mode

(provide 'package_configs)
