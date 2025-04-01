;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))
(use-package catppuccin-theme 
:config
(load-theme 'catppuccin t)
(setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha, or 'frappe
(catppuccin-reload))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 5)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t))

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
  (org-superstar-headline-bullets-list '("●" "○" "■" "●" "○" "■")))

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4)
  :hook (org-mode . org-indent-mode))

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
           ;; Split on lines that contain exactly three dashes
           (banners (split-string content "\n---\n" t)))
      (when banners
        (let* ((banner (nth (random (length banners)) banners))
               ;; Write banner to a temporary .txt file
               (tmp-banner-file (make-temp-file "dashboard-banner-" nil ".txt")))
          (with-temp-file tmp-banner-file
            (insert banner))
          ;; Set the dashboard banner to the temporary file
          (setq dashboard-startup-banner tmp-banner-file)))))

  ;; Advise dashboard-insert-banner to run our randomization each time
  (advice-add 'dashboard-insert-banner :before #'my/dashboard-set-random-banner)

  ;; Define dashboard items
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)))

  ;; Define custom shortcut keys
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")))

  ;; Function to insert a shortcut key before each section
  (defun my/dashboard-insert-shortcut (list-size list-display-name list-name)
    "Insert a shortcut key before each section."
    (let ((shortcut (cdr (assoc list-name dashboard-item-shortcuts))))
      (when shortcut
        (insert (format "[%s] %s\n" shortcut list-display-name)))))

  ;; Add the shortcut insertion function to the dashboard's item generators
  (setq dashboard-item-generators
        '((recents   . (lambda (list-size) (my/dashboard-insert-shortcut list-size "Recent Files" 'recents) (dashboard-insert-recents list-size)))
          (bookmarks . (lambda (list-size) (my/dashboard-insert-shortcut list-size "Bookmarks" 'bookmarks) (dashboard-insert-bookmarks list-size)))
          (projects  . (lambda (list-size) (my/dashboard-insert-shortcut list-size "Projects" 'projects) (dashboard-insert-projects list-size)))
          (agenda    . (lambda (list-size) (my/dashboard-insert-shortcut list-size "Agenda" 'agenda) (dashboard-insert-agenda list-size)))))

  ;; Set up dashboard on startup
  (dashboard-setup-startup-hook))

;; Ensure that when Emacs starts (or when using emacsclient without a file),
;; the dashboard is shown
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;; Function to refresh the dashboard buffer in new frames
(defun my/refresh-dashboard-on-new-frame (frame)
  "Refresh the dashboard buffer in FRAME if it's already open."
  (with-selected-frame frame
    (when (get-buffer dashboard-buffer-name)
      (with-current-buffer dashboard-buffer-name
        (dashboard-refresh-buffer)))))

;; Hook to refresh the dashboard when a new frame is created
(add-hook 'after-make-frame-functions #'my/refresh-dashboard-on-new-frame)

;; Function to open the dashboard in new frames if the current buffer is *scratch*
(defun my/open-dashboard-if-default-buffer (frame)
  "In FRAME, if the current buffer is *scratch* and no file is open, open the dashboard."
  (with-selected-frame frame
    (when (and (string= (buffer-name) "*scratch*")
               (not buffer-file-name))
      (dashboard-open))))

;; Hook to open the dashboard in new frames when appropriate
(add-hook 'after-make-frame-functions #'my/open-dashboard-if-default-buffer)

(provide 'package_configs)
