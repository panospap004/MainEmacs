(use-package gruvbox-theme)
;;   :config
;; (load-theme 'gruvbox-dark-medium t)
(use-package catppuccin-theme 
	:config
	(load-theme 'catppuccin t)
	(setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha, or 'frappe
	(catppuccin-reload)
	)
(use-package doom-themes
  ;; :ensure t
  ;; :config
	;; (load-theme 'doom-monet t)

  ;; Global settings (defaults)
	;; (load-theme 'doom-nord-aurora t)
	;; (load-theme 'doom-rouge t)
	;; (load-theme 'doom-monokai-octagon t)
	;; (load-theme 'doom-ephemeral t)
	;; (load-theme 'doom-monokai-machine t)
  ;; (load-theme 'doom-moonlight t)
	)

(add-to-list 'custom-theme-load-path "~/.config/MainEmacs/theme/")
;; (load-theme 'monet t)

;; keycast
;; (use-package keycast
;;   :ensure t
;;   :config
;; 	(setq keycast-mode-line-mode t
;;         keycast-header-line-mode nil
;;         keycast-tab-bar-mode nil
;;         keycast-log-mode nil)
;; )

(use-package doom-modeline
  :ensure t
  :init
	(setq display-time-24hr-format   t       
				display-time-day-and-date nil     
				display-time-format "%H:%M:%S"
				display-time-interval 1)
	(display-time-mode 1)
	(display-battery-mode 1)
	(line-number-mode    1)
	(column-number-mode  1)
  ;; Basic settings
  (setq doom-modeline-height 25
        doom-modeline-bar-width 6
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-display-default-persp-name nil)
  
  ;; Git integration
  (setq doom-modeline-git t
        doom-modeline-buffer-file-name-style 'relative)
  
  ;; Diagnostics (Flycheck)
  (setq doom-modeline-checker t)

	(setq display-time-load-average nil)
	(setq doom-modeline-buffer-encoding nil)
  
	;; ;; 1) Define Copilot segment with your glyphs

  :config
	;; (Optionally) enable imenu support to find our def-modeline forms:
  (setq doom-modeline-support-imenu t)

	;; ------------------------------------------------------------------------------
	;; 1. Define custom segments for “last key pressed”, Copilot and Codeium status.
	;; ------------------------------------------------------------------------------
	;; We'll record the last key pressed via a hook, and display it in 'last-key'.
	(defvar my/last-key "" 
		"Last key sequence pressed in a human-readable form.")
	(defun my-record-last-key ()
		"Record the last key sequence into `my/last-key`."
		(setq my/last-key (key-description (this-single-command-keys))))
	(add-hook 'post-command-hook #'my-record-last-key)

	(doom-modeline-def-segment last-key
		"Show the most recent key sequence pressed."
		(propertize (format " %s" (or my/last-key ""))
								'face 'doom-modeline-highlight))

	(doom-modeline-def-segment copilot-icon
    (let* ((on?   (bound-and-true-p copilot-mode))
           (icon  "")    ;; your Copilot glyph
           (face  `(:foreground ,(if on? "#A6DA95" "#ED8796"))))
      (propertize (concat " " icon) 'face face)))

  ;; 2) Custom Codeium icon: same idea
  (doom-modeline-def-segment codeium-icon
    (let* ((on?   (bound-and-true-p codeium-overlay-mode))
           (icon  "󰘦")   ;; your Codeium glyph
           (face  `(:foreground ,(if on? "#A6DA95" "#ED8796"))))
      (propertize (concat " " icon) 'face face)))	

	;; Define faces that only set the foreground color:
	(defface my-vcs-added-face
		'((t :foreground "#A6DA95"))
		"Face for added lines in modeline.")

	(defface my-vcs-modified-face
		'((t :foreground "#8AADF4"))
		"Face for modified lines in modeline.")

	(defface my-vcs-removed-face
		'((t :foreground "#ED8796"))
		"Face for removed lines in modeline.")

	;; Make sure diff-hl is loaded
	(require 'diff-hl)
	(require 'cl-lib)

	;; Doom Modeline segment for Git diffs:
	(doom-modeline-def-segment vcs-diff
		"Show per-file Git diff stats (added, modified, deleted) with colored icons/text."
		(when (and buffer-file-name
							 (eq (vc-backend buffer-file-name) 'Git)
							 diff-hl-mode)
			(let* ((changes  (diff-hl-changes))
						 ;; Sum up lengths; empty list → 0
						 (added    (cl-loop for (_line len type) in changes
																when (eq type 'insert) sum len))
						 (deleted  (cl-loop for (_line len type) in changes
																when (eq type 'delete) sum len))
						 (modified (cl-loop for (_line len type) in changes
																when (eq type 'change) sum len)))
				(concat
				 ;; Added
				 (propertize (format "  %d" added)
										 'face 'my-vcs-added-face)
				 ""
				 ;; Modified
				 (propertize (format "  %d" modified)
										 'face 'my-vcs-modified-face)
				 ""
				 ;; Deleted
				 (propertize (format "  %d" deleted)
										 'face 'my-vcs-removed-face)
				 ""
				 ))))

	;; ----------------------------------------------------------------------
	;; 2. Define the custom modeline layout.
	;; ----------------------------------------------------------------------
	;; We use `doom-modeline-def-modeline` with a new name (here 'my-line').
	;; First list = left segments; second list = right segments.
	(doom-modeline-def-modeline 'my-line
		;; Left-hand segments:
		'(bar                             ; the bar (window-probe indicator)
			window-state                    ; e.g. maximize/minimize icons
			workspace-name                  ; Eyebrowse or Tab workspace name
			window-number                   ; current window number
			modals                          ; Evil/overwrite/ryo/etc state
			matches                         ; isearch / query-replace match count
			buffer-info                     ; buffer icon & name & modified flag
			major-mode                      ; major mode name (with icon if any)
			vcs                             ; VC branch (Git branch), with status
			check                         ; error/warning count (flycheck/flymake)
			debug                           ; debug state (DAP/GUD/Edebug indicator)
			remote-host                     ; remote host (if remote file)
			;; Keep selection and word-count segments for completeness:
			word-count                      ; word count (if selection or mode)
			parrot                          ; animated parrot (fun!)
			selection-info)                ; X/Y selection info
		;; Right-hand segments:
		'(misc-info                      ; various info (anzu, etc.)
			project-name                   ; project (if any)
			persp-name                     ; perspective name (if any)
			last-key                       ; our custom “last key pressed”
			copilot-icon                   ; our custom Copilot status
      codeium-icon
			vcs-diff
			lsp                            ; LSP server(s) active
			buffer-position                ; cursor position (line:col)
			battery                        ; battery %
			time                           ; current time (updates every sec)
			;; The rest of the default segments:
			grip                           ; GitHub README preview (optional)
			irc mu4e gnus                  ; email/IRC notifications
			github                         ; GitHub notifications
			minor-modes                    ; minor mode icons (if enabled)
			input-method                   ; input method (e.g. 🈯 for Japanese)
			indent-info                    ; indent style/size
			buffer-encoding                ; file encoding/EOL
			process))                      ; process indicator in mode-line

  ;; 2. Clear out all mode-specific modelines (every buffer uses default/main)
  (setq doom-modeline-mode-alist nil)

  ;; 3. When doom-modeline-mode turns on, set our ‘my-line’ as the default layout
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'my-line 'default)))

  ;; 4. Refresh mode-line in all buffers to apply immediately
  (force-mode-line-update t)

	;; ----------------------------------------------------------------------
	;; 4. Essential: enable Doom Modeline.
	;; ----------------------------------------------------------------------
	(doom-modeline-mode 1)
  
  ;; Fix display issues
  ;; (setq-default mode-line-format
  ;;               (append mode-line-format
  ;;                       '((:eval (doom-modeline-format--main)))))
  
  ;; Font configuration (adjust based on your setup)
  (when (display-graphic-p)
    (set-face-attribute 'doom-modeline-buffer-path nil :font "JetBrains Mono 10")
    (set-face-attribute 'doom-modeline-buffer-file nil :font "JetBrains Mono 10"))
  
	(with-eval-after-load 'company
		(add-to-list 'global-mode-string 'company-lighter))
  (setq visual-replace-display-total t)
  ;; Git diff counts (might need additional setup)
  (setq doom-modeline-git-show-details t)
  (setq doom-modeline-git-show-count t)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-time-analogue-clock t)
	;; set load avarage time to nil
	(setq display-time-load-average nil)

  (setq doom-modeline-load-avr-time nil)
	;; ;; The scaling factor used when drawing the analogue clock.
	;; (setq doom-modeline-time-clock-size 0.7)
	
	;; ;; If non-nil, cause imenu to see `doom-modeline' declarations.
	;; ;; This is done by adjusting `lisp-imenu-generic-expression' to
	;; ;; include support for finding `doom-modeline-def-*' forms.
	;; ;; Must be set before loading doom-modeline.
	;; (setq doom-modeline-support-imenu t)

	;; ;; Override attributes of the face used for padding.
	;; ;; If the space character is very thin in the modeline, for example if a
	;; ;; variable pitch font is used there, then segments may appear unusually close.
	;; ;; To use the space character from the `fixed-pitch' font family instead, set
	;; ;; this variable to `(list :family (face-attribute 'fixed-pitch :family))'.
	;; (setq doom-modeline-spc-face-overrides nil)

	;; ;; How to detect the project root.
	;; ;; nil means to use `default-directory'.
	;; ;; The project management packages have some issues on detecting project root.
	;; ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
	;; ;; to hanle sub-projects.
	;; ;; You can specify one if you encounter the issue.
	;; (setq doom-modeline-project-detection 'auto)

	;; ;; Determines the style used by `doom-modeline-buffer-file-name'.
	;; ;;
	;; ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
	;; ;;   auto => emacs/l/comint.el (in a project) or comint.el
	;; ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
	;; ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
	;; ;;   truncate-with-project => emacs/l/comint.el
	;; ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
	;; ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
	;; ;;   truncate-all => ~/P/F/e/l/comint.el
	;; ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
	;; ;;   relative-from-project => emacs/lisp/comint.el
	;; ;;   relative-to-project => lisp/comint.el
	;; ;;   file-name => comint.el
	;; ;;   file-name-with-project => FOSS|comint.el
	;; ;;   buffer-name => comint.el<2> (uniquify buffer name)



	;; ;; Whether display the buffer name.
	(setq doom-modeline-buffer-name t)

	;; ;; Whether highlight the modified buffer name.
	(setq doom-modeline-highlight-modified-buffer-name t)

	;; ;; When non-nil, mode line displays column numbers zero-based.
	;; ;; See `column-number-indicator-zero-based'.
	(setq doom-modeline-column-zero-based t)

	;; ;; Specification of \"percentage offset\" of window through buffer.
	;; ;; See `mode-line-percent-position'.
	;; (setq doom-modeline-percent-position '(-3 "%p"))

	;; ;; Format used to display line numbers in the mode line.
	;; ;; See `mode-line-position-line-format'.
	;; (setq doom-modeline-position-line-format '("L%l"))

	;; ;; Format used to display column numbers in the mode line.
	;; ;; See `mode-line-position-column-format'.
	;; (setq doom-modeline-position-column-format '("C%c"))

	;; ;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
	(setq doom-modeline-position-column-line-format '("%l:%c"))

	;; ;; Whether display the minor modes in the mode-line.
	;; (setq doom-modeline-minor-modes nil)

	;; ;; If non-nil, a word count will be added to the selection-info modeline segment.
	(setq doom-modeline-enable-word-count t)

	;; ;; Major modes in which to display word count continuously.
	;; ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
	;; ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
	;; ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
	;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

	;; ;; Whether display the buffer encoding.
	(setq doom-modeline-buffer-encoding nil)

	;; ;; Whether display the indentation information.
	(setq doom-modeline-indent-info nil)

	;; ;; Whether display the project name. Non-nil to display in the mode-line.
	;; (setq doom-modeline-project-name t)

	;; ;; Whether display the workspace name. Non-nil to display in the mode-line.
	;; (setq doom-modeline-workspace-name t)

	;; ;; Whether display the perspective name. Non-nil to display in the mode-line.
	;; (setq doom-modeline-persp-name t)

	;; ;; If non nil the default perspective name is displayed in the mode-line.
	;; (setq doom-modeline-display-default-persp-name nil)

	;; ;; If non nil the perspective name is displayed alongside a folder icon.
	;; (setq doom-modeline-persp-icon t)

	;; ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
	(setq doom-modeline-lsp t)

	;; ;; Whether display the GitHub notifications. It requires `ghub' package.
	;; (setq doom-modeline-github nil)

	;; ;; The interval of checking GitHub.
	;; (setq doom-modeline-github-interval (* 30 60))

	;; ;; When non-nil, always show the register name when recording an evil macro.
	(setq doom-modeline-always-show-macro-register t)

	;; ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
	;; (setq doom-modeline-mu4e nil)
	;; ;; also enable the start of mu4e-alert
	;; (mu4e-alert-enable-mode-line-display)

	;; ;; Whether display the battery status. It respects `display-battery-mode'.
	(setq doom-modeline-battery t)

	;; ;; Whether display the time. It respects `display-time-mode'.
	(setq doom-modeline-time t)

	;; ;; Whether display the environment version.
	;; (setq doom-modeline-env-version t)
	;; ;; Or for individual languages
	;; (setq doom-modeline-env-enable-python t)
	;; (setq doom-modeline-env-enable-ruby t)
	;; (setq doom-modeline-env-enable-perl t)
	;; (setq doom-modeline-env-enable-go t)
	;; (setq doom-modeline-env-enable-elixir t)
	;; (setq doom-modeline-env-enable-rust t)

	;; ;; Change the executables to use for the language version string
	;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
	;; (setq doom-modeline-env-ruby-executable "ruby")
	;; (setq doom-modeline-env-perl-executable "perl")
	;; (setq doom-modeline-env-go-executable "go")
	;; (setq doom-modeline-env-elixir-executable "iex")
	;; (setq doom-modeline-env-rust-executable "rustc")
	)

(use-package projectile
  :ensure t
  :init (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  ;; (projectile-switch-project-action #'projectile-dired)
	;; . 1 is the depth to search, nil means infinite depth
  (projectile-project-search-path '("~/projects/" "~/work/" "~/github" ("~/programming/" . 1)))
	(add-hook 'project-find-functions #'project-projectile)
  :config
	;; The default is to not sort files:
	;; (setq projectile-sort-order 'default)
	
	;; To sort files by recently opened:
	;; (setq projectile-sort-order 'recentf)

	;; To sort files by recently active buffers and then recently opened files:
	(setq projectile-sort-order 'recently-active)

	;; To sort files by modification time (mtime):
	;; (setq projectile-sort-order 'modification-time)

	;; To sort files by access time (atime):
	;; (setq projectile-sort-order 'access-time)

	(setq projectile-enable-caching t)
	(setq projectile-enable-caching 'persistent)
	;; Now the project cache is persistent and will be preserved during Emacs restarts. Each project gets its own cache file, that will be placed in the root folder of the project. The name of the cache file is .projectile-cache.eld by default, but you can tweak it if you want to:
	;; (setq projectile-cache-file "foo.eld")

	;; clean up known projects when they are deleted from the filesystem
	(setq projectile-cleanup-known-projects t)
  ;; Use a lambda function to handle the project arg correctly
  ;; (setq projectile-switch-project-action 'projectile-dired)
	(setq projectile-switch-project-action #'projectile-find-dir)
	(setq projectile-find-dir-includes-top-level t)
	)

(use-package rg
  :ensure t
  :commands (rg rg-dwim rg-literal)
  :init
  (rg-enable-default-bindings)
	)

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :config
  ;; (consult-projectile 1)
	;; A multiview for displaying open buffers, files and directories accociated with a project.
	;; When no project is open in the current buffer display a list of known project.
	;; and select a file from the selected project.

	;; Additionally seperate single source function are available.

	;; Just run the function `consult-projectile' and/or bind it to a hotkey.

	;; To filter the multiview use:
	;; b - For project related buffers
	;; d - For project related dirs
	;; f - For project related files
	;; p - For known projects
	;; r - For project recent files
	)

(use-package org-project-capture
  :after (org projectile)
  :ensure t
	)

(use-package org-projectile
  :after (org projectile)
  :ensure t
  :config
	(progn
		(setq org-project-capture-default-backend
					(make-instance 'org-project-capture-projectile-backend))
		(setq org-projectile-per-project-filepath "~/git/Emacs-Todos/TODOs.org")
		;; (org-project-capture-single-file)
		;; OR
		(org-project-capture-per-project)
		)
	)

(use-package flycheck-projectile
  :after (flycheck projectile)
  :ensure t
  :config
  ;; (add-hook 'flycheck-mode-hook #'flycheck-projectile-auto-set-checker)
	)

(defvar my/lsp-mode-list
  '(bash-mode
    c-mode c++-mode
    csharp-mode
    cmake-mode
    css-mode scss-mode lsp-tailwindcss-mode  ;; tailwind minor-mode variant listed
    dockerfile-mode
    eslint-mode
    gdscript-mode
    go-mode
    html-mode
    java-mode
    js-mode typescript-mode tsx-ts-mode
    json-mode yaml-mode
    lua-mode
    markdown-mode
    prisma-mode
    prolog-mode
    python-mode
    ruby-mode
    rust-mode
    sql-mode sqlite-mode
    latex-mode
    conf-mode
    zig-mode)
  "Major (and a few minor) modes for which we try LSP, and fall back to eglot.")

(defcustom my/lsp-to-eglot-fallback-delay 0.8
  "Seconds to wait after requesting lsp before falling back to eglot.
Increase if your `lsp-deferred` usually needs longer to start."
  :type 'number
	)

(defun my/eglot--maybe-fallback-to-eglot ()
  "If neither `lsp-mode` nor `eglot` became active, start eglot as fallback.
This is intended to be run a short time after trying to start `lsp`."
  (unless (or (bound-and-true-p lsp-mode)
              (bound-and-true-p eglot--managed-mode))
    (message "lsp-mode didn't start in %s s — falling back to eglot"
             my/lsp-to-eglot-fallback-delay)
    (eglot-ensure))
	)

(defun my/start-lsp-or-eglot ()
  "Try `lsp-deferred` if available; otherwise start `eglot`.
When we attempt `lsp-deferred` we schedule a short fallback check to
start eglot if lsp wasn't activated."
  (if (fboundp 'lsp-deferred)
      (progn
        (condition-case _err
            (lsp-deferred)
          (error
           ;; if lsp failed immediately, start eglot right away
           (eglot-ensure)))
        ;; schedule fallback check (non-blocking)
        (run-at-time my/lsp-to-eglot-fallback-delay nil #'my/eglot--maybe-fallback-to-eglot))
    ;; no lsp available: just start eglot
    (eglot-ensure))
	)

;; attach the starter to each mode's hook
(dolist (mode my/lsp-mode-list)
  (let ((hook (intern (format "%s-hook" (symbol-name mode)))))
    (add-hook hook #'my/start-lsp-or-eglot))
	)

;; ---------------------------
;; use-package for eglot (keeps your original customizations)
;; ---------------------------
;; (use-package eglot
;; 	:hook (eglot-managed-mode . display-fill-column-indicator-mode)
;;   :ensure nil
;;   :custom
;;   (eglot-events-buffer-size 0)
;;   (eglot-autoshutdown t)
;;   (eglot-report-progress nil)
;;   ;; optional: silence server progress if you prefer
;;   ;; :hook is handled above via my/start-lsp-or-eglot
;;   )

;; ---------------------------
;; company integration (works for lsp-mode and eglot)
;; ---------------------------
;; (when (require 'company nil :noerror)
;;   ;; ensure company uses capf (completion-at-point functions) for LSP/Eglot
;;   ;; `company-capf` will be provided by lsp-mode or eglot via `capf`.
;;   (unless (member 'company-capf company-backends)
;;     (add-to-list 'company-backends 'company-capf))
;;   ;; make sure buffer-local backend is capf when either server is active
;;   (add-hook 'eglot-managed-mode-hook
;;             (lambda ()
;;               (setq-local company-backends (cons 'company-capf
;;                                                  (remove 'company-capf company-backends)))))
;;   (add-hook 'lsp-mode-hook
;;             (lambda ()
;;               (setq-local company-backends (cons 'company-capf
;;                                                  (remove 'company-capf company-backends)))))
;; 	)

;; ---------------------------
;; Optional: a small helper to force eglot fallback manually
;; ---------------------------
;; (defun my/force-eglot-now ()
;;   "Stop lsp-mode (if active) and start eglot in the current buffer."
;;   (interactive)
;;   (when (bound-and-true-p lsp-mode)
;;     (lsp-disconnect))
;;   (eglot-ensure))

(use-package lsp-mode
  :commands (lsp lsp-deferred) 
	:init
  (setq lsp-keymap-prefix "C-c l"
        ;; Performance tweaks
        lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-prefer-flymake nil
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 5000
        ;; UI enhancements
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-enable-which-key-integration t
        ;; Disable costly features
        lsp-enable-on-type-formatting nil
        lsp-enable-folding nil)

  ;; Optional: auto-format and imports on save

  ;; replace XXX-mode with concrete major-mode[e. g. python-mode]
	:hook ((lsp-mode . (lambda ()
                       ;; runs when lsp-mode activates in a buffer
                       (lsp-enable-which-key-integration)
											 (display-fill-column-indicator-mode)
                       ;; format + organize imports on save (buffer-local)
                       (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                       (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
				 
         ;; Shell / Configs
         (bash-mode . lsp-deferred) ;; to install language server: M-x lsp-install-server RET bash-ls
         ;; Debugger: No

         ;; ----------------------------
         ;; C family
         (c-mode . lsp-deferred) ;; to install: sudo pacman -S clang (provides clangd)
         ;; Debugger: Yes (gdb or lldb)

         (c++-mode . lsp-deferred) ;; to install: sudo pacman -S clang (provides clangd)
         ;; Debugger: Yes (gdb or lldb)

         ;; ----------------------------
         ;; C#
         (csharp-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET csharp-ls
         ;; Debugger: Yes (netcoredbg)

         ;; ----------------------------
         ;; CMake
         (cmake-mode . lsp-deferred) ;; to install: pip install cmake-language-server
         ;; Debugger: No

         ;; ----------------------------
         ;; CSS / Tailwind
         (css-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET css-ls
         ;; Debugger: No

         (scss-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET css-ls
         ;; Debugger: No

         (lsp-tailwindcss-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET tailwindcss
         ;; Debugger: No

         ;; ----------------------------
         ;; Docker
         (dockerfile-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET dockerfile-ls
         ;; Debugger: No

         ;; ----------------------------
         ;; ESLint (JS/TS lint integration)
         (eslint-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET eslint
         ;; Debugger: No

         ;; ----------------------------
         ;; Godot
         (gdscript-mode . lsp-deferred) ;; to install: install Godot Engine (with GDNative LSP support enabled)
         ;; Debugger: No

         ;; ----------------------------
         ;; Go
         (go-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET gopls
         ;; Debugger: Yes (dlv - Delve)

         ;; ----------------------------
         ;; HTML
         (html-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET html-ls
         ;; Debugger: No

         ;; ----------------------------
         ;; Java
         (java-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET jdtls
         ;; Debugger: Yes (jdb)

         ;; ----------------------------
         ;; JavaScript / TypeScript / React / Next.js
         (js-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET ts-ls or curl -fsSL https://deno.land/install.sh | sh
         ;; Debugger: Yes (browser devtools / node debug)

         (typescript-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET ts-ls or curl -fsSL https://deno.land/install.sh | sh
         ;; Debugger: Yes (browser devtools / node debug)

         (tsx-ts-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET ts-ls (supports React & Next.js)
         ;; Debugger: Yes (browser devtools / node debug)

         ;; ----------------------------
         ;; JSON / YAML
         (json-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET json-ls
         ;; Debugger: No

         (yaml-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET yamlls
         ;; Debugger: No

         ;; ----------------------------
         ;; Lua
         (lua-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET lua-language-server or luarocks install --server=https://luarocks.org/dev lua-lsp --local

         ;; Debugger: No

         ;; ----------------------------
         ;; Markdown
         (markdown-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET marksman or npm i -g unified-language-server
         ;; Debugger: No

         ;; ----------------------------
         ;; Prisma
         (prisma-mode . lsp-deferred) ;; to install: yay prisma-language-server
         ;; Debugger: No

         ;; ----------------------------
         ;; Prolog
         (prolog-mode . lsp-deferred) ;; to install: swipl -g 'pack_install(lsp_server).'
         ;; Debugger: No

         ;; ----------------------------
         ;; Python
         (python-mode . lsp-deferred) ;; to install: pip install 'python-lsp-server[all]'
         ;; Debugger: Yes (pdb, debugpy for VSCode-like experience)

         ;; ----------------------------
         ;; Ruby
         (ruby-mode . lsp-deferred) ;; to install: gem install solargraph or gem install rubocop
         ;; Debugger: Yes (rdebug-ide)

         ;; ----------------------------
         ;; Rust
         (rust-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET rust-analyzer
         ;; Debugger: Yes (lldb/gdb with CodeLLDB)

         ;; ----------------------------
         ;; SQL / SQLite
         (sql-mode . lsp-deferred) ;; to install: npm i -g sql-language-server
         ;; Debugger: No

         (sqlite-mode . lsp-deferred) ;; to install: npm i -g sql-language-server (same server handles SQLite syntax)
         ;; Debugger: No

         ;; ----------------------------
         ;; LaTeX
         (latex-mode . lsp-deferred) ;; to install: luarocks --server http://luarocks.org/dev install digestif
         ;; Debugger: No

         ;; ----------------------------
         ;; Unix config formats
         (conf-mode . lsp-deferred) ;; to install: varies, often handled by generic text-ls or YAML/JSON servers
         ;; Debugger: No

         ;; ----------------------------
         ;; Zig
         (zig-mode . lsp-deferred) ;; to install: M-x lsp-install-server RET zls
         ;; Debugger: No

         )

  :custom
  ;; keep breadcrumb disabled by default (you can enable it if you like)
  (lsp-headerline-breadcrumb-enable nil)

  :config
  ;; runtime improvements for large projects
  (setq read-process-output-max (* 10 1024 1024) ;; 10MB
        gc-cons-threshold 200000000)             ;; 200MB

  ;; project-specific LSP settings (example)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("rust-analyzer.checkOnSave.command" "clippy" t)
     ("python.linting.enabled" t t))
   )

	:commands lsp
	)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;;lsp ui customazasion options 

;; side line 
;; Customization:

;; lsp-ui-sideline-show-diagnostics show diagnostics messages in sideline
;; lsp-ui-sideline-show-hover show hover messages in sideline
;; lsp-ui-sideline-show-code-actions show code actions in sideline
;; lsp-ui-sideline-update-mode When set to 'line' the information will be updated when user changes current line otherwise the information will be updated when user changes current point
;; lsp-ui-sideline-delay seconds to wait before showing sideline
;; lsp-ui-sideline-diagnostic-max-lines default to showing only the first line of diagnostic messages, increase for more verbose messages, decrease if flickering occurs

;;ui-peek 
;; You may remap xref-find-{definitions,references} (bound to M-. M-? by default):


;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;; There is a window-local jump list dedicated to cross references:


;; (lsp-ui-peek-jump-backward)
;; (lsp-ui-peek-jump-forward)
;; Other cross references:


;; (lsp-ui-peek-find-workspace-symbol "pattern 0")
;; ;; If the server supports custom cross references
;; (lsp-ui-peek-find-custom 'base "$cquery/base")
;; Customization:

;; lsp-ui-peek-enable enable ‘lsp-ui-peek’
;; lsp-ui-peek-show-directory show the directory of files

;; ui-doc 
;; Customization:

;; lsp-ui-doc-enable Enable lsp-ui-doc
;; lsp-ui-doc-position Where to display the doc (top, bottom or at-point)
;; lsp-ui-doc-side Where to display the doc (left or right)
;; lsp-ui-doc-delay Number of seconds before showing the doc
;; lsp-ui-doc-show-with-cursor When non-nil, move the cursor over a symbol to show the doc
;; lsp-ui-doc-show-with-mouse When non-nil, move the mouse pointer over a symbol to show the doc

;; imenu
;; Customization:

;; lsp-ui-imenu-kind-position place to show entries kind
;; lsp-ui-imenu-buffer-position place to show the buffer window
;; lsp-ui-imenu-window-width set window width
;; lsp-ui-imenu-window-fix-width when non-nil, the window will not be resizable (eg. unaffected by balance-windows)
;; lsp-ui-imenu--custom-mode-line-format mode line format
;; lsp-ui-imenu-auto-refresh auto refresh when necessary
;; lsp-ui-imenu-refresh-delay delay to refresh imenu

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
	:custom
	;; sediline configuration
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t)
	(lsp-ui-sideline-show-hover t)

	;; ui-peek configuration
	(lsp-ui-peek-enable t)
	(lsp-ui-peek-always-show t)  ;; Always show the peek window
	(lsp-ui-peek-height 20)      ;; Set the height of the peek window
	(lsp-ui-peek-position 'bottom)  ;; Position the peek window at the bottom
	
	;; ui-doc configuration
	(lsp-ui-doc-enable nil)
	;; move lsp doc at bottom right
  (lsp-ui-doc-position 'bottom)  ;; Show doc at point
  (lsp-ui-doc-delay 0.2)           ;; Delay before showing doc
  (lsp-ui-doc-show-with-cursor t)  ;; Show doc when hovering over a symbol
  (lsp-ui-doc-show-with-mouse t)   ;; Show doc when hovering with mouse

	;; imenu configuration
	;; nothig dont want it 
	)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands
  (lsp-treemacs-errors-list
   lsp-treemacs-symbols
   lsp-treemacs-references
   lsp-treemacs-implementations
   lsp-treemacs-call-hierarchy
   lsp-treemacs-type-hierarchy
   lsp-treemacs-deps-list)
  :init
  ;; Synchronize LSP workspace folders with Treemacs projects
  (setq lsp-treemacs-sync-mode 1)
  ;; Set Treemacs position to the right
  (setq treemacs-position 'right)
  ;; Enable follow mode to automatically focus on the current file
  (setq treemacs-follow-mode t)
  :config
  ;; Optional: Bind keys for quick access to LSP Treemacs views
	)

;; icons for treemacs
(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons")
	)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
	)    

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs)
	)

(use-package treesit-auto
	:ensure t 
  :custom
  (treesit-auto-install 'prompt)
	:config
  (treesit-auto-add-to-auto-mode-alist 'all)  ;; register all supported ts-modes
  (global-treesit-auto-mode)
	)

(use-package lsp-tailwindcss
  :vc (:url "https://github.com/merrickluo/lsp-tailwindcss" :rev :newest)
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (mode '(web-mode css-mode typescript-mode typescript-ts-mode tsx-ts-mode js2-mode js-mode json-mode))
    (add-to-list 'lsp-tailwindcss-major-modes mode))
	)

(use-package hyprlang-ts-mode
  :ensure t
  :custom
  (hyprlang-ts-mode-indent-offset 2)
	)

;; ---------- WEB-MODE ----------
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.scss\\'"  . web-mode)
         ("\\.json\\'"  . web-mode)
         ("\\.jsx?\\'"  . web-mode)  ;; .js/.jsx
         ("\\.tsx?\\'"  . web-mode)) ;; .ts/.tsx (we use web-mode for tsx)
  :init
  ;; Help web-mode detect JSX/TSX by path if needed
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
					("tsx" . "\\.ts[x]?\\'")))
  :config
  ;; Core behavior: auto-pairs, auto-closes, quotes, highlights
	(setq web-mode-enable-auto-pairing     t    ; auto-pair brackets/quotes
				web-mode-enable-auto-quoting     t    ; auto-insert quotes in attributes
				web-mode-enable-css-colorization t
				web-mode-enable-current-element-highlight nil
				;; Auto-close tags: 
				web-mode-tag-auto-close-style     2
				;; Indentation (optional):
				web-mode-markup-indent-offset     2
				web-mode-css-indent-offset        2
				web-mode-code-indent-offset       2)

  ;; If you previously added a hook that disabled pairing, remove it:
  ;; (safe even if that hook doesn't exist)
  (when (fboundp 'my-web-mode-hook)
    (remove-hook 'web-mode-hook #'my-web-mode-hook))

  ;; Start LSP automatically for JS/TSX files opened in web-mode (deferred)
  ;; Hook only lsp-mode, not eglot
  (add-hook 'web-mode-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "\\.\\(jsx?\\|tsx?\\)\\'" buffer-file-name))
                ;; Set JSX/TSX content-type
                (setq-local web-mode-content-type
                            (if (string-match-p "\\.tsx\\'" buffer-file-name) "tsx" "jsx"))
                ;; Always use lsp-mode here
                (when (fboundp 'lsp-deferred)
                  (lsp-deferred)))))
	)

;; ---------- LSP language-id mappings ----------
;; Make lsp-mode treat .jsx/.tsx as the react language ids it expects.
(with-eval-after-load 'lsp-mode
  ;; These are filename regex -> language-id entries (lsp-mode supports both)
  (add-to-list 'lsp-language-id-configuration '("\\.tsx\\'" . "typescriptreact"))
  (add-to-list 'lsp-language-id-configuration '("\\.jsx\\'" . "javascriptreact"))
  (add-to-list 'lsp-language-id-configuration '("\\.ts\\'"  . "typescript"))
  (add-to-list 'lsp-language-id-configuration '("\\.html\\'"  . "html"))
  (add-to-list 'lsp-language-id-configuration '("\\.css\\'"  . "css"))
  (add-to-list 'lsp-language-id-configuration '("\\.js\\'"  . "javascript"))
	)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  ;; Set up UI with icons
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
	(tooltip-mode 1)
	(dap-ui-controls-mode 1)

  (setq dap-lldb-debugged-program-function
        (lambda () (read-file-name "Select file to debug: ")))

  ;; Require language-specific dap adapters
  (require 'dap-gdb)         ;; C, C++, Rust via gdb DAP
  (require 'dap-lldb)        ;; or LLDB
  (require 'dap-cpptools)    ;; optional alternative
  (require 'dap-dlv-go)      ;; Go (Delve)
  (require 'dap-python)      ;; Python (debugpy)
  (require 'dap-java)        ;; Java
  (require 'dap-node)        ;; JS/TS (Node)
  (require 'dap-chrome)      ;; JS/TS (Browser)
  (require 'dap-firefox)     ;; JS/TS (Browser)
  (require 'dap-ruby)        ;; Ruby
  (require 'dap-netcore)     ;; C# .NET Core

  ;; Example debug templates:

  ;; Register patterns for GDB-based debug template
  (dap-register-debug-template "GDB Debug - Select Executable"
    (list :type "gdb" 
          :request "launch"
          :name "GDB::Launch"
          :program nil     ;; Emacs will prompt
          :cwd nil))

  ;; C/C++ (lldb)
  (dap-register-debug-template "C++ LLDB Debug (select executable)"
    (list :type "lldb"
          :request "launch"
          :name "LLDB::Run"
          :program nil
          :cwd nil))

  ;; Rust
  (dap-register-debug-template "Rust :: LLDB Run Configuration"
    (list :type "lldb"
          :request "launch"
          :name "Rust::Run"
          :program nil
          :cwd nil))
          ;; :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
          ;; :cwd "${workspaceFolder}"))

  ;; Go
  (dap-register-debug-template "Go :: Delve Debug"
    (list :type "go"
          :request "launch"
          :name "Go Debug"
          :mode "auto"
          :program nil))
          ;; :program "${workspaceFolder}"))

  ;; Python
  (dap-register-debug-template "Python :: Debug Current File"
    (list :type "python"
          :request "launch"
          :name "Python :: File"
          :program nil
          :cwd nil))
          ;; :program "${file}"
          ;; :cwd "${workspaceFolder}"))

  ;; Node/TSX (React)
  (dap-register-debug-template "Node :: Launch File"
    (list :type "node"
          :request "launch"
          :name "Node :: Launch"
          :program nil
          :cwd nil))
          ;; :program "${file}"
          ;; :cwd "${workspaceFolder}"))

  ;; C# .NET Core
  (dap-register-debug-template "C# .NET Core Launch"
    (list :type "coreclr"
          :request "launch"
          :name "C# :: Launch"
          :program nil
          :cwd nil))
          ;; :program "${workspaceFolder}/bin/Debug/net6.0/YourApp.dll"
          ;; :cwd "${workspaceFolder}"))
)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
	)

(use-package yasnippet-snippets
  :ensure t
	:after yasnippet
	)

(use-package auto-rename-tag
  :ensure t
	:hook (web-mode . auto-rename-tag-mode)
	)

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode)
	)

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/usr/bin/fish")  ;; Use Fish shell
  (setq vterm-max-scrollback 10000)   ;; Increase scrollback buffer
	)

(use-package term
  :ensure nil
  :commands ansi-term
  :config
  (setq explicit-shell-file-name "/usr/bin/fish")  ;; Use Fish shell
	)

;; Use Popper for transient terminals
(use-package popper
	:ensure t
	:bind (("C-=" . popper-toggle-latest)
         ("M-]" . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*vterm\\*" "\\*ansi-term\\*" "\\*eat\\*" "\\*term\\*"))
  (defun my-popper-window-height (win)
    "Set the popup window WIN to one-third of the screen height."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
	
  (setq popper-window-height #'my-popper-window-height)
	
  (popper-mode 1)
	)

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1)
	)  ; Enable commentary functionality globally

(use-package colorful-mode
  :ensure t
  :hook (prog-mode . colorful-mode)
  :custom
  ;; Allow using mouse to change colors
  (colorful-allow-mouse-clicks t)
  ;; Highlight the actual color instead of using a prefix indicator
  (colorful-use-prefix nil)
  ;; Use short hex format when possible (#RGB instead of #RRGGBB when applicable)
  (colorful-short-hex-conversions t)
  ;; Highlight colors everywhere, not just in strings
  (colorful-only-strings nil)
  :config
  ;; Configure color highlighting for different major modes
  (setq colorful-extra-color-keyword-functions
        '(colorful-add-hex-colors
          ((css-mode html-mode web-mode js-mode js2-mode) . 
           (colorful-add-css-variables-colors
            colorful-add-rgb-colors
            colorful-add-hsl-colors
            colorful-add-oklab-oklch-colors
            colorful-add-color-names))
          (emacs-lisp-mode . (colorful-add-color-names 
                              colorful-add-rgb-colors
                              colorful-add-hex-colors))
          (latex-mode . colorful-add-latex-colors)))
	(set-face-attribute 'colorful-base nil :box nil)
	)

(use-package vundo
	:ensure t
	:bind (:map vundo-mode-map
         ("q" . vundo-quit)
         ("?" . vundo-help)
				 ("b" . vundo-stem-root)
				 ("B" . vundo-stem-end)
				 ("d" . vundo-diff)
				 ("<down>" . vundo-next)
				 ("<up>" . vundo-previous)
				 )
  :config
	(setq vundo-glyph-alist vundo-unicode-symbols)
	(setq vundo--timestamps t)
	)

(use-package visual-replace
  :ensure t
  :bind (:map visual-replace-mode-map
         ("C-SPC" . visual-replace-toggle-scope)
				 ("RET" . visual-replace-apply-one)
				 ("C-RET" . visual-replace-enter)
				 ("u" . visual-replace-undo)
				 ("s" . visual-replace-substring-match)
				 ("c" . visual-replace-toggle-case-fold)
				 ("n" . visual-replace-next-match)
				 ("p" . visual-replace-prev-match)
				 ("?" . visual-replace-show-keymap)
				 )
	:custom
	(visual-replace-global-mode 1)
	)

;; HTTP server
(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root (expand-file-name "."))  ;; your project root
  (httpd-start)
	)

;; Live preview
(use-package impatient-mode
  :ensure t
  :config
  (setq impatient-mode-hook
        (lambda () (message "Impatient-mode activated: preview at http://localhost:8080/imp/")))
	)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
	)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-c S" . flyspell-correct-wrapper))
	:config
)

(use-package flyspell-correct-ivy
  :after flyspell-correct
)

(use-package jinx
  :ensure t
  :config 
	(add-hook 'emacs-startup-hook #'global-jinx-mode)
  )

(use-package eldoc-box
  :ensure t
  :custom
  (eldoc-box-cleanup-interval 0.2)
	(eldoc-box-hover-mode t)
	(add-hook 'eldoc-mode-hook
						'eldoc-box-hover-mode)
	(add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
	(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode)
	(add-hook 'lsp-mode-hook #'eldoc-box-hover-mode)
	)

(use-package evil-textobj-tree-sitter
  :ensure t
  :after (evil)
	)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el.git"
            :rev  :newest)
  :ensure nil ;; Since we are loading it manually, no need for package installation
	;; :init 
  ;; (add-to-list 'completion-at-point-functions #'copilot-complete)
  :hook (prog-mode . copilot-mode) ;; Enable Copilot in programming modes
  :bind (:map copilot-completion-map
              ("C-<tab>" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion)
              ("C-S-<iso-lefttab>" . copilot-accept-completion-by-word)
              ("C-e" . copilot-decline-completion)
							("C-p" . copilot-panel-complete)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("C-<right>" . copilot-next-completion)
              ("C-<left>" . copilot-previous-completion)
              ("M-c" . copilot-clear-overlay))
	:config
  ;; Optional: Set Node.js path if needed
  ;; (setq copilot-node-executable "/path/to/node")

  ;; Disable company-preview to avoid inline overlay conflicts&#8203;:contentReference[oaicite:12]{index=12}.
  (with-eval-after-load 'company
    (delq 'company-preview-if-just-one-frontend company-frontends))
  ;; (Optional) Use childframe frontend (via company-box or company-childframe) to keep UI separate&#8203;:contentReference[oaicite:13]{index=13}.
  ;; (company-box-mode 1)
	)

;; enable copilot globaly 
(defun my-enable-copilot-mode ()
  (when (and buffer-file-name (not (minibufferp)))
    (copilot-mode 1)))

(add-hook 'find-file-hook #'my-enable-copilot-mode)

;; (defun my-copilot-fallback-indent-offset (orig-fun &rest args)
;;   "Return a default indentation offset of 2 spaces if none is detected."
;;   (or (apply orig-fun args) 2))

;; (advice-add 'copilot--infer-indentation-offset :around #'my-copilot-fallback-indent-offset)

(setq copilot-indent-offset-warning-disable t)

;; need to do the following 2 for it to work 
;; git clone --depth 1 https://github.com/tjohnman/codeium-overlay.el.git ~/.emacs.d/codeium-overlay.el
;; git clone --depth 1 https://github.com/Exafunction/codeium.el ~/.emacs.d/codeium.el
(use-package codeium
  :vc (:url "https://github.com/Exafunction/codeium.el.git"
            :rev  :newest)
  :ensure nil
	:init
	;; Primary CAPF for Company
	(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq use-dialog-box nil)
  (setq codeium/metadata/api_key "6ce583f3-2c12-4f9e-8962-e6b3d8d3de2e")
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (setq codeium-api-enabled
				(lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))		

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions
                           #'codeium-completion-at-point)))
	)

;; fix keybinds under codeium-overlay
(use-package codeium-overlay
  :vc (:url "https://github.com/tjohnman/codeium-overlay.el.git"
            :rev  :newest)
	;; :bind (:map codeium-overlay-mode-map
  ;;             ("M-<tab>" . codeium-overlay-accept-suggested-completion)
  ;;             ;; ("M-<tab>" . codeium-overlay-tab-command)
	;; 						("M-p" . codeium-overlay-send-instruct)
  ;;             ("M-S-<iso-lefttab>" . codeium-overlay-tab-command)
  ;;             ("M-e" . codeium-overlay-reject-suggested-completion))
  :after codeium
  )

(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode t)
  :custom
  (diff-hl-margin-symbols-alist
   '((insert . " ")   ;; added
     (change . " ")   ;; modified
     (delete . " "))) ;; deleted
	:config
  (diff-hl-margin-mode t)
	;; (global-diff-hl-mode t) 
  ;; (diff-hl-flydiff-mode t)  
	)

;; evil stops some dired maps from working so use emacs mode isntead 
(evil-set-initial-state 'dired-mode 'emacs)
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("h"      . my/dired-up-directory)
              ("<left>" . my/dired-up-directory)
              ("l"      . my/dired-open-item)
              ("<right>" . my/dired-open-item)
              ("."    . dired-hide-dotfiles-mode)
              ("x"      . my/dired-cut-files)     ;; Custom cut
              ("c"      . my/dired-copy-files) ;; Custom copy
              ("p"      . my/dired-paste-files) ;; Custom paste
              ("M-c"      . dired-do-compress)
              ("M-RET"      . dired-open-with)
              ("r"      . dired-do-rename)
              ("d"      . dired-do-delete)
              ("a"      . dired-create-empty-file)
              ("M-SPC" . my/dired-toggle-mark)  ;; Custom toggle mark
							("P" . dired-preview-mode)
							("S-<up>" . dired-preview-page-up)
							("S-<down>" . dired-preview-page-down)
							("/" . dired-narrow)
							("TAB" . dirvish-subtree-toggle)
							("S-<iso-lefttab>" . dired-subtree-cycle)
							("C-r"    . dired-rsync)
							)
  :custom
  ; X -- extension short breaks dir tho so dint use it
  (dired-listing-switches "-agholv --group-directories-first --time-style=long-iso")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Navigation functions omitted for brevity

  ;; Enhanced navigation functions
  (defun my/dired-up-directory ()
    "Smart parent directory navigation with buffer reuse"
    (interactive)
    (if (file-symlink-p dired-directory)
        (dired (file-name-directory (file-chase-links dired-directory)))
      (dired-up-directory))
    (dired-kill-subdir))

  (defun my/dired-open-item ()
    "Smart open with buffer reuse and file handling"
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))

	;; Clipboard vars
  (defvar my/dired-file-clipboard nil
    "Clipboard for dired cut/copy operations.")
  (defvar my/dired-clipboard-is-cut nil
    "Non-nil if the clipboard is a cut (move) rather than copy.")
	
	(defun my/dired--files-here-or-marked ()
		"Marked files/dirs, or file/dir at point if none are marked."
		(let ((xs (dired-get-marked-files nil nil)))
			(unless xs (user-error "No file at point"))
			xs))

	(defun my/dired-copy-files ()
		"Copy marked (or current) files/dirs into the clipboard."
		(interactive)
		(setq my/dired-clipboard-is-cut nil
					my/dired-file-clipboard (my/dired--files-here-or-marked))
		(message "Copied %d item(s)" (length my/dired-file-clipboard)))

	(defun my/dired-cut-files ()
		"Cut marked (or current) files/dirs into the clipboard."
		(interactive)
		(setq my/dired-clipboard-is-cut t
					my/dired-file-clipboard (my/dired--files-here-or-marked))
		(message "Cut %d item(s)" (length my/dired-file-clipboard)))

	(defun my/dired-paste-files ()
		"Paste previously copied/cut files/dirs into current Dired directory."
		(interactive)
		(unless my/dired-file-clipboard
			(user-error "Clipboard is empty"))
		(let* ((dest (expand-file-name default-directory))
					 (n 0))
			(dolist (src my/dired-file-clipboard)
				(let* ((name (file-name-nondirectory (directory-file-name src)))
							 (dst  (expand-file-name name dest)))
					(if my/dired-clipboard-is-cut
							;; moves files *and* directories
							(rename-file src dst t)                           ;; overwrite OK
						;; copy file vs directory
						(if (file-directory-p src)
								(copy-directory src dst t t t)                  ;; keep-time, parents, contents
							(copy-file src dst t t t)))                       ;; overwrite, keep-time, preserve-uid/gid
					(cl-incf n)))
			(revert-buffer)
			(setq my/dired-file-clipboard nil
						my/dired-clipboard-is-cut nil)
			(message "Pasted %d item(s)" n)))

	(defun my/dired-toggle-mark ()
		"Toggle ‘*’ mark on the file/dir at point."
		(interactive)
		(let ((fname (dired-get-filename nil t)))
			(unless fname
				(user-error "No file on this line"))
			(save-excursion
				(beginning-of-line)
				(if (eq (char-after) dired-marker-char)
						(dired-unmark 1)
					(dired-mark 1)))))
	)

;; Additional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
	)

(use-package dired-subtree
  :after dired
  :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "TAB" 'dired-subtree-cycle))
	)

(use-package dired-narrow
  :after dired
  :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "/" 'dired-narrow))
	)

(use-package dired-rsync
  :after dired
  :config
	)

(use-package fd-dired
  :config (setq fd-dired-use-fdwalk-executable t)
	)

(use-package dired-preview
  :after dired
  :config
  (setq dired-preview-delay 0.5)
	(defun my-dired-preview-to-the-right ()
		"My preferred `dired-preview-display-action-alist-function'."
		'((display-buffer-in-side-window)
			(side . right)
			(window-width . 0.3)))

	(setq dired-preview-display-action-alist #'my-dired-preview-to-the-right)
	)

;; dont need open open-with is perfect for the times i need it
;; (use-package dired-open
;; :after dired
;; :config
;; (setq dired-open-extensions '(("png" . "xdg-open")
;;                               ("jpg" . "xdg-open")
;;                               ("jpeg" . "xdg-open")
;;                               ("pdf" . "xdg-open")
;;                               ("mp4" . "xdg-open")
;;                               ("mkv" . "xdg-open")
;;                               ("webm" . "xdg-open")
;;                               ("mp3" . "xdg-open")
;;                               ("ogg" . "xdg-open")
;;                               ("html" . "xdg-open")
;;                               ("htm" . "xdg-open")
;;                               ("xhtml" . "xdg-open")
;;                               ("docx" . "libreoffice")
;;                               ("doc" . "libreoffice")
;;                               ("xlsx" . "libreoffice")
;;                               ("xls" . "libreoffice")
;;                               ("pptx" . "libreoffice")
;;                               ("ppt" . "libreoffice")
;;                               ("odt" . "libreoffice")
;;                               ("ods" . "libreoffice")
;;                               ("odp" . "libreoffice")
;;                               ("txt" . "gedit") ;; or your preferred text editor
;; ))
;; )

(use-package dired-open-with
	:ensure t
	:after dired
  )

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                               "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                          "Drives")
     ("t" "~/.local/share/Trash/files/"  "TrashCan")
     ("g" "~/git/"          "Git cloned directorys")
     ("n" "~/git/emacs-notes/"              "Notes")
     ("p" "~/programming/" "Programming Directorys")
     ("r" "~/programming/Raylib-Game-Dev/" "Raylib")
     ("w" "~/programming/web_dev/"   "Web-dev code")
     ("w" "~/programming/Work/"         "Work Code")
     ("C" "~/programming/C++_Code/"      "C++ Code")
     ("P" "~/programming/Personal_Portfolio/" "Personal portfolio")
     ("e" "~/programming/ekpa/"              "Ekpa")
     ("c" "~/.config/"          "Config Directorys")
     ("e" "~/.config/MainEmacs/"     "Emacs config")
     ("h" "~/.config/hypr/"       "Hyprland config")
     ("k" "~/.config/kitty/"         "Kitty config")
     ("f" "~/.config/fish/"           "Fish config")
     ("N" "~/.config/MainNvim/"     "Neovim config")
     ("T" "~/git/Emacs-Todos/"              "Todos")
     ;; ("s" "/ssh:my-remote-server")      "SSH server"
     ;; ("e" "/sudo:root@localhost:/etc")  "Modify program settings"
		 ))
  :config
  (dirvish-side-follow-mode)
	(setq dirvish-hide-details t)
  (setq dirvish-cache-dir "~/.config/MainEmacs/dirvish-cache/")
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
	
  ;; X -- extension short breaks dir tho so dint use it
  ;; (dired-listing-switches "-agholv --group-directories-first --time-style=long-iso")
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state git-msg nerd-icons collapse file-size))
	

  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
	

	:bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   ("a"   . dired-create-empty-file)
	 ("r"   . dired-do-rename)
   ("M"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("h"   . dirvish-history-jump)      ; [r]ecent visited
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("g"   . dirvish-quick-access)      ; [g]o `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("e"   . dirvish-emerge-mode)
   ("t"   . dirvish-layout-toggle)
   ("S"   . dirvish-layout-switch)
   ("T"   . dired-toggle-marks)
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("S-<iso-lefttab>" . dired-subtree-cycle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)
	 ("C-R" . dirvish-rsync)
	 )
	)

(use-package dirvish-rsync
	:ensure nil ;; its inside dirvish just need to call it to be enabled
	:after dirvish
	)

(use-package dirvish-emerge
  :ensure nil
  :after dirvish
  :hook (dirvish-setup-hook . dirvish-emerge-mode) (dired-hook . dirvish-emerge-mode)
  :custom
  (dirvish-emerge-groups
   '(("Recent files" (predicate . recent-files-2h))
     ("Documents"(extensions "pdf" "tex" "bib" "epub"))
		 ("Audio" (extensions "mp3" "flac" "wav" "aac" "ogg" "m4a" "wma"))
		 ("Video" (extensions "mp4" "mkv" "webm" "avi" "mov" "mpg" "wmv" "flv"))
     ("Pictures" (extensions "jpg" "png" "svg" "gif" "eps" "ai" "jpeg" "bmp" "tiff" "tif" "webp" "heic" "apng"))
		 ("Documentation" (extensions "org" "md" "txt"))
     ("Src" (extensions "cpp" "c" "tsx" "ts" "jsx" "js" "html" "htm" "xhtml" "css" "xml" "json" "py" "java" "lua"))
		 ("Src-2" (extensions "el" "bash" "sh" "rb" "go" "rs" "php" "swift" "odin" "jai" "zig"))
		 ("Headers" (extensions "h" "hpp"))
		 ("Completion / Build files" (extensions "CMakeLists.txt" "cmake" "xmake" "Makefile" "makefile"))
		 ("Completion / Build files-2" (extensions "Meson.build" "SConstruct" "configure.ac" "configure.in"))
		 ("Completion / Build files-3" (extensions "Makefile.am" "build.ninja" "BUILD" "WORKSPACE" "Dockerfile"))
     ("Archives" (extensions "gz" "rar" "zip" "7z" "tar" "bz2" "xz" "tgz" "tar.gz"))
		 ("Config & Settings" (extensions "ini" "cfg" "conf" "yaml" "yml" "json" "toml" "xml" "env"))
	   ("Disk Images" (extensions "iso" "img" "dmg" "vhd"))
		 ("Executable & Installers" (extensions "exe" "msi" "rpm" "deb" "apk" "app"))
		 ("Temporary & Logs" (extensions "tmp" "log" "bak"))
		 ))
	)


(use-package dirvish-peek
	:ensure nil
	:after (dirvish ivy ivy-rich)
	:hook (after-init . dirvish-peek-mode)
	)

(use-package dirvish-vc
  :ensure nil
  :after dirvish
  )

(use-package dirvish-side
	:ensure nil 
	:after (dirvish)
	:custom 
	;; move to the right side
  (dirvish-side-display-alist '((side . right) (slot . -1)))
  (dirvish-side-width 20.0) ;; 30% of the frame width
	(dirvish-side-follow-mode t)
	(dirvish-side-attributes '(file-size))

	;; This extension provides the dirvish-side command, which toggles a Dirvish sidebar within the current frame. The width is fixed to prevent the window from unexpected resizing, but you can adjust it using the dirvish-side-increase-width and dirvish-side-decrease-width commands.

	;; When dirvish-side-follow-mode is enabled, the visible side session will select the current buffer’s filename, similar to treemacs-follow-mode in treemacs. It will also visits the latest project-root after switching to a new project.

	;; These customization options are available:

	;; dirvish-side-attributes: like dirvish-attributes, but for side window.
	;; dirvish-side-mode-line-format: like dirvish-mode-line-format, but for side window.
	;; dirvish-side-header-line-format: like dirvish-header-line-format, but for side window.
	;; dirvish-side-display-alist: Display actions for the side window.
	;; dirvish-side-window-parameters: Window parameters for the side window.
	;; dirvish-side-width: Width of the side window.
	;; dirvish-side-open-file-action: Action to perform before opening a file in a side window.
	;; dirvish-side-auto-expand: Whether to auto expand parent directories of current file.
	
	)

(use-package nerd-icons
  :if (display-graphic-p)
  :config
  (nerd-icons-install-fonts t)
	)

;; (use-package nerd-icons-dired
;;   :hook (dired-mode . (lambda () (nerd-icons-dired-mode t)))
;; 	)

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
	)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
	(setq centaur-tabs-style "wave")
	(setq centaur-tabs-height 32)
	(setq centaur-tabs-set-icons t)
	(setq centaur-tabs-icon-type 'nerd-icons)  ; or 'all-the-icons
	(setq centaur-tabs-set-bar 'under)
	(setq x-underline-at-descent-line t)
	(setq centaur-tabs-set-modified-marker t)
	(setq centaur-tabs-show-navigation-buttons t)
	;; (setq centaur-tabs-modified-marker "*")
	;; (setq centaur-tabs-set-close-button nil)
	(setq centaur-tabs-close-button "󰅖")
	(setq centaur-tabs-cycle-scope 'tabs)
	;; ‘default: (Already described)
	;; ‘tabs: Cycle through visible tabs (that is, the tabs in the current group)
	;; ‘groups: Navigate through tab groups only
	;; (setq centaur-tabs-label-fixed-length 8)

	;; Ivy integration
	;; You can integrate Ivy with centaur-tabs for changing tab-groups. Just use the (centaur-tabs-counsel-switch-group) and bind it to any key you want.

	;; Projectile integration
	;; You can group your tabs by Projectile’s project. Just use the following function in your configuration:

	(centaur-tabs-group-by-projectile-project)
	;; This function can be called interactively to enable Projectile grouping. To go back to centaur-tabs’s user defined (or default) buffer grouping function you can interactively call:

	;; (centaur-tabs-group-buffer-groups)

	)

(evil-set-initial-state 'dashboard-mode 'emacs)
(use-package dashboard
  :ensure t
  :config
  ;; Center dashboard content
  (setq dashboard-center-content t
        dashboard-vertically-center-content t)
	(setq dashboard-navigation-cycle t)
	(setq dashboard-heading-shorcut-format " [%s]")
	;; (setq dashboard-projects-switch-function 'projectile-persp-switch-project)
	(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
	(add-to-list 'dashboard-items '(agenda) t)
	(setq dashboard-week-agenda t)
	(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

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
  ;; Add icons
  (setq dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t)

  ;; shortcuts 
  (setq dashboard-item-shortcuts '((recents  . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")))
  ;; Define dashboard items
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 100)))

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

(use-package beacon
  :ensure t
  :custom
  (beacon-color "#16B4B6") ;; Set the color of the beacon
  (beacon-size 20) ;; Set the size of the beacon
  (beacon-blink-when-window-scrolls t) ;; Blink when the window scrolls
  (beacon-blink-when-point-moves t) ;; Blink when the point moves
  (beacon-blink-when-buffer-changes t) ;; Blink when the buffer changes
  :config
  (beacon-mode 1) ;; Enable beacon mode
	)

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t)
	)

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  ;; draw a solid line, not dots
  (highlight-indent-guides-method 'character)
  ;; only highlight the guide at point
  (highlight-indent-guides-responsive 'top)
	(highlight-indent-guides-character ?┃)
  :config
  ;; set colors exactly as desired
  (custom-set-faces
   '(highlight-indent-guides-character-face ((t (:foreground "gray50"))))
   '(highlight-indent-guides-top-character-face ((t (:foreground "#608EA9")))))
  (highlight-indent-guides-mode +1)
	)

(use-package hl-todo
	:custom
  (global-hl-todo-mode t)
  :config
	(setq hl-todo-keyword-faces
				'(("TODO"   . "#2563EB")
          ("HACK"   . "#FBBF24")
					("WARN"  . "#FBBF24")
          ("WARNING" . "#FBBF24")
					("PERFORMANCE" . "#5CD5DB")
          ("OPTIMIZE" . "#5CD5DB")
          ("NOTE"   . "#10B981")
          ("INFO" . "#10B981")
          ("TEST" . "#7C3AED")
          ("TESTING" . "#2563EB")
          ("PASSED" . "#22C55E")
          ("FAILED" . "#FF4500")
          ("EDIT" . "#7D3C98")
          ("REMOVE" . "#FF4500")
          ("DONE" . "#22C55E")
          ("FIXED" . "#22C55E")
          ("ERROR" . "#DC2626")
          ("IMPORTANT" . "#FF4500")
          ("CRITICAL" . "#FF3333")
					("FIXME"  . "#7D3C98")
					("DEBUG"  . "#7C3AED")
          ("REVIEW" . "#A020F0")
          ("DEPRECATED" . "#FF3333")
          ("BUG"     . "#FF3333")))
	(with-eval-after-load 'magit
		(add-hook 'magit-log-wash-summary-hook
							#'hl-todo-search-and-highlight t)
		(add-hook 'magit-revision-wash-message-hook
							#'hl-todo-search-and-highlight t))
  
	)

(use-package flycheck-hl-todo
	:after flycheck, hl-todo
  :config
  (flycheck-hl-todo-setup t)
	)

(use-package magit-todos
  :after magit, hl-todo
  :config (magit-todos-mode 1)
	)

(use-package golden-ratio
	:ensure t 
	:hook (after-init . golden-ratio-mode)
	)

(use-package dimmer
  :ensure t
  :custom
  (dimmer-fraction 0.20) ;; Dim non-focused windows to 20% brightness
  :config
	(dimmer-configure-which-key)
  (dimmer-configure-company-box)
	(dimmer-configure-hydra)
  (dimmer-mode t)
	)

;; (use-package cape
;;   :ensure t
;; 	:bind ("C-c c" . cape-prefix-map)
;; 	;; Alternatively bind Cape commands individually.
;;   ;; :bind (("C-c p d" . cape-dabbrev)
;;   ;;        ("C-c p h" . cape-history)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ...)
;;   :config
;; 	;; Use Company backends as Capfs.
;; ;; Cape provides the adapter cape-company-to-capf for Company backends. The adapter transforms Company backends to Capfs which are understood by the built-in Emacs completion mechanism. The function is approximately the inverse of the company-capf backend from Company. The adapter can be used as follows:

;; 	;; (setq-local completion-at-point-functions
;; 	;; 						(mapcar #'cape-company-to-capf
;; 	;; 										(list #'codeium-completion-at-point #'lsp-completion-at-point)))

;; ;; Note that the adapter does not require Company to be installed or enabled. Backends implementing the Company specification do not necessarily have to depend on Company, however in practice most backends do. The following shows a small example completion backend, which can be used with both completion-at-point (Corfu, default completion) and Company.

;; 	;; (defvar demo-alist
;; 	;; 	'((":-D" . "😀")
;; 	;; 		(";-)" . "😉")
;; 	;; 		(":-/" . "😕")
;; 	;; 		(":-(" . "🙁")
;; 	;; 		(":-*" . "😙")))

;; 	;; (defun demo-backend (action &optional arg &rest _)
;; 	;; 	(pcase action
;; 	;; 		('prefix (and (memq (char-before) '(?: ?\;))
;; 	;; 									(cons (string (char-before)) t)))
;; 	;; 		('candidates (all-completions arg demo-alist))
;; 	;; 		('annotation (concat " " (cdr (assoc arg demo-alist))))
;; 	;; 		('post-completion
;; 	;; 		 (let ((str (buffer-substring (- (point) 3) (point))))
;; 	;; 			 (delete-region (- (point) 3) (point))
;; 	;; 			 (insert (cdr (assoc str demo-alist)))))))

;; 	;; ;; Register demo backend with `completion-at-point'
;; 	;; (setq completion-at-point-functions
;; 	;; 			(list (cape-company-to-capf #'demo-backend)))

;; 	;; ;; Register demo backend with Company.
;; 	;; (setq company-backends '(demo-backend))

;; 	;; It is possible to merge multiple Company backends and use them as a single Capf using the company--multi-backend-adapter function from Company. The adapter transforms multiple Company backends into a single Company backend, which can then be used as a Capf via cape-company-to-capf. Capfs can be merged directly with cape-capf-super.

;; 	;; ;; ;; Use the company-dabbrev and company-elisp backends together.
;; 	;; (setq completion-at-point-functions
;; 	;; 			(list
;; 	;; 			 (cape-company-to-capf
;; 	;; 				(apply-partially #'company--multi-backend-adapter
;; 	;; 												 '(company-dabbrev company-elisp)))))

;; ;; 	;; Merge the dabbrev, dict and keyword capfs, display candidates together.
;; ;; (setq-local completion-at-point-functions
;; ;;             (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))

;; ;; ;; Alternative: Define named Capf instead of using the anonymous Capf directly
;; ;; (defun cape-dabbrev-dict-keyword ()
;; ;;   (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
;; ;; (setq-local completion-at-point-functions (list #'cape-dabbrev-dict-keyword))

;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               ;; Merge Codeium with LSP CAPF
;;               (setq-local completion-at-point-functions
;;                           (list (cape-capf-super
;;                                  #'codeium-completion-at-point
;;                                  #'lsp-completion-at-point))))) 
;; 	)

;; --- Codeium-only completion command ---
(defun my/codeium-only-completion ()
  "Temporarily use only Codeium for completion-at-point and invoke company."
  (interactive)
  (let ((capf-bk (or completion-at-point-functions
                     (default-value 'completion-at-point-functions))))
    ;; Set capf list to Codeium only
    (setq-local completion-at-point-functions '(codeium-completion-at-point))
    ;; Trigger company completion
    (call-interactively 'company-complete-common)
    ;; Restore original capf list
    (setq-local completion-at-point-functions capf-bk)))

;; Bind this command to C-c c (or any preferred key)
(global-set-key (kbd "C-c c") #'my/codeium-only-completion)

;;; --- Default CAPF configuration ---
;; Use Cape’s functions for additional completions
(use-package cape
  :ensure t
  :config
  ;; Add common Cape capfs to completion-at-point-functions.
  ;; The order matters: earlier functions take priority.
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list
                           ;; LSP/Eglot CAPFs first (language-aware completions)
                           (bound-and-true-p lsp-completion-at-point)
                           (bound-and-true-p eglot-completion-at-point)
                           ;; Cape backends for words, keywords, files, etc.
                           #'cape-dabbrev            ; words in buffers
                           #'cape-dict               ; dictionary words
                           #'cape-file               ; file names
                           #'cape-history            ; history in shell/minibuffer
                           #'cape-keyword            ; language keywords
                           #'cape-line               ; complete whole line
                           ;; Optionally, other Cape capfs can be added here
                           ))))
  ;; If using Org src blocks and other modes, you can hook similarly:
  ;; (add-hook 'org-src-mode-hook
  ;;           (lambda ()
  ;;             (let ((lang (org-src-get-lang-mode (org-element-property :language (org-element-context)))))
  ;;               (pcase lang
  ;;                 ("emacs-lisp" (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))
  ;;                 ("c++"         (add-to-list 'completion-at-point-functions #'cape-file))
  ;;                 ;; add more mode-specific capfs if desired
  ;;                 ))))
	)

;; Optionally wrap Codeium to only run in certain contexts:
(defun my/codeium-completion-at-point ()
  (when (and (null (thing-at-point 'symbol))
             (not (company-explicit-action-p)))
    (codeium-completion-at-point)))
(add-hook 'completion-at-point-functions #'my/codeium-completion-at-point -10 t)


(defun my/complete-codeium ()
  "Fetch completions from Codeium at point via `company-complete`."
  (interactive)
  (let ((completion-at-point-bkp completion-at-point-functions))
    (setq completion-at-point-functions '(codeium-completion-at-point))
    (call-interactively 'company-complete)
    (setq completion-at-point-functions completion-at-point-bkp))
	)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
	)

;; done with this
(use-package company-c-headers
  :ensure t
  :hook ((c-mode c++-mode) . (lambda ()
                               (add-to-list 'company-backends 'company-c-headers)))
	)

;; done with this
(use-package company-anaconda
  :ensure t
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends 'company-anaconda)))
	)

;; done with this
(use-package company-web
  :ensure t
  :hook ((web-mode css-mode html-mode) . (lambda ()
                                           (add-to-list 'company-backends 'company-web)))
	)

;; ;; Org-mode specific configuration
;; (defun org-src-mode-setup ()
;;   (when (and (boundp 'org-src-lang)  ;; Check if in Org source block context
;;              (derived-mode-p 'org-src-mode))
;;     (let ((lang (org-src-get-lang-mode org-src-lang)))  ;; Use org-src-lang instead of org-element-at-point
;;       (pcase lang
;;         (`emacs-lisp-mode (company-elisp-init))
;;         (`c++-mode (company-c-headers-init))
;;         ;; Add more language mappings as needed
;;         )))
;; 	)

;; (add-hook 'org-src-mode-hook 'org-src-mode-setup)

;; Company Mode Configuration
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-show-quick-access t)
  (company-tooltip-limit 10)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence))
  (company-tooltip-maximum-width   120)   ; max 90 columns 
  (company-tooltip-minimum-width   0)    ; allow shrink 
  (company-tooltip-width-grow-only t)    ; don’t shrink on page flip 
  :bind (:map company-active-map
              ("C-e" . company-abort)
              ("S-<down>" . company-select-next)
              ("S-<up>" . company-select-previous)
							("<up>" . nil)
							("<down>" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)  ;; Fixed keybinding
              ("TAB" . tab-indent-or-complete))
  :config
  (global-company-mode t)
  
  company-frontends '(company-pseudo-tooltip-frontend
											company-box-frontend
                      company-preview-frontend)

  (setq company-backends
        '((company-capf :with company-yasnippet)
          company-dabbrev-code
          company-keywords
          company-files
          company-dabbrev))
  )

;; Visual Enhancements (keep existing)
(use-package all-the-icons :ensure t)

(use-package company-box
  :ensure t
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-tooltip-minimum-width 40) 
  (company-box-tooltip-maximum-width 120)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-doc-enable t)
  (company-box-doc-position 'at-point)
  :config
  (setq company-box-scrollbar nil
        company-box-show-single-candidate t
        company-box-doc-delay 0.1)

	(setq company-box-icons-all-the-icons
        (cl-remove-if (lambda (item) (eq (car item) 'capf))
                      company-box-icons-all-the-icons))
  ;; Add a generic capf icon (for non-Codeium capf completions)
  (add-to-list 'company-box-icons-all-the-icons
               ;; `(capf . ,(all-the-icons-material "code" :face 'all-the-icons-purple)))
               `(capf . ,(all-the-icons-material "functions" :face 'all-the-icons-purple)))
  ;; Add a Codeium icon for Codeium completions
  (defun my/company-box-icon-for-codeium (candidate)
    ;; (when (and (eq (get-text-property 0 'company-backend candidate) 'capf)
    (when (and (eq (get-text-property 0 'company-backend candidate) 'company-capf)
               (string-match-p "codeium" (or (get-text-property 0 'company-kind candidate)
                                             (get-text-property 0 'company-label candidate)
                                             (get-text-property 0 'company-docsig candidate)
                                             "")))
      (propertize "󰘦" 'face '(:inherit font-lock-constant-face :height 1.0))))
  (add-to-list 'company-box-icons-functions #'my/company-box-icon-for-codeium)
	)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion)))
     (eglot (styles . (orderless flex)))
		 (lsp-mode (styles . (orderless flex)))))  ;; For LSP completions
  :config
  ;; Configure orderless to work better with programming language completions
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-regexp
          orderless-initialism
          ;; orderless-flex
					))
	)

;; Machine learning-powered sorting
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1)  ; Persistent history across sessions
  (setq prescient-aggressive-file-save t
        prescient-save-file "~/.config/MainEmacs/prescient-save.el")
	(setq prescient-sort-full-matches-first t)
	)

;; Ivy-prescient integration
(use-package ivy-prescient
  :ensure t
  :after prescient
  :config
  (ivy-prescient-mode 1)
  (setq ivy-prescient-retain-classic-highlighting t
        ivy-prescient-enable-filtering t
        ivy-prescient-enable-sorting t)
	)


;; Company-prescient integration
(use-package company-prescient
	:ensure t 
	:after prescient
	:config
	(company-prescient-mode 1)
	
)

(use-package eee
  :vc (:url "https://github.com/eval-exec/eee.el.git"
            :rev  :newest)
  :bind-keymap
  ;; ("s-e" . ee-keymap)
  :config
  
  ;; Should have wezterm or alacritty installed, more terminal application is supporting...
	;; Issues and pull requests are welcome
  (setq ee-terminal-command "kitty")
  )

(use-package perspective
  :ensure t
  :diminish perspective-mode
	:custom
	(persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  ;; 1. Name the very first perspective “Default” instead of “main”
  (setq persp-initial-frame-name           "Random"
        persp-nil-name                     "Random"
        ;; optional: sort perspectives by creation time
        persp-sort                         'created
        persp-show-modestring              t
        persp-modestring-short             t)
  ;; 2. Enable perspective-mode globally
  (persp-mode)
	)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'nerd-icons
        neo-window-position 'right
        neo-window-width 35
        neo-window-fixed-size nil
        neo-show-updir-line nil
        neo-smart-open t
        neo-show-hidden-files t)
	)

;; Ensure core dependencies are installed first
(use-package hydra
  :ensure t
  :defer t)

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (treemacs-load-theme "nerd-icons")
;;   (setq treemacs-display-current-project-exclusively t
;;         treemacs-project-follow-mode t)
;; ;; As long as there is exactly a single project in your workspace you can also use M-H and M-L (or treemacs-root-up and treemacs-root-down) to arbitrarily change the project’s root and freely navigate through your your file system, similar to dired. M-H will navigate one level upward in the file system, M-L will move into the directory at point.
;; )
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'right
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil
          treemacs-display-current-project-exclusively t)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

		(treemacs-load-theme "nerd-icons")
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
	)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t
	)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
	)

(use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs perspective) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives)
	)

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

(treemacs-start-on-boot)

;; Package Management Interface
(defun my/counsel-package-setup ()
  "Configure package display with rich annotations."
  (ivy-set-display-transformer 'counsel-package 'ivy-rich-counsel-package-transformer)
	)

(defun my/setup-package-commands ()
  "Ensure package commands display completions immediately."
  (dolist (cmd '(package-install package-delete counsel-package))
    (ivy-add-actions
     cmd
     '(("d" describe-package "describe package")
       ("h" helpful-package "helpful package"))))
  
  ;; Ensure package commands show candidates immediately
  (advice-add 'package-install :around
              (lambda (orig-fun &rest args)
                (let ((ivy-initial-inputs-alist nil)
                      (ivy-display-functions-alist nil))
                  (apply orig-fun args))))
  
  (advice-add 'package-delete :around
              (lambda (orig-fun &rest args)
                (let ((ivy-initial-inputs-alist nil)
                      (ivy-display-functions-alist nil))
                  (apply orig-fun args))))
	)

;; Add this to after-init-hook
(add-hook 'after-init-hook #'my/setup-package-commands)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)  
  :hook (after-init . ivy-mode)
	:bind (:map ivy-minibuffer-map
							("C-p" . dirvish-peek-mode))
  :config
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
																(counsel-package . ivy--regex-plus)
																(package-install . ivy--regex-plus)
																(package-delete . ivy--regex-plus)
																;; (t . ivy--regex-plus)
																(t . orderless-ivy-re-builder))
				
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-height 15
        ivy-display-style 'fancy
        ivy-dynamic-exhibit-delay 0.1)
  
  ;; This ensures commands like package-install show completions immediately
  (setq ivy-read-action-function #'ivy-read-action-by-key
        ivy-display-functions-alist '((t . ivy-display-function-fallback)))

  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
	)

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-parse-remote-buffer nil) ; Improve performance
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns ((nerd-icons-ivy-rich-buffer-icon :width 2)
                     (ivy-rich-candidate :width 40)
                     (ivy-rich-switch-buffer-size :width 7)
                     (ivy-rich-switch-buffer-indicators :width 4)
                     (ivy-rich-switch-buffer-major-mode :width 18)
                     (ivy-rich-switch-buffer-project :width 20)
                     (ivy-rich-switch-buffer-path :width 80)))
          package-install
          (:columns ((nerd-icons-ivy-rich-package-icon :width 2)
                     (ivy-rich-candidate :width 40)
                     (ivy-rich-package-version :width 16)
                     (ivy-rich-package-status :width 12)))
          package-delete
          (:columns ((nerd-icons-ivy-rich-package-icon :width 2)
                     (ivy-rich-candidate :width 40)
                     (ivy-rich-package-version :width 16)
                     (ivy-rich-package-status :width 12)))
          counsel-package
          (:columns ((nerd-icons-ivy-rich-package-icon :width 2)
                     (ivy-rich-candidate :width 40)
                     (ivy-rich-package-version :width 16)
                     (ivy-rich-package-status :width 12))))))


(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

;; Nerd icons integration for visual hierarchy
(use-package nerd-icons-ivy-rich
  :ensure t
  :after (ivy-rich nerd-icons)
  :config
  (nerd-icons-ivy-rich-mode 1)
  (setq nerd-icons-ivy-rich-icon-size 0.9
        nerd-icons-ivy-rich-icon-color "#6c71c4")
	)

;; (use-package ivy-file-preview
;;   :ensure t
;;   :after ivy
;;   :hook (ivy-mode . ivy-file-preview-mode)
;;   :config
;;   ;; You can customize whether to open in other window, delay, etc.:
;;   (setq ivy-file-preview-delay 0.2)
;; 	;; (setq ivy-file-preview-other-window t)
;; 	:bind (:map ivy-minibuffer-map
;;               ("C-v" . ivy-file-preview-dispatch))
;; 	;; :custom
;; 	;; (ivy-file-preview)
;; 	)

;; ;; Floating window display
;; (use-package ivy-posframe
;;   :ensure t
;;   :after ivy
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;         '((swiper          . ivy-posframe-display-at-point)
;;           (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
;;           (t               . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1)
;;   (setq ivy-posframe-width 120
;;         ivy-posframe-min-height 20
;;         ivy-posframe-border-width 2)
;;  )

;; Hydra-powered action menus
(use-package ivy-hydra
  :ensure t
  :after hydra
  :bind (:map ivy-minibuffer-map
							("C-o" . hydra-ivy/body))
  :config
  (defhydra hydra-ivy (:color pink :hint nil)
    "Ivy Actions"
    ("m" ivy-mark "Mark")
    ("u" ivy-unmark "Unmark")
    ("t" ivy-toggle-marks "Toggle marks")
    ("C" ivy-avy-action-copy "Copy text")
    ("R" ivy-avy-action-rename "Rename item")
    ("D" ivy-avy-action-delete "Delete item"))
	)

;; Git workflow enhancements
(use-package ivy-gitlab
  :ensure t
  :after ivy
  :config
  (setq ivy-gitlab-url "https://gitlab.com/api/v4"
        ivy-gitlab-token-file "~/.gitlab-token")
	)

;; Enhanced command suite with Counsel
(use-package counsel
  :ensure t
  :after ivy
  ;; :diminish
  :config (counsel-mode 1)
  :bind (("M-x" . counsel-M-x)                          ; Intelligent command dispatch
         ("C-x C-f" . counsel-find-file)                ; File navigation with preview
         ("C-c f" . counsel-recentf)                    ; Recent files with metadata
         ("C-c g" . counsel-git)                        ; Git file navigation
         ("C-c r" . counsel-rg))                        ; Ripgrep integration
  :hook (counsel-package . my/counsel-package-setup)
	)

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode 1)
  (setq counsel-projectile-sort-files t
        counsel-projectile-find-file-matcher 'ivy--regex-plus
        ;; Use a function that properly handles the project argument
        counsel-projectile-switch-project-action
        (lambda (project)
          (let ((projectile-switch-project-action 'projectile-dired))
            (projectile-switch-project-by-name project))))
  )

(use-package counsel-fd
	:ensure t
	:after counsel
)

(use-package counsel-css
	:ensure t
	:after counsel
)

;; Superior search experience with Swiper
(use-package swiper
  :ensure t
  :after ivy
	)

;; Quick navigation with Avy (similar to ivy-avy)
(use-package avy
  :ensure t
	)

;; Avy integration for quick navigation
(use-package ivy-avy
  :ensure t
  :after (ivy avy)
  :bind (:map ivy-minibuffer-map
							("C-;" . ivy-avy))
  :config
  (setq avy-all-windows t
        avy-background t)
	)

(use-package async
  :ensure t
	)

(use-package consult
  :ensure t
  :after ivy
  :config
  ;; consult has async-capable commands (consult-grep/consult-ripgrep, etc.)
  (setq consult-async-min-input 3)
	:custom
	(consult-fd "pattern" nil :require-match nil)
	(consult-rg "pattern" nil :require-match nil)
	)

(use-package harpoon
	:ensure t 
	;; :hook (after-init . harpoon-mode)
)

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1)
  (setq anzu-minimum-input 2)
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-to-string-separator " => ")
  (setq anzu-replace-at-cursor t)
  (setq anzu-use-migemo nil)
	)

(use-package evil-anzu
  :after (evil anzu)
	)

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
	)

(use-package expand-region
	:ensure t
)

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
  (which-key-allow-imprecise-window-fit nil)
	)

(use-package pdf-tools
  :ensure t
	:mode "\\.pdf\\'"
  :config
	(add-to-list 'revert-without-query ".pdf")
	(setq pdf-view-midnight-colors '("#CAD3F5" . "#24273A"))
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
	;; Enable themed mode automatically when viewing PDFs
	(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)		 
	(add-hook 'pdf-view-mode-hook
						(lambda ()
							(when display-line-numbers-mode
								(display-line-numbers-mode -1))))
	(setq epdfinfo-cache-passwords t)
  (setopt pdf-view-continuous nil)
  :bind (:map pdf-view-mode-map
							("\\" . hydra-pdftools/body)
							("<s-spc>" .  pdf-view-scroll-down-or-next-page)
							("g"  . pdf-view-first-page)
							("G"  . pdf-view-last-page)
							("l"  . image-forward-hscroll)
							("h"  . image-backward-hscroll)
							("j"  . pdf-view-next-page)
							("k"  . pdf-view-previous-page)
							("p"  . pdf-view-goto-page)
							("aa" . pdf-annot-attachment-dired) ;; if pdf has others connected view them in dired
							("al" . pdf-annot-list-annotations)
							("ad" . pdf-annot-delete)
							("am" . pdf-annot-add-markup-annotation)
							("at" . pdf-annot-add-text-annotation)
							("ah" . pdf-annot-add-highlight-markup-annotation)
							("au" . pdf-annot-add-underline-markup-annotation)
							("ac" . pdf-annot-add-strikeout-markup-annotation)
							("as" . pdf-annot-add-squiggly-markup-annotation)
							("y"  . pdf-view-kill-ring-save)
							("d"  . pdf-misc-display-metadata)
							("s"  . pdf-occur)
							("r"  . pdf-view-reset-slice) ;; check what this does
							("b"  . pdf-view-set-slice-from-bounding-box) ;; check what this does
							("u"  . pdf-view-revert-buffer) ;; check what this does
							)
	)

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red)
	)

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
	(setq pdf-view-restore-filename "~/.config/MainEmacs/.pdf-view-restore")
	)

(use-package org-pdftools
  :after (org pdf-tools)
  :ensure t
  :hook (org-mode . org-pdftools-setup-link)
	)

(use-package cheat-sh 
	:ensure t 
)

(use-package devdocs
	:ensure t
	:config
	;; make ./devdocs directory default 
	(setq devdocs-data-dir "~/.config/MainEmacs/DevDocs/")
)

(use-package org
	:config
	;; Custom org TODO sequence
	;; the | means anything before it is active and after it its complete
	(setq org-todo-keywords
				'((sequence "TODO(t)" "DOING(d)" "|" "DONE(d!)")
					(sequence "ISSUE(i)" "REVIEW(r)" "|" "CLOSED(c!)")
					(sequence "BUG(b)" "FIXING(f)" "|" "FIXED(k!)")
					(sequence "IDEA(e)" "PLANNING(p)" "|" "IMPLEMENTED(m!)")
					(sequence "STUDY(s)" "LEARNING(l)" "|" "LEARNED(w!)")
					))
	
	;; set some tags 
	(setq org-tag-alist '((:startgroup)
                        ("work" . ?w)
                        ("personal" . ?p)
                        ("school" . ?s)
                        ("project" . ?r)
                        ("urgent" . ?u)
                        ("computer" . ?c)
                        ))

	;; Define custom agenda commands
	(setq org-agenda-custom-commands
				'(("a" "ALL tasks" todo "" ((org-agenda-overriding-header "All TODOs")))
					("n" "Check not-started tasks"
					 todo ""
					 ((org-agenda-overriding-header "Not started TODOs")
						(org-agenda-skip-function
						 '(org-agenda-skip-entry-if
							 'todo '("DOING" "DONE" "REVIEW" "CLOSED" "FIXING" "FIXED"
											 "PLANNING" "IMPLEMENTED" "LEARNING" "LEARNED")))))
					("o" "Currently ongoing tasks"
					 todo ""
					 ((org-agenda-overriding-header "Working on TODOs")
						(org-agenda-skip-function
						 '(org-agenda-skip-entry-if
							 'todo '("TODO" "DONE" "ISSUE" "CLOSED" "BUG" "FIXED"
											 "IDEA" "IMPLEMENTED" "STUDY" "LEARNED")))))
					("f" "Finished tasks"
					 todo ""
					 ((org-agenda-overriding-header "Finished TODOs")
						(org-agenda-skip-function
						 '(org-agenda-skip-entry-if
							 'todo '("TODO" "DOING" "ISSUE" "REVIEW" "BUG" "FIXING"
											 "IDEA" "PLANNING" "STUDY" "LEARNING")))))

					;; General tag commands
					("T" "All tagged tasks" tags "+" ((org-agenda-overriding-header "All Tagged Tasks")))
					("t" "All tagged + TODO states"
					 tags-todo "+/+TODO=\"TODO|DOING|DONE|ISSUE|BUG|IDEA|STUDY|REVIEW|FIXING|PLANNING|CLOSED|FIXED|IMPLEMENTED|LEARNED\""
					 ((org-agenda-overriding-header "All Tagged Tasks (any TODO)")))

					;; -------------------------
					;; WORK family (uppercase prefix description + children)
					("w"  "All Work Tasks" tags-todo "+work" ((org-agenda-overriding-header "All Work Tasks")))
					("Wt" "Work — Not Started"
					 tags-todo "+work+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
					 ((org-agenda-overriding-header "Work (Not Started)")))
					("Wd" "Work — Doing"
					 tags-todo "+work+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
					 ((org-agenda-overriding-header "Work (Working)")))
					("Wc" "Work — Completed"
					 tags-todo "+work+TODO={DONE\\|CLOSED\\|FIXED\\|IMPLEMENTED\\|LEARNED}"
					 ((org-agenda-overriding-header "Work (Completed)")))

					;; PERSONAL
					("p"  "All Personal Tasks" tags-todo "+personal" ((org-agenda-overriding-header "All Personal Tasks")))
					("Pt" "Personal — Not Started"
					 tags-todo "+personal+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
					 ((org-agenda-overriding-header "Personal (Not Started)")))
					("Pd" "Personal — Doing"
					 tags-todo "+personal+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
					 ((org-agenda-overriding-header "Personal (Working)")))
					("Pc" "Personal — Completed"
					 tags-todo "+personal+TODO={DONE\\|CLOSED\\|FIXED\\|IMPLEMENTED\\|LEARNED}"
					 ((org-agenda-overriding-header "Personal (Completed)")))

					;; SCHOOL
					("s"  "All School Tasks" tags-todo "+school" ((org-agenda-overriding-header "All School Tasks")))
					("St" "School — Not Started"
					 tags-todo "+school+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
					 ((org-agenda-overriding-header "School (Not Started)")))
					("Sd" "School — Doing"
					 tags-todo "+school+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
					 ((org-agenda-overriding-header "School (Working)")))
					("Sc" "School — Completed"
					 tags-todo "+school+TODO={DONE\\|CLOSED\\|FIXED\\|IMPLEMENTED\\|LEARNED}"
					 ((org-agenda-overriding-header "School (Completed)")))

					;; PROJECT / RESEARCH
					("r"  "All Project Tasks" tags-todo "+project" ((org-agenda-overriding-header "All Project Tasks")))
					("Rt" "Project — Not Started"
					 tags-todo "+project+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
					 ((org-agenda-overriding-header "Project (Not Started)")))
					("Rd" "Project — Doing"
					 tags-todo "+project+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
					 ((org-agenda-overriding-header "Project (Working)")))
					("Rc" "Project — Completed"
					 tags-todo "+project+TODO={DONE\\|CLOSED\\|FIXED\\|IMPLEMENTED\\|LEARNED}"
					 ((org-agenda-overriding-header "Project (Completed)")))

					;; URGENT
					("u"  "All Urgent Tasks" tags-todo "+urgent" ((org-agenda-overriding-header "All Urgent Tasks")))
					("Ut" "Urgent — Not Started"
					 tags-todo "+urgent+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
					 ((org-agenda-overriding-header "Urgent (Not Started)")))
					("Ud" "Urgent — Doing"
					 tags-todo "+urgent+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
					 ((org-agenda-overriding-header "Urgent (Working)")))
					("Uc" "Urgent — Completed"
					 tags-todo "+urgent+TODO={DONE\\|CLOSED\\|FIXED\\|IMPLEMENTED\\|LEARNED}"
					 ((org-agenda-overriding-header "Urgent (Completed)")))
					
					;; COMPUTER
					("c"  "All Computer Tasks" tags-todo "+computer" ((org-agenda-overriding-header "All Computer Tasks")))
					("Ct" "Computer — Not Started"
					 tags-todo "+computer+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
					 ((org-agenda-overriding-header "Computer (Not Started)")))
					("Cd" "Computer — Doing"
					 tags-todo "+computer+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
					 ((org-agenda-overriding-header "Computer (Working)")))
					("Cc" "Computer — Completed"
					 tags-todo "+computer+TODO={DONE\\|CLOSED\\|FIXED\\|IMPLEMENTED\\|LEARNED}"
					 ((org-agenda-overriding-header "Computer (Completed)")))
					
					;; dashboard with agenda + doing todos + todo todos 
					("d" "Dashboard"
					 (
						(agenda "" 
										((org-agenda-span '14)
										 ;; (org-agenda-span 'month)           ; Show one month
                     (org-agenda-start-on-weekday 1)  ; Start Monday
										 (org-deadline-warning-days 14)
                     (org-agenda-overriding-header "Weekly Agenda / Calendar")))
						(tags-todo "+TODO={TODO\\|ISSUE\\|BUG\\|IDEA\\|STUDY}"
											 ((org-agenda-overriding-header "\nNext TODOs")))
						(tags-todo "+TODO={DOING\\|REVIEW\\|FIXING\\|PLANNING}"
											 ((org-agenda-overriding-header "\nActive TODOs")))
						))
					))

	;; campture templates

	(setq org-capture-templates
				;;  %? : place cursor here after capture; user adds description
				;;  %a : link to where the capture was initiated (contextual link)
				;;  %A : Custom description
				;;  %i : initial content, e.g., selected region
				;;  %U  : inactive timestamp with date and time (e.g. [2025-08-24 Sun 12:34])
				;;  %^T : prompt for timestamp (interactive) can combine with SCHEDULED: %^t DEADLINE: %^t to make a scedual for when to start till deadline
				;; [[%^{LINK}][%^{DESCRIPTION}]]\n : custom insersion with synstax %^{text} particual one to add links
        '(("t" "TODOS / IDEAS / ISUES")
          ("tu" "Urgent Issues" entry (file+headline "~/git/Emacs-Todos/TODO.org" "URGENT")
					 "* ISSUE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %i" :empty-lines 1)
          ("tw" "Work Todo" entry (file+headline "~/git/Emacs-Todos/TODO.org" "WORK")
					 "* TODO %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %i" :empty-lines 1)
          ("ts" "School Todo" entry (file+headline "~/git/Emacs-Todos/TODO.org" "SCHOOL")
					 "* TODO %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %i" :empty-lines 1)
          ("tp" "Personal Todo" entry (file+headline "~/git/Emacs-Todos/TODO.org" "PERSONAL")
					 "* TODO %?\n %a\n  %i" :empty-lines 1)
          ("tt" "Todo" entry (file+headline "~/git/Emacs-Todos/TODO.org" "TODOS")
					 "* TODO %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %i" :empty-lines 1)
          ("tW" "Work bugs" entry (file+headline "~/git/Emacs-Todos/TODO.org" "CODE_ERRORS_WORK")
					 "* BUG %?\n %F\n %U\n %a\n %i" :empty-lines 1)
          ("tS" "School bugs" entry (file+headline "~/git/Emacs-Todos/TODO.org" "CODE_ERRORS_SCHOOL")
					 "* BUG %?\n %F\n %a\n %i" :empty-lines 1)
          ("tP" "Personal bugs" entry (file+headline "~/git/Emacs-Todos/TODO.org" "CODE_ERRORS_PERSONAL")
					 "* BUG %?\n %F\n %a\n %i" :empty-lines 1)
          ("ti" "Ideas" entry (file+headline "~/git/Emacs-Todos/TODO.org" "IDEAS")
					 "* IDEA %?\n %U\n %a\n %i" :empty-lines 1)
          ("tl" "Linux issues os todos" entry (file+headline "~/git/Emacs-Todos/TODO.org" "LINUX_TODOS_BUGS")
					 "* REVIEW %?\n %F\n %a\n %i" :empty-lines 1)
          ("tL" "Stuff to learn" entry (file+headline "~/git/Emacs-Todos/TODO.org" "LEARN")
					 "* STUDY %?\n [[%^{LINK}][%^{DESCRIPTION}]]\n %i" :empty-lines 1)
					;; ---- Notes ---- ;;
					("n" "Larger NOTEs")
          ("nw" "Work Note" entry (file+headline "~/git/Emacs-Todos/NOTES.org" "WORK")
					 "* NOTE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %A\n %i\n %?" :empty-lines 1)
          ("ns" "School Note" entry (file+headline "~/git/Emacs-Todos/NOTES.org" "SCHOOL")
					 "* NOTE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %A\n %i\n %?" :empty-lines 1)
          ("np" "Personal Note" entry (file+headline "~/git/Emacs-Todos/NOTES.org" "PERSONAL")
					 "* NOTE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %A\n %i\n %?" :empty-lines 1)
          ("nW" "Work Note with custom link" entry (file+headline "~/git/Emacs-Todos/NOTES.org" "WORK")
					 "* NOTE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %A\n %i\n [[%^{LINK}][%^{DESCRIPTION}]]\n %?" :empty-lines 1)
          ("nS" "School Note With custom link" entry (file+headline "~/git/Emacs-Todos/NOTES.org" "SCHOOL")
					 "* NOTE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %A\n %i\n [[%^{LINK}][%^{DESCRIPTION}]]\n %?" :empty-lines 1)
          ("nP" "Personal Note With custom link" entry (file+headline "~/git/Emacs-Todos/NOTES.org" "PERSONAL")
					 "* NOTE %?\n %U\n SCHEDULED: %^t DEADLINE: %^t\n %a\n %A\n %i\n [[%^{LINK}][%^{DESCRIPTION}]]\n %?" :empty-lines 1)
					;; ---- Calendar ---- ;;
					("c" "Calendar")
					("cp" "pappanos004@gmail.com")
					("cpg" "General" entry (file+headline "~/git/Emacs-Todos/emails/pappanos-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tpappanos004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cpa" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/pappanos-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tpappanos004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cpp" "Project" entry (file+headline "~/git/Emacs-Todos/emails/pappanos-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\tpappanos004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cpe" "Exam" entry (file+headline "~/git/Emacs-Todos/emails/pappanos-mails.org" "Exams")
					 "* %^{Exam name}\n:PROPERTIES:\n:calendar-id:\tpappanos004@gmail.com\n:END:\n%^{Exam date and time}T\n:org-gcal:\n%^{Exam date and time}T\n:END:\n\n" :jump-to-captured t)
					("cpb" "Birthday" entry (file+headline "~/git/Emacs-Todos/emails/pappanos-mails.org" "Birthdays")
					 "* %^{Name}'s Birthday\n:PROPERTIES:\n:calendar-id:pappanos004@gmail.com\n:END:\n:org-gcal:\n%(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:BDAY: %(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:END:\n\n" :jump-to-captured t)

					("cu" "user.idc004@gmail.com")
					("cug" "General" entry (file+headline "~/git/Emacs-Todos/emails/user-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tuser.idc004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cua" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/user-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tuser.idc004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cup" "Project" entry (file+headline "~/git/Emacs-Todos/emails/user-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\tuser.idc004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cue" "Exam" entry (file+headline "~/git/Emacs-Todos/emails/user-mails.org" "Exams")
					 "* %^{Exam name}\n:PROPERTIES:\n:calendar-id:\tuser.idc004@gmail.com\n:END:\n%^{Exam date and time}T\n:org-gcal:\n%^{Exam date and time}T\n:END:\n\n" :jump-to-captured t)
					("cub" "Birthday" entry (file+headline "~/git/Emacs-Todos/emails/user-mails.org" "Birthdays")
					 "* %^{Name}'s Birthday\n:PROPERTIES:\n:calendar-id:user.idc004@gmail.com\n:END:\n:org-gcal:\n%(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:BDAY: %(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:END:\n\n" :jump-to-captured t)

					("cw" "pappanos.work.004@gmail.com")
					("cwg" "General" entry (file+headline "~/git/Emacs-Todos/emails/work-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tpappanos.work.004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cwa" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/work-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tpappanos.work.004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cwp" "Project" entry (file+headline "~/git/Emacs-Todos/emails/work-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\tpappanos.work.004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cwd" "Deadline" entry (file+headline "~/git/Emacs-Todos/emails/work-mails.org" "Deadlines")
					 "* %^{Deadline name}'s Deadline\n:PROPERTIES:\n:calendar-id:\tpappanos.work.004@gmail.com\n:END:\n%^{Deadline date and time}T\n:org-gcal:\n%^{Deadline date and time}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cwm" "Meeting" entry (file+headline "~/git/Emacs-Todos/emails/work-mails.org" "Meetings")
					 "* %^{Meeting name}'s Meeting\n:PROPERTIES:\n:calendar-id:pappanos.work.004@gmail.com\n:END:\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)

					("cg" "pappanos.gamedev.004@gmail.com")
					("cgg" "General" entry (file+headline "~/git/Emacs-Todos/emails/gamedev-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tpappanos.gamedev.004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cga" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/gamedev-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tpappanos.gamedev.004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cgp" "Project" entry (file+headline "~/git/Emacs-Todos/emails/gamedev-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\tpappanos.gamedev.004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cwd" "Deadline" entry (file+headline "~/git/Emacs-Todos/emails/gamedev-mails.org" "Deadlines")
					 "* %^{Deadline name}'s Deadline\n:PROPERTIES:\n:calendar-id:\tpappanos.gamedev.004@gmail.com\n:END:\n%^{Deadline date and time}T\n:org-gcal:\n%^{Deadline date and time}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cgr" "Realease Date" entry (file+headline "~/git/Emacs-Todos/emails/gamedev-mails.org" "Realease-Dates")
					 "* %^{Realease}'s Date\n:PROPERTIES:\n:calendar-id:pappanos.gamedev.004@gmail.com\n:END:\n:org-gcal:\n%^{Realese date and time}T\n:org-gcal:\n%^{Realease date and time}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cgu" "Update" entry (file+headline "~/git/Emacs-Todos/emails/gamedev-mails.org" "Updates")
					 "* %^{Update}'s Update\n:PROPERTIES:\n:calendar-id:pappanos.gamedev.004@gmail.com\n:END:\n:org-gcal:\n%^{Update date and time}T\n:org-gcal:\n%^{Update date and time}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)

					("cm" "lets.mine004@gmail.com")
					("cmg" "General" entry (file+headline "~/git/Emacs-Todos/emails/mine-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tlets.mine004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cma" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/mine-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\tlets.mine004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cmp" "Project" entry (file+headline "~/git/Emacs-Todos/emails/mine-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\tlets.mine004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cme" "Exam" entry (file+headline "~/git/Emacs-Todos/emails/mine-mails.org" "Exams")
					 "* %^{Exam name}\n:PROPERTIES:\n:calendar-id:\tlets.mine004@gmail.com\n:END:\n%^{Exam date and time}T\n:org-gcal:\n%^{Exam date and time}T\n:END:\n\n" :jump-to-captured t)
					("cmb" "Birthday" entry (file+headline "~/git/Emacs-Todos/emails/mine-mails.org" "Birthdays")
					 "* %^{Name}'s Birthday\n:PROPERTIES:\n:calendar-id:lets.mine004@gmail.com\n:END:\n:org-gcal:\n%(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:BDAY: %(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:END:\n\n" :jump-to-captured t)

					("ce" "euro.money004@gmail.com")
					("ceg" "General" entry (file+headline "~/git/Emacs-Todos/emails/money-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\teuro.money004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cea" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/money-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\teuro.money004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cep" "Project" entry (file+headline "~/git/Emacs-Todos/emails/money-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\teuro.money004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("cee" "Exam" entry (file+headline "~/git/Emacs-Todos/emails/money-mails.org" "Exams")
					 "* %^{Exam name}\n:PROPERTIES:\n:calendar-id:\teuro.money004@gmail.com\n:END:\n%^{Exam date and time}T\n:org-gcal:\n%^{Exam date and time}T\n:END:\n\n" :jump-to-captured t)
					("ceb" "Birthday" entry (file+headline "~/git/Emacs-Todos/emails/money-mails.org" "Birthdays")
					 "* %^{Name}'s Birthday\n:PROPERTIES:\n:calendar-id:euro.money004@gmail.com\n:END:\n:org-gcal:\n%(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:BDAY: %(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:END:\n\n" :jump-to-captured t)

					("co" "overwhatch.004@gmail.com")
					("cog" "General" entry (file+headline "~/git/Emacs-Todos/emails/overwatch-mails.org" "General")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\toverwhatch.004@gmail.com\n:END:\n%^{Date and time}T\n:org-gcal:\n%^{Date and time}T--%^{End time (optional)}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("coa" "Appointment" entry (file+headline "~/git/Emacs-Todos/emails/overwatch-mails.org" "Appointments")
					 "* %^{Description}\n:PROPERTIES:\n:calendar-id:\toverwhatch.004@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
					("cop" "Project" entry (file+headline "~/git/Emacs-Todos/emails/overwatch-mails.org" "Projects")
					 "* %^{Project name}\n:PROPERTIES:\n:calendar-id:\toverwhatch.004@gmail.com\n:END:\nStart: %^{Start date}t\nEnd: %^{Due date}t\n:org-gcal:\n%^{Start date}T--%^{Due date}T\n%^{Description}\n:END:\n\n" :jump-to-captured t)
					("coe" "Exam" entry (file+headline "~/git/Emacs-Todos/emails/overwatch-mails.org" "Exams")
					 "* %^{Exam name}\n:PROPERTIES:\n:calendar-id:\toverwhatch.004@gmail.com\n:END:\n%^{Exam date and time}T\n:org-gcal:\n%^{Exam date and time}T\n:END:\n\n" :jump-to-captured t)
					("cob" "Birthday" entry (file+headline "~/git/Emacs-Todos/emails/overwatch-mails.org" "Birthdays")
					 "* %^{Name}'s Birthday\n:PROPERTIES:\n:calendar-id:overwhatch.004@gmail.com\n:END:\n:org-gcal:\n%(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:BDAY: %(concat \"<\" (format-time-string \"%Y-%m-%d\") \" +1y>\")\n:END:\n\n" :jump-to-captured t)

					))

	(setq org-emphasis-alist
				'(("*" (bold :slant italic :weight black :foreground "cyan" ))
					("/" (italic :foreground "violet" ))
					("_" (underline :background "purple3" :foreground "white"))
					("=" (:background "RoyalBlue" :foreground "white" ))
					("~" (:background "DarkCyan" :foreground "white" ))
					("+" (:strike-through t :background"maroon" :foreground "white" ))
					))
	)

;; (use-package toc-org
;; 	:commands toc-org-enable
;; 	:init (add-hook 'org-mode-hook 'toc-org-enable)
;; 	:hook (org-mode . toc-org-mode))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable)
)

(use-package org-modern
  :init (add-hook 'org-mode-hook 'org-modern-mode)
	(setq org-modern-list        nil    ; turn off plain-list bullets
				org-modern-star        nil    ; turn off headline-star replacement
				org-modern-hide-stars  nil)   ; don’t hide any stars
	)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars t)
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("●" "○" "■" "●" "○" "■")) ;; replace the * with this symbols
	)

(setq org-hide-emphasis-markers t);; hide the * + _ ~ etc when you use them
;; Disable org-indent-mode by default (stops outline-style indentation)
(setq org-startup-indented nil)
;; Disable electric indentation in Org mode
(add-hook 'org-mode-hook
					(lambda ()
						(electric-indent-local-mode -1))
					)
;; Prevent Org from adapting indentation to outline structure
(setq org-adapt-indentation nil)
(setq org-agenda-files
      (append
       ;; All org files inside ~/git/Emacs-Todos/
       (directory-files-recursively "~/git/Emacs-Todos/" "\\.org$")
       ;; All org files inside ~/git/emacs-notes/ (recursive)
       (directory-files-recursively "~/git/emacs-notes/" "\\.org$"))
			)

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
												:height (cdr face-height)))
	)

(add-hook 'org-mode-hook #'my/org-mode-header-font-setup)

(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-screenshot-method "flameshot gui -p %s")
  (org-download-image-dir "~/git/emacs-notes/images/")
  (org-download-heading-lvl nil)
  (org-download-method 'directory)
  (org-download-annotate-function 'org-download-default-annotate-function)
	)

(use-package org-remoteimg
  :ensure t                              ;; install if not present
  :vc   (:url   "https://github.com/gaoDean/org-remoteimg.git"
								:rev   :newest)                ;; clone tip of default branch
  :after org                             ;; load only after Org mode is ready
  :config
  ;; 3 Enable remote-image caching
  (setq org-display-remote-inline-images 'cache)  ;; cache first fetc
	)

(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 120)  ; Adjust 80 to your preferred text width
	)

(use-package ob-mermaid
	:ensure t 
	:config
	(setq ob-mermaid-cli-path "mmdc")
	)

(use-package diagram-preview
	  :vc (:url "https://github.com/natrys/diagram-preview.git"
            :rev  :newest)
  :ensure nil
  :hook ((graphviz-dot-mode plantuml-mode mermaid-mode pikchr-mode d2-mode) . diagram-preview-mode)
	)

(use-package org
  :ensure t
  :custom
  (org-edit-src-content-indentation 2)
  :hook (org-mode . org-indent-mode)
	)

(with-eval-after-load 'org
  ;; No need for (require 'org-tempo) in Org 9.2+
  (add-to-list 'org-structure-template-alist '("cs" . "src shell"))
  (add-to-list 'org-structure-template-alist '("ce" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cj" . "src jsx"))
  (add-to-list 'org-structure-template-alist '("ct" . "src tsx"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cp" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("cJ" . "src java"))
  (add-to-list 'org-structure-template-alist '("cl" . "src lua"))
  (add-to-list 'org-structure-template-alist '("cm" . "src makefile"))
  (add-to-list 'org-structure-template-alist '("cd" . "src mermaid :file test.png"))
	)

(with-eval-after-load 'org
	(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell       . t)
     (js          . t)    ;; for jsx
     (C           . t)    ;; provides C, C++, D
     (java        . t)
     (lua         . t)
		 (mermaid     . t)
     ;; makefile source blocks often work as shell scripts
     ;; depending on your setup; Org doesn't have explicit make support
     ))
  
  ;; Optional: highlight known extension modes
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
	)

(use-package imenu-list
  :config
    (setq imenu-list-minor-mode 1)
		(setq imenu-list-focus-after-activation t)
		(setq imenu-list-auto-resize t)
		(setq imenu-list-idle-update-delay-time 0.2)
		(setq imenu-list-position "right")
    (setq imenu-list-size 0.75)
		)

(use-package org-krita
  :vc (:url "https://github.com/lepisma/org-krita.git"
            :rev  :newest)
  :config
	(add-hook 'org-mode-hook 'org-krita-mode)
	)

(with-eval-after-load 'org
  (let ((png (cdr (assq 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler
               '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter
               '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter
               '("dvipng -D %D -T tight -bg Transparent -o %O %F")))
	)

(use-package org-transclusion
  :after org
  )

(use-package org-roam
  :ensure t
  :custom
	(make-directory "~/git/emacs-notes/")
  (org-roam-directory (file-truename "~/git/emacs-notes/"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
	(setq org-roam-db-gc-threshold most-positive-fixnum)

  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
	)

(use-package org-roam-ui
    :after org-roam
		;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
)

(use-package org-appear 
	:commands (org-appear-mode)
	:hook (org-mode . org-appear-mode)
	:init
	(setq org-hide-emphasis-markers t)
	(setq org-appear-autoemphasis t)
	(setq org-appear-autolinks t)
	(setq org-appear-autosubmarkers t)
	)

;; calf and gcal
(use-package calfw
	:ensure t
	:init (cfw:org-clean-exit)
  :bind (:map cfw:calendar-mode-map
							("SPC-q" . cfw:org-clean-exit)
							
							;; [left], b, h	Previous day
							;; [right], f, l	Next day
							;; [up], p, k	Previous week
							;; [down], n, j	Next week
							;; ^	Week begin
							;; $	Week end
							;; [home]	First date in this month
							;; [end]	Last date in this month
							;; M-v, [PgUp], <	Previous month
							;; C-v, [PgDown], >	Next month
							;; t	Today
							;; g	Absolute date (YYYY/MM/DD)
							;; TAB	Next item in a day
							;; Changing View	
							;; M	Month view
							;; W	1 Week view
							;; T	2 Week view
							;; D	Day view
							;; Operation	
							;; r	Refresh data and re-draw contents
							;; SPC	Pop-up detail buffer (like Quicklook in Mac)
							;; RET, [click]	Jump (howm, orgmode)
							;; q	Bury buffer
							)
	:config

	;; ;; ;; Unicode characters
	;; (setq cfw:fchar-junction ?╋
	;; 			cfw:fchar-vertical-line ?┃
	;; 			cfw:fchar-horizontal-line ?━
	;; 			cfw:fchar-left-junction ?┣
	;; 			cfw:fchar-right-junction ?┫
	;; 			cfw:fchar-top-junction ?┯
	;; 			cfw:fchar-top-left-corner ?┏
	;; 			cfw:fchar-top-right-corner ?┓)

	;; ;; Unicode characters
	(setq cfw:fchar-junction ?╬
	      cfw:fchar-vertical-line ?║
	      cfw:fchar-horizontal-line ?═
	      cfw:fchar-left-junction ?╠
	      cfw:fchar-right-junction ?╣
	      cfw:fchar-top-junction ?╦
	      cfw:fchar-top-left-corner ?╔
	      cfw:fchar-top-right-corner ?╗)
	
	(defun my-open-calendar ()
	  (interactive)
	  (cfw:open-calendar-buffer
	   :contents-sources
	   (list
	    (cfw:org-create-source "cyan3")  ; orgmode source
	    ;; (cfw:howm-create-source "Blue")  ; howm source
	    ;; (cfw:cal-create-source "Orange") ; diary source
	    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
			(cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/user.idc004%40gmail.com/private-a9c27b6aeffca1f9755f2a3a12d2bb57/basic.ics" "cyan") ; google calendar ICS
			(cfw:open-ical-calendar "https://calendar.google.com/calendar/ical/user.idc004%40gmail.com/private-a9c27b6aeffca1f9755f2a3a12d2bb57/basic.ics" "yellow") ; google calendar ICS
			))) 

	;; Month
	;; (setq calendar-month-name-array
	;;   ["January" "February" "March"     "April"   "May"      "June"
	;;    "July"    "August"   "September" "October" "November" "December"])

	;; Week days
	(setq calendar-day-name-array
	      ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

	;; First day of the week
	(setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

	)

;; calfw-org.el : Display org schedules
(use-package calfw-org
	:ensure t
	:after calfw
  )

;; calfw-ical.el : Display schedules of the iCalendar format, such as the google calendar.
(use-package calfw-ical
  :ensure t
  :after calfw
	:config
	(cfw:open-ical-calendar "https://calendar.google.com/calendar/ical/user.idc004%40gmail.com/private-a9c27b6aeffca1f9755f2a3a12d2bb57/basic.ics")
	;; Go to https://calendar.google.com/.
	;; Open the settings menu (gear-wheel -> settings).
	;; In settings for my calendar click on your name.
	;; There unfolds a menu containing one menu item "Integrate calendar"
	;; There you find "Secret address in iCal format". Replace the argument of cfw:open-ical-calendar with that string.
  )

;; Edit google calendar for calfw
(use-package calfw-gcal
	:ensure t
	:after (calfw)
	:config
)


(use-package calfw-ical 
	:ensure t 
	:after (calfw)
	:config 
	(cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/user.idc004%40gmail.com/private-a9c27b6aeffca1f9755f2a3a12d2bb57/basic.ics" "cyan") ; google calendar ICS
)

;; gcal dependencies 
(use-package request
  :ensure t)

(use-package alert
  :ensure t)

(use-package persist
  :ensure t)

(use-package aio
  :ensure t)

(use-package oauth2-auto
  :vc (:url "https://github.com/telotortium/emacs-oauth2-auto.git"
            :rev  :newest)
	)

(use-package org-gcal
	:ensure t 
	:init
	(setq org-gcal-client-id "127662390628-1klrqs7f13h8fsp7ithb9v30oiq3vpmn.apps.googleusercontent.com"
				org-gcal-client-secret "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"
				org-gcal-fetch-file-alist '(("user.idc004@gmail.com" .  "~/git/Emacs-Todos/emails/user-mails.org")
																		("pappanos.004@gmail.com" .  "~/git/Emacs-Todos/emails/pappanos-mails.org")
																		("lets.mine004@gmail.com" .  "~/git/Emacs-Todos/emails/mine-mails.org")
																		("euro.money004@gmail.com" .  "~/git/Emacs-Todos/emails/money-mails.org")
																		("overwhatch.004@gmail.com" .  "~/git/Emacs-Todos/emails/overwatch-mails.org")
																		("pappanos.work.004@gmail.com" .  "~/git/Emacs-Todos/emails/work-mails.org")
																		("pappanos.gamedev.004@gmail.com" .  "~/git/Emacs-Todos/emails/gamedev-mails.org")
																		;; ("sdi2200224@di.uoa.gr" .  "~/git/Emacs-Todos/emails/school-mails.org")
																		))
	
	)
;; ;; emacs-calendar google client id
;; 127662390628-1klrqs7f13h8fsp7ithb9v30oiq3vpmn.apps.googleusercontent.com

;; ;; client secret
;; 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

(provide 'package_configs)
