;; keymaps.el --- Keybinding configuration

;; Make sure you have already loaded the packages that these depend on.
;; For example, Evil, Evil-Collection, and General.
;; (These packages should be installed or available via your package manager.)

;; Evil configuration (key-related parts)
(use-package evil
  :init
  (evil-mode)
  :config
  ;; Set initial state for 'eat-mode to 'insert
  (evil-set-initial-state 'eat-mode 'insert)
  :custom
  (evil-want-keybinding nil)   ;; Disable default keybindings in some modes
  (evil-want-C-u-scroll t)       ;; Allow C-u scrolling up
  (evil-want-C-i-jump nil)       ;; Disable C-i jump
  (evil-undo-system 'undo-redo)  ;; Use the modern undo system
  (org-return-follows-link t)    ;; Make RETURN follow links in Org mode
  :bind (:map evil-motion-state-map
              ("SPC" . nil)
              ("RET" . nil)
              ("TAB" . nil)))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult))
  (evil-collection-init))

;; General (for leader key and custom keybindings)
(use-package general
  :config
  (general-evil-setup)
  ;; Leader key definer
  (general-create-definer start/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/global-keys-no-insert
    :states '(normal visual motion emacs)
    :keymaps 'override)
  
  ;; Global keybinds definer (no prefix)
  (general-create-definer start/global-keys-no-motion
    :states '(normal visual insert emacs)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys
    :states '(normal insert visual)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-insert-only
    :states '(insert)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-visual-only
    :states '(visual)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-normal-only
    :states '(normal)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-normal-and-visual
    :states '(normal visual)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-normal-and-insert
    :states '(normal insert)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-visual-insert
    :states '(insert visual)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/emacs-only
    :states '(emacs)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/motion-only
    :states '(motion)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/emacs-motion
    :states '(emacs motion)
    :keymaps 'override)

  ;; Global keybindings using the leadeir key:
  (start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "p" '(projectile-command-map :wk "Projectile command map"))

  (start/leader-keys
    "f" '(:ignore t :wk "Find")
    "f c" '((lambda () (interactive)
              (find-file "~/.config/MainEmacs/config.org"))
            :wk "Edit Emacs config")
    "f r" '(consult-recent-file :wk "Recent files")
    "f f" '(consult-fd :wk "Fd search for files")
    "f g" '(consult-ripgrep :wk "Ripgrep search in files")
    "f l" '(consult-line :wk "Find line")
    "f i" '(consult-imenu :wk "Imenu buffer locations"))

  (start/leader-keys
    "b" '(:ignore t :wk "Buffer Bookmarks")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b j" '(consult-bookmark :wk "Bookmark jump"))

  (start/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current"))

  (start/leader-keys
    "e" '(:ignore t :wk "Eglot Evaluate")
    "e e" '(eglot-reconnect :wk "Eglot Reconnect")
    "e f" '(eglot-format :wk "Eglot Format")
    "e l" '(consult-flymake :wk "Consult Flymake")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Magit status"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help")
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.config/MainEmacs/init.el"))
            :wk "Reload Emacs config"))

  (start/leader-keys
    "s" '(:ignore t :wk "Show")
    "s e" '(eat :wk "Eat terminal"))

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers"))

  ;; Global keybindings (no leader prefix)
  (start/global-keys-no-insert
    "<escape>" 'keyboard-escape-quit
    "C-<tab>" 'switch-to-next-buffer
    "C-S-<iso-lefttab>" 'switch-to-prev-buffer
    "C-SPC p" 'projectile-command-map))      
      
  (start/emacs-motion			
    "M-'" 'eval-expression				
   )

(provide 'keymaps)
