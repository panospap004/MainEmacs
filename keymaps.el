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
  (evil-want-C-u-scroll nil)   ;; Allow C-u scrolling up
  (evil-want-C-d-scroll nil)
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
  (general-create-definer start/global
    :states '(normal insert visual motion emacs)
    :keymaps 'override)

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
  (general-create-definer start/Nvim-Keys-normal-and-visual
    :states '(normal visual)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-normal-and-insert
    :states '(normal insert)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-insert-visual
    :states '(insert visual)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-normal-only
    :states '(normal)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-visual-only
    :states '(visual)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/Nvim-Keys-insert-only
    :states '(insert)
    :keymaps 'override)
  
  ;; Code keybinds definer (no prefix)
  (general-create-definer start/emacs-motion
    :states '(emacs motion)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/emacs-only
    :states '(emacs)
    :keymaps 'override)

  ;; Code keybinds definer (no prefix)
  (general-create-definer start/motion-only
    :states '(motion)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-all-nvim
    :states '(normal insert visual emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-normal-visual
    :states '(normal visual emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-normal-isert
    :states '(normal insert emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-insert-visual
    :states '(insert visual emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-normal
    :states '(normal emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-visual
    :states '(visual emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/emacs-insert
    :states '(insert emacs)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-all-nvim
    :states '(normal insert visual motion)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-normal-visual
    :states '(normal visual motion)
    :keymaps 'override)
	
  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-normal-insert
    :states '(normal insert motion)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-insert-visual
    :states '(insert visual motion)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-normal
    :states '(normal motion)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-visual
    :states '(visual motion)
    :keymaps 'override)

  ;; Global keybinds definer (no prefix)
  (general-create-definer start/motion-insert
    :states '(normal insert visual motion emacs)
    :keymaps 'override)
)

(general-define-key
  :states '(normal motion emacs)
  :keymaps 'dired-mode-map
    "h" 'my/dired-up-directory
    "l" 'my/dired-open-item
    "<left>" 'my/dired-up-directory
    "<right>" 'my/dired-open-item
    "C-h" 'dired-hide-dotfiles-mode
    "C-r" 'dired-rsync)

;; Defuns 
;; Define functions for shifting left and right without restoring cursor position
(defun my/evil-shift-right-and-restore ()
  "Shift region right by 2 spaces, keep the cursor position, and stay in Visual mode."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (cursor-pos (point))) ;; Save the current cursor position
    (evil-shift-right start end)
    (goto-char cursor-pos) ;; Restore the cursor position
    (evil-visual-restore))) ;; Re-enter Visual mode

(defun my/evil-shift-left-and-restore ()
  "Shift region left by 2 spaces, keep the cursor position, and stay in Visual mode."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (cursor-pos (point))) ;; Save the current cursor position
    (evil-shift-left start end)
    (goto-char cursor-pos) ;; Restore the cursor position
    (evil-visual-restore))) ;; Re-enter Visual mode

;; Set the shift width to 2 instead of the default 4
(setq evil-shift-width 2)
(setq-default tab-width 2)

(use-package general
      :config
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
      "S-e" '(:ignore t :wk "Eglot Evaluate")
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
      "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
)

    (start/global
      "C-<down>" 'evil-window-down   ;; Move to the window below
      "C-<up>" 'evil-window-up       ;; Move to the window above
      "C-<left>" 'evil-window-left   ;; Move to the window on the left
      "C-<right>" 'evil-window-right ;; Move to the window on the right
    )

(start/global-keys-no-insert
  "<escape>" 'keyboard-escape-quit
  "C-<tab>" 'switch-to-next-buffer
  "C-S-<iso-lefttab>" 'switch-to-prev-buffer
  "C-SPC p" 'projectile-command-map))

(start/Nvim-Keys
   "C-z" 'evil-undo
   "C-r" 'evil-redo
   "M-a" (lambda () (interactive) (evil-goto-first-line) (evil-visual-line) (evil-goto-line) (move-end-of-line nil))
)

;; Keybindings matching Neovim behavior
(start/Nvim-Keys-normal-and-visual
  "S-<up>"   'drag-stuff-up
	"S-<down>" 'drag-stuff-down
)





(start/Nvim-Keys-normal-only
  "C-s" nil
  "C-s" (lambda () (interactive) (save-buffer) (org-babel-tangle))
)

;; Remap < and > to the custom functions
(start/Nvim-Keys-visual-only
  "<" nil
  ">" nil
  "<" 'my/evil-shift-left-and-restore
  ">" 'my/evil-shift-right-and-restore
  "S-<down>" nil
  "S-<up>" nil
  "<S-up>" 'my/evil-move-lines-up
  "<S-down>" 'my/evil-move-lines-down
)

(start/Nvim-Keys-insert-only
   "TAB" nil
   "S-TAB" nil
;;   "TAB" 'tab-to-tab-stop
;;   "S-TAB" 'corfu-next
)

(start/emacs-motion			
  "M-'" 'eval-expression				
 )













(start/emacs-normal
	"SPC u p" 'package-upgrade-all
  "SPC q" 'kill-buffer-and-window
	"SPC e" nil
  "SPC e" 'treemacs
	"t t" 'eat
	;; "t t" 'vterm
)



















(provide 'keymaps)
