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

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired ibuffer magit ivy company corfu vertico consult mu4e mu4e-conversation nov))
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
  (general-create-definer start/emacs-motion-normal
    :states '(emacs motion normal)
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

;; Defuns 
(require 'windmove)

(defconst buffer-move-version "0.6.3"
  "Version of buffer-move.el")

(defgroup buffer-move nil
  "Swap buffers without typing C-x b on each window"
  :group 'tools)

(defcustom buffer-move-behavior 'swap
  "If set to 'swap (default), the buffers will be exchanged
  (i.e. swapped), if set to 'move, the current window is switch back to the
  previously displayed buffer (i.e. the buffer is moved)."
  :group 'buffer-move
  :type 'symbol)

(defcustom buffer-move-stay-after-swap nil
  "If set to non-nil, point will stay in the current window
  so it will not be moved when swapping buffers. This setting
  only has effect if `buffer-move-behavior' is set to 'swap."
  :group 'buffer-move
  :type 'boolean)

(defun buf-move-to (direction)
  "Helper function to move the current buffer to the window in the given
  direction (with must be 'up, 'down', 'left or 'right). An error is
  thrown, if no window exists in this direction."
  (cl-flet ((window-settings (window)
              (list (window-buffer window)
                    (window-start window)
                    (window-hscroll window)
                    (window-point window)))
            (set-window-settings (window settings)
              (cl-destructuring-bind (buffer start hscroll point)
                  settings
                (set-window-buffer window buffer)
                (set-window-start window start)
                (set-window-hscroll window hscroll)
                (set-window-point window point))))
    (let* ((this-window (selected-window))
           (this-window-settings (window-settings this-window))
           (other-window (windmove-find-other-window direction))
           (other-window-settings (window-settings other-window)))
      (cond ((null other-window)
             (error "No window in this direction"))
            ((window-dedicated-p other-window)
             (error "The window in this direction is dedicated"))
            ((window-minibuffer-p other-window)
             (error "The window in this direction is the Minibuffer")))
      (set-window-settings other-window this-window-settings)
      (if (eq buffer-move-behavior 'move)
          (switch-to-prev-buffer this-window)
        (set-window-settings this-window other-window-settings))
      (select-window other-window))))

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
  If there is no split, ie now window above the current one, an
  error is signaled."
  (interactive)
  (buf-move-to 'up))

;;;###autoload
(defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
  If there is no split, ie now window under the current one, an
  error is signaled."
  (interactive)
  (buf-move-to 'down))

;;;###autoload
(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
  If there is no split, ie now window on the left of the current
  one, an error is signaled."
  (interactive)
  (buf-move-to 'left))

;;;###autoload
(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
  If there is no split, ie now window on the right of the current
  one, an error is signaled."
  (interactive)
  (buf-move-to 'right))

;;;###autoload
(defun buf-move ()
  "Begin moving the current buffer to different windows.

Use the arrow keys to move in the desired direction.  Pressing
any other key exits this function."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (dolist (x '(("<up>" . buf-move-up)
                 ("<left>" . buf-move-left)
                 ("<down>" . buf-move-down)
                 ("<right>" . buf-move-right)))
      (define-key map (read-kbd-macro (car x)) (cdr x)))
    (set-transient-map map t)))

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

(defun my/persp-create-defaults ()
  "Ensure the seven default perspectives exist, in a fixed order, and switch back to Default."
  (interactive)
  (let ((my-persp-names
         '("Random" "personal" "work" "webdev" "gamedev" "dotfiles" "Default")))
    (dolist (name my-persp-names)
      (persp-new name))
    (persp-switch "Default"))
)

(defvar my/neotree-clipboard nil
  "Holds the last cut or copied node for later pasting.
Structure is (ACTION . PATH), where ACTION is 'copy or 'cut.")

(defun my/neotree-cut-node ()
  "Cut the node at point for a later move."
  (interactive)
  (let ((path (neo-buffer--get-filename-current-line)))
    (unless path
      (user-error "No node at point"))
    (setq my/neotree-clipboard (cons 'cut path))
    (message "Cut: %s" (file-name-nondirectory path))))

(defun my/neotree-paste-node ()
  "Paste the previously cut or copied node into the current directory."
  (interactive)
  (unless my/neotree-clipboard
    (user-error "Clipboard is empty"))
  (let* ((action (car my/neotree-clipboard))
         (src    (cdr my/neotree-clipboard))
         (dest   (let ((pt (neo-buffer--get-filename-current-line)))
                   (if (file-directory-p pt)
                       pt
                     (file-name-directory pt))))
         (name   (file-name-nondirectory src))
         (dst    (expand-file-name name dest)))
    (cond
     ((eq action 'copy)
      (copy-file src dst t)
      (message "Copied %s → %s" name dest))
     ((eq action 'cut)
      (rename-file src dst t)
      (message "Moved %s → %s" name dest)))
    (setq my/neotree-clipboard nil)
    (neotree-refresh)))

(defun my/neotree-toggle ()
  "Toggle NeoTree, rooting it at the current buffer’s parent directory."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Not visiting a file"))
    (let ((dir (file-name-directory file)))
      (neotree-toggle)
      (when (neo-global--window-exists-p)
        (neotree-dir dir)
        (neotree-find file)))))

;;; Toggle between lsp-mode and eglot per-buffer.
(defvar my/lsp-eglot--first-toggle t
  "If non-nil, the next toggle is the 'first' run: stop both then start lsp-mode.
Set to nil after the first initialization.")

(defvar-local my/lsp-eglot--current nil
  "Buffer-local marker of which client is considered 'current' in this buffer.
Either the symbol 'lsp or 'eglot, or nil if unknown.")

(defun my/lsp-eglot--lsp-active-p ()
  "Return non-nil if lsp-mode appears active in the current buffer."
  (bound-and-true-p lsp-mode))

(defun my/lsp-eglot--eglot-active-p ()
  "Return non-nil if eglot is managing the current buffer."
  (cond
   ;; prefer official predicate if available
   ((fboundp 'eglot-managed-p)
    (ignore-errors (eglot-managed-p)))
   ;; fallback to the internal flag used by eglot
   (t (bound-and-true-p eglot--managed-mode))))

(defun my/lsp-eglot--start-lsp ()
  "Start lsp in the current buffer (prefer lsp-deferred if available)."
  (cond
   ((fboundp 'lsp-deferred) (lsp-deferred))
   ((fboundp 'lsp) (lsp))
   (t (message "No lsp-mode entrypoint found (`lsp` or `lsp-deferred`)."))))

(defun my/lsp-eglot--stop-lsp ()
  "Stop lsp-mode in current buffer, using best-available API."
  (when (my/lsp-eglot--lsp-active-p)
    (condition-case _err
        (if (fboundp 'lsp-disconnect)
            (lsp-disconnect)
          ;; fallbacks; try workspace shutdown if present
          (if (fboundp 'lsp-workspace-shutdown)
              (lsp-workspace-shutdown)
            (when (fboundp 'lsp-shutdown-workspace)
              (lsp-shutdown-workspace))))
      (error (message "Error stopping lsp-mode (ignored)")))))

(defun my/lsp-eglot--stop-eglot ()
  "Stop eglot if it's managing this buffer."
  (when (my/lsp-eglot--eglot-active-p)
    (condition-case _err
        (when (fboundp 'eglot-shutdown)
          ;; eglot-shutdown will ask which server if ambiguous; that is okay.
          (eglot-shutdown))
      (error (message "Error shutting down eglot (ignored)")))))

;;;###autoload
(defun my/toggle-lsp-eglot ()
  "Toggle between `lsp-mode` and `eglot` in the current buffer.

Behavior:
- First time you call this (global `my/lsp-eglot--first-toggle` is non-nil):
  * Stop both clients if running, then start `lsp-mode` and set the buffer to 'lsp.
  * Clears `my/lsp-eglot--first-toggle` so future calls toggle instead.
- After initialization:
  * If current buffer is using 'lsp, stop lsp and start eglot (set buffer state to 'eglot).
  * If current buffer is using 'eglot, stop eglot and start lsp (set buffer state to 'lsp).
  * If the buffer has neither and no remembered state, start lsp by default."
  (interactive)
  ;; initial run: stop both and start lsp
  (if my/lsp-eglot--first-toggle
      (progn
        (message "Initializing LSP toggle: stopping both clients (if any) and starting lsp-mode...")
        (my/lsp-eglot--stop-lsp)
        (my/lsp-eglot--stop-eglot)
        (my/lsp-eglot--start-lsp)
        (setq-local my/lsp-eglot--current 'lsp)
        (setq my/lsp-eglot--first-toggle nil)
        (message "Initialized: lsp-mode started. Future presses will toggle between lsp-mode and eglot."))
    ;; normal toggle behavior
    (let ((current my/lsp-eglot--current)
          (actual (or (and (my/lsp-eglot--lsp-active-p) 'lsp)
                      (and (my/lsp-eglot--eglot-active-p) 'eglot)
                      nil)))
      ;; If remembered state disagrees with actual active client, prefer actual client.
      (when (and actual (not (eq actual current)))
        (setq-local my/lsp-eglot--current actual)
        (setq current actual))
      ;; If still nil, assume lsp as default to start
      (unless current (setq current 'lsp
														my/lsp-eglot--current 'lsp))
      (cond
       ((eq current 'lsp)
        ;; switch to eglot
        (message "Switching: stopping lsp-mode and starting eglot...")
        (my/lsp-eglot--stop-lsp)
        (when (fboundp 'eglot-ensure)
          (eglot-ensure))
        (setq-local my/lsp-eglot--current 'eglot)
        (message "Now using eglot."))
       ((eq current 'eglot)
        ;; switch to lsp
        (message "Switching: stopping eglot and starting lsp-mode...")
        (my/lsp-eglot--stop-eglot)
        (my/lsp-eglot--start-lsp)
        (setq-local my/lsp-eglot--current 'lsp)
        (message "Now using lsp-mode."))
       (t
        (message "Unknown toggle state; starting lsp-mode by default.")
        (my/lsp-eglot--start-lsp)
        (setq-local my/lsp-eglot--current 'lsp))))))

(defun my/treesit-install-missing (&optional languages)
  "Install missing tree-sitter grammars from `treesit-language-source-alist`.
If LANGUAGES is non-nil, restrict to that list of language symbols."
  (interactive)
  (unless treesit-language-source-alist
    (error "treesit-language-source-alist is not set!"))
  (let* ((pairs (if languages
                    (mapcar (lambda (sym)
                              (cons sym (cdr (assoc sym treesit-language-source-alist))))
                            languages)
                  treesit-language-source-alist))
         (missing
          (seq-filter
           (lambda (entry)
             (when entry
               (not (treesit-language-available-p (car entry)))))
           pairs)))
    (if (null missing)
        (message "All tree-sitter grammars already installed.")
      (dolist (entry missing)
        (let ((lang (car entry)))
          (message "[treesit] Installing grammar: %s" lang)
          (condition-case err
              (progn
                (treesit-install-language-grammar lang)
                (message "[treesit] Installed %s" lang))
            (error
             (message "[treesit] Failed %s: %s" lang (error-message-string err)))))))))


(defun org-insert-tangle-header (language file)
  "Insert an org-mode tangle header for LANGUAGE and FILE at point."
  (interactive "sLanguage: \nsFile: ")
  (insert (format "#+PROPERTY: header-args:%s :tangle ./%s :mkdirp yes" language file)))

;; (defun org-insert-transclusion-lines (file anchor lines lang)
;;   "Insert #+transclude for FILE with ::ANCHOR, :lines LINES, :src LANG."
;;   (interactive
;;    (list
;;     (read-file-name "File: ")
;;     (read-string "Anchor after '::' (e.g. function name or heading): ")
;;     (read-string "Lines (e.g. 1-5, 2-5 , 1- ): ")
;;     (read-string "Src language (e.g. c, python, org, md): ")))
;;   (insert (format "#+transclude: [[file:%s::%s]] :lines %s :src %s"

;;                   (file-relative-name file) anchor lines lang)))
(defun org-insert-transclusion-conditional (file anchor lines lang only-contents end-term)
  "Insert a conditional #+transclude: link.
Only include each property if its argument is non-empty."
  (interactive
   (list
    (read-file-name "File: ")
    (let ((a (read-string "Anchor after '::' (leave blank to skip): ")))
      (if (string-empty-p a) nil a))
    (read-string "Lines (e.g. 1-5, 2-5 , 1- ,leave blank to skip): ")
    (let ((l (read-string "Src language, leave blank to skip: ")))
      (if (string-empty-p l) nil l))
    (y-or-n-p "Include :only-contents? ")
    (let ((e (read-string "End search term (leave blank to skip): ")))
      (if (string-empty-p e) nil e))))
  (let* ((link (if anchor
                   (format "[[file:%s::%s]]" (file-relative-name file) anchor)
                 (format "[[file:%s]]" (file-relative-name file))))
         (props
          (concat
           (when lang (format " :src %s" lang))
           (when (and only-contents lang) ;; only-contents makes sense after src
             " :only-contents")
           (when (not (string-empty-p lines))
             (format " :lines %s" lines))
           (when end-term
             (format " :end \"%s\"" end-term)))))
    (insert (format "#+transclude: %s%s" link props))))


(defun refresh-my-agenda-files ()
  "Refresh `org-agenda-files` to include all relevant org files."
  (interactive)
  (setq org-agenda-files
        (append
         ;; All org files inside ~/git/Emacs-Todos/
         (directory-files-recursively
          "~/git/Emacs-Todos/" "\\.org$")
         ;; All org files inside ~/git/emacs-notes/ (recursive)
         (directory-files-recursively
          "~/git/emacs-notes/" "\\.org$")))
  (message "Refreshed org-agenda-files, now contains %d files."
           (length org-agenda-files)))

(defun my/org-wrap-region-with (marker)
  "Wrap the current visual region with MARKER characters."
  (interactive "cWrap with marker: ")
  (let ((beg (region-beginning)) (end (region-end)))
    (goto-char end)
    (insert marker)
    (goto-char beg)
    (insert marker)))
;; Wrap for specific markers via small wrappers:
(defun my/org-wrap-bold () (interactive) (my/org-wrap-region-with ?*))
(defun my/org-wrap-italic () (interactive) (my/org-wrap-region-with ?/))
(defun my/org-wrap-underline () (interactive) (my/org-wrap-region-with ?_))
(defun my/org-wrap-strike () (interactive) (my/org-wrap-region-with ?+))
(defun my/org-wrap-highlight-eq () (interactive) (my/org-wrap-region-with ?=))
(defun my/org-wrap-highlight-tilde () (interactive) (my/org-wrap-region-with ?~))

(defun my/org-roam-subdirs ()
  "Return list of subdirectories inside `org-roam-directory`."
  (let ((dirs (directory-files-recursively org-roam-directory "")))
    (delete-dups
     (mapcar (lambda (f) (file-relative-name (file-name-directory f) org-roam-directory))
             dirs))))

(defun my/org-roam-create-node-in-directory ()
  "Select or create subdir, enter title, and create an org-roam node."
  (interactive)
  (let* ((subdir (completing-read "Choose folder: " (my/org-roam-subdirs) nil nil))
         (full-dir (expand-file-name subdir org-roam-directory))
         (title (read-string "Title: "))
         (filename (expand-file-name (concat (org-roam-node-slug (org-roam-node-create :title title))
                                             ".org")
                                     full-dir)))
    (unless (file-directory-p full-dir)
      (make-directory full-dir t))
    (find-file filename)
    (insert (format "#+title: %s\n" title))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file filename)))

(defun my/org-roam-insert-or-create-node ()
  "Prompt for subdir and title, create note if not exist, then prompt via `org-roam-node-insert`."
  (interactive)
  (let* ((subdir (completing-read "Choose folder: " (my/org-roam-subdirs) nil nil))
         (dir (expand-file-name subdir org-roam-directory))
         (title (read-string "Title: "))
         (slug (org-roam-node-slug (org-roam-node-create :title title)))
         (file (expand-file-name (concat slug ".org") dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-exists-p file)
      (with-temp-buffer
        (insert (format "#+title: %s\n" title))
        (write-file file))
      (with-current-buffer (find-file-noselect file)
        (org-id-get-create)
        (save-buffer))
      (org-roam-db-update-file file))
    ;; Let Org-roam insert the node link using its usual insertion UI
    (org-roam-node-insert)))

(defun my/org-capture-fullframe (&optional key)
  "Open `org-capture` in a dedicated full-frame, no splits."
  (interactive)
  (let ((frame (make-frame '((name . "org-capture")
                             (fullscreen . fullboth)))))
    (select-frame-set-input-focus frame)
    (delete-other-windows)
    ;; Ensure the selection UI doesn't cause splits:
    (advice-add 'org-capture-place-template :after #'delete-other-windows)
    (add-hook 'org-capture-after-finalize-hook
              (lambda ()
                (advice-remove 'org-capture-place-template #'delete-other-windows)
                (delete-frame)) :append)
    (org-capture nil key)))

(defcustom my/org-agenda-frame-name "org-agenda"
  "Name of dedicated frame for `org-agenda`."
  :type 'string)

(defun my/org-agenda-fullframe (&optional cmd)
  "Open `org-agenda` in a dedicated full-frame."
  (interactive)
  (let ((frame (make-frame `((name . ,my/org-agenda-frame-name)
                             (fullscreen . fullboth)))))
    (select-frame-set-input-focus frame)
    (delete-other-windows)
    (advice-add 'org-agenda :after #'delete-other-windows)
    (add-hook 'org-agenda-quit-hook
              (lambda ()
                (advice-remove 'org-agenda #'delete-other-windows)
                (delete-frame)) :append)
    (org-agenda nil cmd)))

(defun my/mu4e-fullframe ()
  "Launch mu4e in fullscreen and close frame when the main mu4e buffer is killed."
  (interactive)
  ;; Make frame fullscreen
  (toggle-frame-fullscreen)
  ;; Launch mu4e
  (mu4e)
  (delete-other-windows)
  ;; Set up frame deletion when the main mu4e buffer is killed
  (with-current-buffer "*mu4e-main*"
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (frame-parameter nil 'my/mu4e-fullframe)
                  (delete-frame)))
              nil t))
  ;; Mark this frame
  (set-frame-parameter nil 'my/mu4e-fullframe t))


;;; ---------------- ;;;
;;; Replace helpers  ;;;
;;; ---------------- ;;;

(defun my/replace-word-at-point-single (replacement)
  "Replace the word or symbol at point with REPLACEMENT (this occurrence only)."
  (interactive
   (let* ((word (or (thing-at-point 'symbol t) (thing-at-point 'word t)))
          (prompt (format "Replace '%s' with: " (or word ""))))
     (list (read-string prompt word))))
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                    (bounds-of-thing-at-point 'word))))
    (unless bounds (user-error "No word at point"))
    (let ((beg (car bounds)) (end (cdr bounds)))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert replacement)))))

(defun my/evil-comment-at-eol ()
  "Append a comment at end of line, place point after the delimiter, and enter Evil insert."
  (interactive)
  (comment-normalize-vars)          ;; make sure comment vars are set for this mode
  (end-of-line)
  ;; `comment-dwim` at EOL inserts the proper comment starter for the mode and
  ;; places point after it (per Emacs/CC Mode docs).
  (let ((current-prefix-arg nil))   ;; ensure we’re not killing comments (C-u M-;)
    (call-interactively #'comment-dwim))
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun my/open-impatient-preview ()
  "Open the impatient-mode preview in the default browser."
  (interactive)
  (browse-url "http://localhost:8080/imp/"))

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Org")
    (cfw:ical-create-source "GCal"
														"https://calendar.google.com/calendar/ical/user.idc004%40gmail.com/private-a9c27b6aeffca1f9755f2a3a12d2bb57/basic.ics"
														"cyan"))))

;; Custom functions to update individual email accounts
(defun update-pappanos-mail ()
  "Update only pappanos email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync pappanos")
  (mu4e-update-index))

(defun update-work-mail ()
  "Update only work email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync work")
  (mu4e-update-index))

(defun update-gamedev-mail ()
  "Update only gamedev email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync gamedev")
  (mu4e-update-index))

(defun update-user-mail ()
  "Update only user email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync user")
  (mu4e-update-index))

(defun update-mine-mail ()
  "Update only mine email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync mine")
  (mu4e-update-index))

(defun update-money-mail ()
  "Update only money email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync money")
  (mu4e-update-index))

(defun update-overwhatch-mail ()
  "Update only overwhatch email"
  (interactive)
  (mu4e-update-mail-and-index t)
  (shell-command "mbsync overwhatch")
	(mu4e-update-index))

(defun my/switch-theme-clean ()
  "Clear current themes without breaking non-theme face customizations or font size."
  (interactive)
  ;; Store current font size before theme change
  (let ((current-themes custom-enabled-themes)
        (current-font-height (face-attribute 'default :height)))
    ;; Disable all currently enabled themes
    (dolist (theme current-themes)
      (disable-theme theme))
    ;; Force a redisplay to ensure the disable takes effect
    (redraw-display)
    ;; Small delay to let the cleanup finish
    (sit-for 0.1)
    ;; Now load the new theme
    (call-interactively #'load-theme)
    ;; Restore the original font size
    (set-face-attribute 'default nil :height current-font-height)
    (message "Theme switched cleanly with preserved font size!")))

(defun my/tvd-emms-beginning-of-song()
  "Jump to beginning of a song"
  (interactive)
  (emms-seek-to '00:00))

;; Fixed function to extract info for all tracks in playlist
(defun emms-playlist-tracks-refresh-info ()
  "Refresh info for all tracks in current playlist."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((tracks '()))
      ;; Collect all tracks
      (while (not (eobp))
        (when-let ((track (emms-playlist-track-at (point))))
          (push track tracks))
        (forward-line 1))
      ;; Extract info for all tracks
      (dolist (track tracks)
        (emms-info-initialize-track track)
        (dolist (info-fn emms-info-functions)
          (ignore-errors (funcall info-fn track))))
      (message "Refreshed info for %d tracks" (length tracks)))))

(defun my/audio-open-playlist (filename &optional wildcards)
  "Makes a new EMMS playlist buffer from the playlist of FILENAME.
When prompting, starts in ~/Music."
  (interactive
   (let ((default-directory (expand-file-name "~/Music/")))
     (find-file-read-args "Find playlist file: "
                          (confirm-nonexistent-file-or-buffer))))
  (emms-playlist-new (file-name-base filename))
  (switch-to-buffer (file-name-base filename))
  (emms-playlist-set-playlist-buffer)
  ;; Load the playlist
  (emms-play-playlist filename)
  (emms-playlist-tracks-refresh-info)
  ;; Update display
  (emms-playlist-mode-update-track-function))

(defun my/audio-create-playlist(name)
  "Create a new audio playlist for EMMS player.
It asks interactively for NAME.  The playlistname will be derived
as     this: 'source-file-default-directory + \"playlist-\" + NAME.
If the playlist already exists nothing will be done.
Otherwise the  user will be  asked wether  to add a  directory or
file[s] to  the playlist.  In the  first case  the user  can then
interactively select a directory. In the latter case the user can
add interactively one or more files.
The playlist will be saved when  a directory has been selected or
the user declines to add another file."
  (interactive "sPlaylist name: ")
  (let ((filename (expand-file-name (format "playlist-%s" name) emms-source-file-default-directory)))
    (message filename)
    (if (file-exists-p filename)
        (message (format "Playlist %s already exists!" filename))
      (progn
        (emms-playlist-new name)
        (switch-to-buffer name)
        (emms-playlist-set-playlist-buffer name)
        (let ((read-answer-short t)
              (answer (read-answer "add [D]irectory or [F]ile(s)? "
                                   '(("dir" ?d "add dir")
                                     ("file" ?f "add file")
                                     ("url" ?u "add a streaming url")
                                     ("quit" ?q "quit"))))
              (done nil)
              (abort nil))
          (setq default-directory emms-source-file-default-directory)
          (cond
           ((equal answer "url")
            (let ((url (read-string "Enter a url: ")))
              (when url
                (emms-add-url url))))
           ((equal answer "dir")
            (while (not done)
              (emms-playlist-set-playlist-buffer name)
              (let ((dir (read-directory-name "Select directory:")))
                (when dir
                  (emms-add-directory-tree dir))
                (setq answer (read-char-from-minibuffer "Add another dir [Yn]? "))
                (when (equal answer ?n)
                  (setq done t)))))
           ((equal answer "file")
            (while (not done)
              (emms-playlist-set-playlist-buffer name)
              (let ((file (read-file-name "Select audio file:")))
                (when file
                  (emms-add-file file)
                  (setq default-directory (file-name-directory file)))
                (setq answer (read-char-from-minibuffer "Add another file [Yn]? "))
                (when (equal answer ?n)
                  (setq done t)))))
           (t (setq abort t)))
          (if abort
              (kill-buffer)
            (progn
              (emms-playlist-tracks-refresh-info)
              (emms-playlist-save 'native filename)
              (emms-playlist-mode))))))))

;; Set the shift width to 2 instead of the default 4
(setq evil-shift-width 2)
(setq-default tab-width 2)

;; Global keybindings using the leadeir key:
(start/leader-keys
  "." '(find-file :wk "Find file")
  "<tab>" '(:ignore t :wk "Comment")
  "<tab> <tab>" '(evil-commentary :wk "Comment lines")
  "<tab> e" '(my/evil-comment-at-eol :wk "Comment end of line")
  "p" '(:ignore t :wk "Projectile / Projectile tools")
  "p C-p" '(projectile-command-map :wk "Projectile command map")
	"p c e" '(flycheck-projectile-list-errors :wk "List project errors")
  "p b" '(counsel-projectile-switch-to-buffer :wk "Switch buffer")
  "p B" '(counsel-projectile-switch-to-buffer :wk "Open buffer in split")
  "p i" '(projectile-ibuffer :wk "Switch Ibuffer")
	"p p" '(projectile-switch-project :wk "Switch project")
  "p r" '(projectile-recentf :wk "Recently visited project files")
	"p q" '(projectile-kill-buffers :wk "Kill all project buffers")
  "p d" '(projectile-dired :wk "Open project root in dired")
  "p D" '(projectile-dired-other-window :wk "Open project root in dired in other window")
	"p g" '(projectile-ripgrep :wk "RipGrep in project")
  "p c" '(:ignore t :wk "Code actions")
	"p c c" '(projectile-compile-project :wk "Compile project")
	"p c t" '(projectile-test-project :wk "Test project")
	"p c i" '(projectile-install-project :wk "Install project ")
	"p c r" '(projectile-run-project :wk "Run project")
	"p c R" '(projectile-find-references :wk "(NOT WORKING)Find references in project")
  "p s" '(:ignore t :wk "Search file / project")
	"p s r" '(projectile-find-related-file :wk "Switch between related files foo.c foo.h")
	"p s w" '(projectile-find-file-other-window :wk "Find file and open in split window")
	"p s W" '(projectile-find-file-dwim-other-window :wk "Find file at point and open in split window")
	"p s F" '(projectile-find-file-dwim :wk "Find file in project and open it")
	"p s f" '(projectile-find-file :wk "Find file in project and open it")
  "p C-r" '(projectile-replace :wk "(NOT WORKING)Replace in project")
	)

(start/leader-keys
	"P" '(:ignore t :wk "Perspective/Workspaces")
	"P c" '(my/persp-create-defaults :wk "create default workspaces")
	"P r" '(persp-rename :wk "Rename workspace")
	"P S" '(persp-state-save :wk "Save all persp in a file")
	"P L" '(persp-state-load :wk "Load all persp from a file")
	"P q" '(persp-kill :wk "Kill current workspace")
	"P w" '(persp-switch :wk "Switch or create to workspace")
	"P s" '(persp-switch-by-number :wk "Switch to workspace by number")
	"P m" '(persp-merge :wk "Merge 2 perspectives")
	"P u" '(persp-umerge :wk "Unmerge perspective")
	)

(start/leader-keys
  "f" '(:ignore t :wk "Find / files")
  "f k" '((lambda () (interactive)
            (find-file "~/.config/MainEmacs/keymaps.org"))
          :wk "Edit Emacs keymaps")
  "f t" '((lambda () (interactive)
            (find-file "~/git/Emacs-Todos/TODO.org"))
          :wk "Open TODO file")
  "f c" '((lambda () (interactive)
            (find-file "~/.config/MainEmacs/package_configs.org"))
          :wk "Edit Emacs config")
  "f C-c" '((lambda () (interactive)
							(dired "~/git/Emacs-Todos/emails/")) 
						:wk "Open email notes in dired")
  "f e" '((lambda () (interactive)
            (dired "~/.config/MainEmacs/")) 
          :wk "Open user-emacs-directory in dired")
  "f E" '((lambda () (interactive)
            (find-file "~/.config/MainEmacs/org-mode-example.org")) 
          :wk "Emacs org mode examples")
  "f n" '((lambda () (interactive)
            (dired "~/git/emacs-notes/")) 
          :wk "Open user-emacs-directory in dired")
  "f r" '(consult-recent-file :wk "Recent files")
  ;; "f f" '(consult-fd :wk "Fd search for files")
  "f f" '(counsel-fd-file-jump :wk "Fd search for files")
  "f F" '(counsel-fd-dired-jump :wk "Fd search for directorys")
  "f C" '(counsel-css :wk "Jump to a css selector")
  ;; "f s" '(counsel-grep-or-swiper :wk "Search for string current file")
  "f s" '(swiper :wk "Search {C-c C-o} for string current file")
  "f S" '(swiper-all :wk "Search for string all open files")
  "f g" '(counsel-rg :wk "Rg search for string in directory")
  "f G" '(consult-ripgrep :wk "Ripgrep search for file in directory")
	"f j" '(counsel-file-jump :wk "Jump to a file below current directory")
  "f l" '(counsel-locate :wk "Locate a file")
  "f L" '(consult-line :wk "Find line")
  "f d" '(find-grep-dired :wk "Search for string in files in DIR")
  "f C-s" '(sudo-edit-find-file :wk "Sudo find file")
  "f M-s" '(sudo-edit :wk "Sudo edit file")
  "f i" '(consult-imenu :wk "Imenu buffer locations")
	)

(start/leader-keys
  "l" '(:ignore t :wk "Lsp / Eglot Evaluate")
	"l l" '(:ignore t :wk "Lsp")
	"l l f" '(lsp-format-buffer :wk "Lsp Format buffer")
  "l l F" '(lsp-format-region :wk "Lsp Format region")
	"l l s" '(lsp-ui-sideline-mode :wk "Enable lsp-mode sydeline info")
	"l l i" '(lsp-ivy-workspace-symbol :wk "Find and jump to specific word")
	"l l I" '(lsp-inlay-hints-mode :wk "Lsp inlay hints")
	"l l c" '(Lsp-execute-code-action :wk "Lsp recomended code changes")
  "l e" '(:ignore t :wk "Eglot")
  "l e r" '(eglot-reconnect :wk "Eglot Reconnect")
  "l e f" '(eglot-format-buffer :wk "Eglot Format buffer")
  "l e F" '(eglot-format :wk "Eglot Format region")
	"l e I" '(eglot-inlay-hints-mode :wk "Eglot inlay hints")
	"l e c" '(eglot-code-actions :wk "Eglot recomended code changes")
	"l g" '(:ignore t :wk "Lsp Go to")
  "l g d" '(lsp-find-definition :wk "Lsp Find definition")
  "l g r" '(lsp-find-references :wk "Lsp Find references")
  "l g t" '(lsp-find-type-definition :wk "Lsp Find type definition")
  "l g i" '(lsp-find-implementation :wk "Lsp Find implementation")
  "l g D" '(lsp-find-declaration :wk "Lsp Find declaration")
  "l p" '(:ignore t :wk "Lsp Peek To")
	"l p d" '(lsp-ui-peek-find-definitions :wk "Lsp Peek definitions")
  "l p r" '(lsp-ui-peek-find-references :wk "Lsp Peek references")
  "l p i" '(lsp-ui-peek-find-implementation :wk "Lsp Peek implementations")
  "l d" '(:ignore t :wk "Lsp Open documentation")
	"l d e" '(eldoc :wk "Show documentation Eldoc")
	"l d p" '(eldoc-box-hover-at-point-mode :wk "Show/quit doc Eldoc at point")
	"l d E" '(eldoc-box-quit-frame :wk "Close Eldoc")
	"l d <down>" '(eldoc-box-scroll-down :wk "Eldoc scroll down")
	"l d <up>" '(eldoc-box-scroll-up :wk "Eldoc scroll up")
	"l d d" '(lsp-ui-doc-show :wk "Lsp Show documentation")
  "l d D" '(lsp-ui-doc-hide :wk "Lsp Hide documentation")
	"l d f" '(lsp-ui-doc-focus-frame :wk "Place cursor in doc frame")
  "l d F" '(lsp-ui-doc-unfocus-frame :wk "Exit cursor from doc frame")
	"l d q" '(lsp-ui-doc-mode :wk "Enable or disable doc mode")
	"l d g" '(lsp-ui-doc-glance :wk "Quick documentationon point")
	"l m" '(disaster :wk "Make c,cpp line into assembly")
  "l s" '(my/toggle-lsp-eglot :wk "Toggle between eglot and lsp-mode")
  "l S" '(ivy-yasnippet :wk "Snipet insert")
  "l q" '(:ignore t :wk "Close or enable eglot or lsp-mode")
  "l q e" '(eglot-shutdown :wk "Eglot Shutdown")
  "l q E" '(eglot :wk "Eglot Enable")
  "l q l" '(lsp-disconnect :wk "Lsp disconnect")
  "l q L" '(lsp :wk "Lsp Enable")
	"l t" '(:ignore t :wk "Lsp Treemacs")
	"l t T" '(my/treesit-install-missing :wk "auto install tree-sitter missing languages")
  "l t e" '(lsp-treemacs-errors-list :wk "Lsp Treemacs Errors")
  "l t s" '(lsp-treemacs-symbols :wk "Lsp Treemacs Symbols")
  "l t r" '(lsp-treemacs-references :wk "Lsp Treemacs References")
  "l t i" '(lsp-treemacs-implementations :wk "Lsp Treemacs Implementations")
  "l t c" '(lsp-treemacs-call-hierarchy :wk "Lsp Treemacs Call Hierarchy")
  "l t t" '(lsp-treemacs-type-hierarchy :wk "Lsp Treemacs Type Hierarchy")
  "l t d" '(lsp-treemacs-deps-list :wk "Lsp Treemacs Dependencies")
  "l r" '(:ignore t :wk "Lsp rename")
  "l r r" '(lsp-rename :wk "Rename word under cursor and all its refrences")
  "l r f" '(lsp-javascript-rename-file :wk "Rename current file and all its refrences")
  "l f" '(consult-flymake :wk "Consult Flymake")
  "l b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "l R" '(eval-region :wk "Evaluate elisp in region")
	)

(start/leader-keys
  "C-l" '(:ignore t :wk "Lorem-ipsum {filler text}")
	"C-l p" '(lorem-ipsum-insert-paragraphs :wk "Lorem ipsum paragraphs")
	"C-l l" '(lorem-ipsum-insert-sentences :wk "Lorem ipsum lines")
	"C-l h" '(lorem-ipsum-insert-list :wk "Lorem ipsum header")
  )

(start/leader-keys
  "i" '(:ignore t :wk "Imenu list")
	"i t" '(imenu-list :wk "Launch list")
	"i r" '(imenu-list-refresh :wk "Refresh imenu list")
	"i q" '((lambda ()
						(interactive)
						(imenu-list-show)
						(imenu-list-quit-window)) 
					:wk "Hide imenu list")
	"i s" '(imenu-list-show-noselect :wk "Show imenu list")
	)

(start/leader-keys
  "v" '(:ignore t :wk "Change page View")
	"v z" '(focus-mode :wk "Focus at one block at a time")
	"v c" '(olivetti-mode :wk "Center Text in window")
	)

(start/leader-keys
  "o" '(:ignore t :wk "Org")
	"o h" '( (lambda ()
						 (interactive)
						 (call-interactively #'org-insert-tangle-header)
						 (org-ctrl-c-ctrl-c))
					 :wk "Org insert tangle header")
  "o a" '(org-agenda :wk "Org agenda")
	"o n" '(org-capture :wk "Notes/TODOS/Calendar captures")
  "o t" '(org-todo :wk "Org todo")
	"o T" '(counsel-org-tag :wk "Add tags")
	"o S" '(org-set-tags-command :wk "Org set tags")
  "o C-p" '(org-priority :wk "Set priority")
	"C-s" (lambda () (interactive) (save-buffer) (org-babel-tangle))
  "o r C-s" '((lambda () (interactive) (save-buffer) (org-babel-tangle) (refresh-my-agenda-files))
					:wk "Org save")
  "o s" '(:ignore t :wk "Saround / Highlight text")
	"o s b" '(my/org-wrap-bold :wk "Bold")
  "o s i" '(my/org-wrap-italic :wk "Italic")
  "o s u" '(my/org-wrap-underline :wk "Underline")
  "o s c" '(my/org-wrap-strike :wk "Cut")
  "o s h" '(my/org-wrap-highlight-eq :wk "Highlight blue")
  "o s H" '(my/org-wrap-highlight-tilde :wk "Highlight green")
  "o L" '(org-todo-list :wk "Org todo list")
	"o p" '(org-project-capture-project-todo-completing-read :wk "per project TODOs")
	"o c" '(org-toggle-checkbox :wk "Org toggle checkbox")
	"o l" '(org-insert-link :wk "Org insert link")
	"o C-l" '(org-latex-preview :wk "Org latex preview")
  "o i" '(org-toggle-inline-images :wk "Org toggle images on and off")
  "o C-i" '(org-insert-structure-template :wk "Org insert templetes")
  "o I" '(org-toggle-item :wk "Org toggle item")
	"o C-d" '((lambda ()
							(interactive)
							(insert (format "1 field name.kra 2 field tag name\n"))
							(call-interactively #'org-krita-insert-new-image))
						:wk "Create drawings with krita")
  "o E" '(org-export-dispatch :wk "Org export dispatch")
  "o e" '(:ignore t :wk "Download images")
	"o e d" '(org-download-clipboard :wk "Org download from clipboard")
	"o e s" '(org-download-screenshot :wk "Org download screenshot")
	"o e u" '(org-download-image :wk "Org download image from url")
  "o M-t" '(:ignore t :wk "Tables")
	"o M-t c" '(org-table-create :wk "Create a table")
	"o M-t d" '(org-table-move-row :wk "Move current row down")
	"o M-t u" '(org-table-move-row-up :wk "Move current row up")
	"o M-t D" '(org-table-move-culumn :wk "Move current column down")
	"o M-t U" '(org-table-move-column-up :wk "Move current column up")
	"o M-t l" '(org-table-move-column-left :wk "Move current column left")
	"o M-t r" '(org-table-move-column-right :wk "Move current column down")
	"o M-t x" '(org-table-kill-row :wk "Delete current row")
	"o M-t X" '(org-table-delete-column :wk "Delete current column")
  "o M-t a" '(org-table-insert-row :wk "Add a row")
  "o M-t A" '(org-table-insert-column :wk "Add a column")
  "o M-t -" '(org-table-insert-hline :wk "Insert hline in table")
  "o d" '(:ignore t :wk "Date/deadline")
  "o d t" '(org-time-stamp :wk "Org time stamp")
  "o d d" '(org-deadline :wk "Org time deadline stamp")
	"o d s" '(org-schedule :wk "Org time scedual stamp")
  "o C-t" '(:ignore t :wk "Tranclusion (imbend text/files)")
	"o C-t t" '(org-transclusion-mode :wk "Toggle tranclusion mode")
	"o C-t s" '((lambda ()
								(interactive)
								(org-transclusion-live-sync-start)
								(org-transclusion-move-to-source)) 
							:wk "Start live edit files")
	"o C-t e" '(org-transclusion-live-sync-exit :wk "Exit live edit files")
	"o C-t a" '(org-transclusion-add :wk "Open the transclusion at point")
	"o C-t c" '(org-transclusion-remove :wk "Close the transclusion at point")
	"o C-t m" '(org-insert-transclusion-conditional :wk "Create a custom imbend")
	"o C-t o" '((lambda ()
								(interactive)
								(org-transclusion-open-source)
								(org-transclusion-move-to-source))
							:wk "open imbend file in buffer")
  "o r" '(:ignore t :wk "Org-roam/roam-ui")
	"o r b" '(org-roam-buffer-toggle :wk "Buffer whith refrences to nodes")
	"o r f" '(org-roam-node-find :wk "Open node")
	"o r c" '(my/org-roam-create-node-in-directory :wk "Create node in dir")
	"o r L" '(org-roam-node-insert :wk "Link node")
	"o r l" '(my/org-roam-insert-or-create-node :wk "Make and link node")
	"o r g" '(org-roam-graph :wk "Open Graph")
	"o r h" '(org-id-get-create :wk "Make a header/file into a node")
	"o r a" '(org-roam-alias-add :wk "Make node alias")
	"o r r" '(org-roam-alias-remove :wk "Remove node alias")
	"o r u" '((lambda ()
							(interactive)
							(org-roam-ui-mode)
							(org-roam-ui-open))
						:wk "Enable and open org-R-ui")
	)

(start/leader-keys
  "h" '(:ignore t :wk "Help")
  "h f" '(counsel-describe-function :wk "Describe function") 
  "h v" '(counsel-describe-variable :wk "Describe variable")
  "h k" '(helpful-key :wk "Describe key")
  "h c" '(helpful-command :wk "Describe command")
  "h p" '(helpful-at-point :wk "Describe at point")
  "h C-f" '(helpful-function :wk "Describe function non councel")
  "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
  "h t" '(my/switch-theme-clean :wk "Load theme properly")
	"h F" '(describe-face :wk "Describe face")
	;; "h C" '(read-color :wk "List of emacs colors")
	"h C" '(counsel-colors-emacs :wk "List of emacs colors")
	"h C-c" '(counsel-colors-web :wk "List of css colors")
	"h r" '((lambda () (interactive)
						(load-file "~/.config/MainEmacs/init.el"))
					:wk "Reload Emacs config")
	)

(start/leader-keys
  "H" '(:ignore t :wk "Harpoon")
  "H a" '(harpoon-add-file :wk "Harpoon add file")
  "H t" '(harpoon-quick-menu-hydra :wk "Harpoon toggle")
  "H T" '(harpoon-toggle-quick-menu :wk "Harpoon simple toggle")
  "H c" '(harpoon-clear :wk "Harpoon clear all files")
  "H r" '(harpoon-delete-item :wk "Harpoon remove a file")
  "H n" '(harpoon-go-to-next :wk "Harpoon clear all files")
  "H p" '(harpoon-go-to-prev :wk "Harpoon clear all files")
  "H 1" '(harpoon-go-to-1 :wk "Harpoon go to 1")
  "H 2" '(harpoon-go-to-2 :wk "Harpoon go to 2")
  "H 3" '(harpoon-go-to-3 :wk "Harpoon go to 3")
  "H 4" '(harpoon-go-to-4 :wk "Harpoon go to 4")
  "H 5" '(harpoon-go-to-5 :wk "Harpoon go to 5")
  "H 6" '(harpoon-go-to-6 :wk "Harpoon go to 6")
  "H 7" '(harpoon-go-to-7 :wk "Harpoon go to 7")
  "H 8" '(harpoon-go-to-8 :wk "Harpoon go to 8")
  "H 9" '(harpoon-go-to-9 :wk "Harpoon go to 9")
  "H C-1" '(harpoon-delete-1 :wk "Harpoon remove 1")
  "H C-2" '(harpoon-delete-2 :wk "Harpoon remove 2")
  "H C-3" '(harpoon-delete-3 :wk "Harpoon remove 3")
  "H C-4" '(harpoon-delete-4 :wk "Harpoon remove 4")
  "H C-5" '(harpoon-delete-5 :wk "Harpoon remove 5")
  "H C-6" '(harpoon-delete-6 :wk "Harpoon remove 6")
  "H C-7" '(harpoon-delete-7 :wk "Harpoon remove 7")
  "H C-8" '(harpoon-delete-8 :wk "Harpoon remove 8")
  "H C-9" '(harpoon-delete-9 :wk "Harpoon remove 9")
	)

(start/leader-keys
  "t" '(:ignore t :wk "Toggle/Todo/pdf")
  "t w" '(visual-line-mode :wk "Toggle wrap lines (truncated)")
	"t f" '(flycheck-mode :wk "Toggle flycheck")
  "t L" '(display-line-numbers-mode :wk "Toggle line numbers")
	"t n" '(hl-todo-next :wk "Go to next todo comment")
  "t p" '(hl-todo-previous :wk "Go to previous todo comment")
	"t l" '(hl-todo-occur :wk "List all todo comments in current buffer")
  "t i" '(hl-todo-insert :wk "Add a todo comment")
	"t t" '(eat :wk "Toggle eat terminal")
	"t P" '(pdf-tools-enable-minor-modes :wk "Enable pdf-tools modes")
	)

(start/leader-keys
	"T" '(:ignore t :wk "Tools {rg, yazi, lazy, etc}")
	"T l" '(ee-lazygit :wk "Open lazygit")
	"T y" '(ee-yazi :wk "Yazi current folder")
  "T Y" '(ee-yazi-project :wk "Yazi project root")
  "T S" '(ee-rg :wk "Search current file")
  "T s" '(ee-rga :wk "Search all open files")
  "T f" '(ee-find :wk "Find file in current folder")
  )

(start/leader-keys
	"j" '(:ignore t :wk "Jump to")
	"j w" '(avy-goto-word-1 :wk "Jump to word")
	"j l" '(avy-goto-line :wk "Jump to line")
  "j c" '(avy-goto-char :wk "Jump to char")
  "j C" '(avy-goto-char-2 :wk "Jump to char 2")
  )

(start/leader-keys
	"C-p" '(:ignore t :wk "Packages actions")
	"C-p i" '(package-install :wk "Install package")
	"C-p d" '(package-delete :wk "Delete package")
  "C-p u" '((lambda ()
							(interactive)
							(package-refresh-contents)
							(let ((upgrades (package-upgrade-all)))
								(when upgrades
									(when (yes-or-no-p (format "Upgrade %d package%s? "
																						 (length upgrades)
																						 (if (= (length upgrades) 1) "" "s")))
										(package-refresh-contents)
										(package-upgrade-all))))
							(package-vc-upgrade-all))
						:wk "Upgrade packages with prompt")
	"C-p l" '(list-packages :wk "List packages")
	"C-p r" '(package-refresh-contents :wk "Refresh package list")
	)

(start/leader-keys
	"s" '(:ignore t :wk "Split / Show")
	"s d" '(dictionary-search :wk "Search dictionary")
	"s h" '(split-window-horizontally :wk "Split window horizontally")
	"s v" '(split-window-vertically :wk "Split window vertically")
	"s q" '(delete-window :wk "Close current split window")
	"s e" '(balance-windows :wk "Evenly size all split windows")
	)

(start/leader-keys
	"e" '(:ignore t :wk "tree")
	;; "e e" '(neotree-toggle :wk "neotree")
	"e e" '(my/neotree-toggle :wk "neotree")
	"e C-e" '(dirvish-side :wk "dirvish tree")
	"e E" '(treemacs :wk "treemacs")
	"e a" '(neotree-create-node :wk "neotree create file or dir with /")
	"e d" '(neotree-delete-node :wk "neotree delete file or dir i am on")
	"e r" '(neotree-rename-node :wk "neotree rename file or dir i am on")
	"e c" '(neotree-copy-node :wk "neotree copy file or dir i am on")
	"e ." '(neotree-hidden-file-toggle :wk "neotree toggle dotfiles")
	"e S-<up>" '(neotree-select-up-node :wk "neotree go to parent directory")
	"e S-<down>" '(neotree-select-down-node :wk "neotree go to child directory")
	"e x" '(my/neotree-cut-node :wk "neotree cut file or dir i am on")
	"e p" '(my/neotree-paste-node :wk "neotree paste file or dir i cut or coppied")
	)

(start/leader-keys
	"E" '(:ignore t :wk "Evaluate")    
	"E b" '(eval-buffer :wk "Evaluate elisp in buffer")
	"E d" '(eval-defun :wk "Evaluate defun containing or after point")
	"E e" '(eval-expression :wk "Evaluate and elisp expression")
	"E l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
	"E r" '(eval-region :wk "Evaluate elisp in region")
	)

(start/leader-keys
	"b" '(:ignore t :wk "Bookmarks/Buffers")
	;; "b b" '(consult-buffer :wk "Switch buffer")
	"b b" '(persp-counsel-switch-buffer :wk "Switch buffer counsel")
	"b B" '(counsel-switch-buffer-other-window :wk "Open buffer in other window")
	;; "b a" '(persp-switch-to-buffer :wk "Switch buffer")
	"b a" '(persp-switch-to-buffer* :wk "Switch buffer")
	;; "b k" '(kill-this-buffer :wk "Kill this buffer")
	"b k" '(persp-kill-buffer* :wk "Kill buffer")
	;; "b i" '(ibuffer :wk "Ibuffer")
	"b i" '(persp-ibuffer :wk "Ibuffer")
	"b n" '(next-buffer :wk "Next buffer")
	"b p" '(previous-buffer :wk "Previous buffer")
	"b r" '(revert-buffer :wk "Reload buffer")
	"b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
	"b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
	"b s" '(bookmark-set :wk "Set bookmark")
	"b R" '(bookmark-rename :wk "Rename bookmark")
	"b d" '(bookmark-delete :wk "Delete bookmark")
	"b D" '(bookmark-delete-all :wk "Delete all bookmarks")
	;; "b j" '(consult-bookmark :wk "Bookmark jump")
	"b j" '(counsel-bookmark :wk "Bookmark jump")
	"b J" '(bookmark-jump-other-window :wk "Bookmark jump split window")
	"b l" '(list-bookmarks :wk "List bookmarks")
	"b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
	)

(start/leader-keys
	"g" '(:ignore t :wk "Git-Games")
	"g /" '(magit-displatch :wk "Magit dispatch")
	"g ." '(magit-file-displatch :wk "Magit file dispatch")
	"g b" '(magit-branch-checkout :wk "Switch branch")
	"g c" '(:ignore t :wk "Create") 
	"g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
	"g c c" '(magit-commit-create :wk "Create commit")
	"g c f" '(magit-commit-fixup :wk "Create fixup commit")
	"g C" '(magit-clone :wk "Clone repo")
	"g f" '(:ignore t :wk "Find") 
	"g f c" '(magit-show-commit :wk "Show commit")
	"g f f" '(magit-find-file :wk "Magit find file")
	"g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
	"g F" '(magit-fetch :wk "Git fetch")
	"g m" '(magit-status :wk "Magit status")
	"g i" '(magit-init :wk "Initialize git repo")
	"g l" '(magit-log-buffer-file :wk "Magit buffer log")
	"g r" '(vc-revert :wk "Git revert file")
	"g s" '(magit-stage-file :wk "Git stage file")
	"g t" '(git-timemachine :wk "Git time machine")
	"g u" '(magit-stage-file :wk "Git unstage file")
	"g g" '(:prefix t :wk "games")
	"g g t" '(tetris :wk "Tetris") 
	"g g s" '(snake  :wk "Snake")  
	"g g 5" '(5x5 :wk "5x5") 
	"g g B" '(blackbox :wk "Blackbox") 
	"g g b" '(bubbles :wk "Bubbles") 
	"g g d" '(dunnet :wk "Dunnet") 
	"g g g" '(gomoku :wk "Gomoku") 
	"g g h" '(hanoi :wk "Hanoi") 
	"g g l" '(life :wk "Life") 
	"g g m" '(mpuz :wk "M-puzzle") 
	"g g p" '(pong :wk "Pong") 
	"g g S" '(solitaire :wk "Solitaire") 
	"g g z" '(zone :wk "Zone") 
	"g g d" '(doctor :wk "Doctor") 
	)

(start/leader-keys
	"d" '(:ignore t :wk "Dired")
	"d p" '(dired-preview-mode :wk "Preview dired files")
	"d t" '((lambda ()
						(interactive)
						(dirvish)
						(setq dirvish-hide-details t))
					:wk "Open dirvish")
	"d M-t" '((lambda ()                      
							(interactive)                 
							;; (dired default-directory)                     
							(counsel-dired)                     
							(setq dirvish-hide-details nil)) 
						:wk "Open dired")
	"d T" '(dired-jump-other-window :wk "Split buffer and open dired at current directory")
	"d C-t" '(dired-other-window :wk "Select dir split buffer and open it")
	"d j" '(dired-jump :wk "Dired jump to current")
	)

(start/leader-keys
	"D" '(:ignore t :wk "Debug")
	"D d" '(dap-debug :wk "Start debugging")
	"D e" '(dap-disconnect :wk "End debugging")
	"D r" '(dap-debug-restart :wk "Restart debugging")
	"D b" '(dap-breakpoint-toggle :wk "Toggle breakpoint")
	"D B" '(dap-breakpoint-condition :wk "Conditional breakpoint")
	"D l" '(dap-debug-last :wk "Debug last")
	"D n" '(dap-next :wk "Next")
	"D s" '(dap-step-in :wk "Step in")
	"D o" '(dap-step-out :wk "Step out")
	"D c" '(dap-continue :wk "Continue")
	"D h" '(dap-hydra :wk "DAP Hydra")
	)

(start/leader-keys 
	"u" '(:ignore t :wk "Undo tree")
	"u t" '(vundo :wk "Visual undo tree")
	;; ("q" . vundo-quit)
	;; ("?" . vundo-help)
	;; ("b" . vundo-stem-root)
	;; ("B" . vundo-stem-end)
	;; ("d" . vundo-diff)
	;; ("<down>" . vundo-next)
	;; ("<up>" . vundo-previous)
	)

(start/leader-keys
	"r" '(:ignore t :wk "Replace word/name/etc")
	"r w"  '(my/replace-word-at-point-single :wk "Replace at point (single)")
	"r W"  '(visual-replace-selected :wk "Replace word at point (buffer)")
	"r b"  '(visual-replace-regexp :wk "Find & replace in buffer")
	)

(start/leader-keys
	"L" '(:ignore t :wk "Listen music")
	"L b" '(emms-browser :wk "Browse buffer")
	"L o" '(my/audio-open-playlist :wk "Open playlist")
	"L c" '(my/audio-create-playlist :wk "Create a new playlist")
	"L C" '(emms-playlist-clear :wk "Clear current buffer playlist")
	"L r" '(my/tvd-emms-beginning-of-song :wk "Restart current song")
	"L R" '(emms-repeat-track :wk "Repeat current song")
	"L C-r" '(emms-repeat-playlist :wk "Repeat playlist")
	"L s" '(emms-shuffle :wk "Shuffle playlist")
	"L C-s" '(emms-sort :wk "Sort playlist")
	"L p" '(emms-pause :wk "Pause-Play playback")
	"L P" '(emms-start :wk "Start playback")
	"L S" '(emms-stop :wk "Stop playback")
	"L C-p" '(emms-previous :wk "Previous track")
  "L C-n" '(emms-next :wk "Next track")
	"L g" '(emms-playlist-mode-go :wk "Go to playlist buffer")
	"L G" '(emms-browser-display-playlist :wk "Go to browser playlist")
	"L O" '(emms-playlist-mode-go-popup :wk "Open playlist buffer in popup")
	"L f" '(emms-play-file :wk "Play a file")
	"L d" '(emms-play-directory :wk "Play a directory")
	"L D" '(emms-play-directory-tree :wk "Play a directory tree")
	"L u" '(emms-play-url :wk "Play a url")
	"L U" '(emms-add-url :wk "Add a url to browser")
	"L +" '(emms-volume-raise :wk "Raise volume")
  "L -" '(emms-volume-lower :wk "Lower volume")
	"L =" '(emms-seek-to :wk "Seek to position in track")
	"L <right>" '(emms-seek-forward :wk "Seek forward 10s in track")
  "L <left>" '(emms-seek-backward :wk "Seek backward 10s in track")
	"L 3" '(emms-add-m3u-playlist :wk "Add m3u playlist to browser")
	"L C-3" '(emms-play-m3u-playlist :wk "Play m3u playlist")
)

(start/leader-keys
	"w" '(:ignore t :wk "Webdev server")
	"w s" '((lambda ()
						(interactive)
						(httpd-start)
						(impatient-mode))
					:wk "Start server and add file")
	"w S" '(httpd-stop :wk "Stop web server")
	"w a" '(impatient-mode :wk "Add/remove another file to web server")
	"w o" '(my/open-impatient-preview :wk "Open web server in browser")
	)

(start/leader-keys
	"m" '(:ignore t :wk "Movment")
	"m n" '(:ignore t :wk "Goto next")
	"m n f" '((lambda () 
							(interactive)
							(evil-textobj-tree-sitter-goto-textobj "function.outer" nil nil)) 
						:wk "Function")
	"m n e" '((lambda ()                                                          
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
						:wk "End")
	"m n c" '((lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "class.outer" nil nil))
						:wk "Class")
	"m n o" '((lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))
		 				:wk "Outer")
	"m p" '(:ignore t :wk "Goto previous")
	"m p f" '((lambda ()
              (interactive)
						  (evil-textobj-tree-sitter-goto-textobj "function.outer" t nil))
						:wk "Function")
	"m p e" '((lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))
						:wk "End")
	"m p c" '((lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "class.outer" t nil))
						:wk "Class")
	"m p o" '((lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))
						:wk "Outer")
	)

(start/leader-keys
	"M" '(:ignore t :wk "Mail")
	"M ?" '(mu4e-display-manual :wk "Opem Manual")
	"M t" '(mu4e :wk "Open mu4e")
	"M q" '(mu4e-quit :wk "Quit mu4e")
	"M c" '(mu4e-compose-mail :wk "Compose/Send mail")
	"M s" '(mu4e-search :wk "Search mail")
	"M b" '(mu4e-headers-search-bookmark :wk "Search bookmarks")
	"M V" '(mu4e-view-action :wk "View action")
	"M r" '(mu4e-compose-reply-to :wk "Reply to message")
  "M f" '(message-forward :wk "Forward mail")
	"M D" '(message-dont-send :wk "Draft message")
	"M e" '(mu4e-compose-edit :wk "Edit Draft message")
	"M m" '(:ignore t :wk "Mark mail")
	"M m e" '(mu4e-marked-execute :wk "Execute mark")
	"M m E" '(mu4e-mark-execute-all :wk "Execute all marks")
	"M m u" '(mu4e-view-mark-for-unmark :wk "UnMark mail")
	"M m m" '(mu4e-view-mark-for-move :wk "Mark to move mail")
	"M m r" '(mu4e-view-mark-for-read :wk "Mark to read mail")
	"M m R" '(mu4e-view-mark-for-unread :wk "Mark to unread mail")
	"M m C-r" '(mu4e-view-mark-for-refile :wk "Mark to refile mail")
	"M m f" '(mu4e-view-mark-for-flag :wk "Mark to flag mail")
	"M m F" '(mu4e-view-mark-for-unflag :wk "Mark to unflag mail")
	"M m t" '(mu4e-view-mark-for-trash :wk "Mark to trash mail")
	"M m T" '(mu4e-view-mark-for-untrash :wk "Mark to untrash mail")
	"M m d" '(mu4e-view-mark-for-delete :wk "Mark to delete mail")
	"M m l" '(mu4e-view-mark-for-label :wk "Mark to label mail")
	"M m L" '(mu4e-view-mark-for-unlabel :wk "Mark to unlabel mail")
	"M m s" '(mu4e-view-mark-for-something :wk "Mark to something mail")
	"M v" '(:ignore t :wk "View attachments")
	"M v v" '(gnus-mime-view-part :wk "View attachments chose method")
	"M v e" '(gnus-mime-view-internally :wk "View in emacs attachments")
	"M v E" '(gnus-mime-view-externally :wk "View outside of emacs attachments")
	"M d" '(gnus-mime-save-part :wk "Save attachments")
	"M C-a" '(gnus-mime-action-on-part :wk "Action on attachments")
	"M a" '(mml-attach-file :wk "Attach a file/image to mail")
	"M M-a" '(mml-attach-external :wk "Attach a external file/image to mail")
	"M A" '(mu4e-compose-attach-captured-message :wk "Attach a captured email")
	"M U" '(mu4e-update-mail-and-index :wk "Update mu4e changes to mail")
	"M u" '(:ignore t :wk "Update mail")
	"M u p" '(update-pappanos-mail :wk "Update Pappanos email")
  "M u w" '(update-work-mail :wk "Update Work email")
  "M u g" '(update-gamedev-mail :wk "Update Gamedev email")
	"M u u" '(update-user-mail :wk "Update User email")
  "M u m" '(update-mine-mail :wk "Update Mine email")
  "M u M" '(update-money-mail :wk "Update Money email")
  "M u o" '(update-overwhatch-mail :wk "Update Overwhatch email")
)

(start/leader-keys 
	"n" '(:ignore t :wk "Notes/Novel")
  "n <left>"  '(nov-previous-document :wk "Novel previous-page")
	"n <right>"   '(nov-next-document :wk "Novel next-page")
  "n M"  '(nov-display-metadata :wk "Novel metadata")
  "n i"  '(nov-goto-toc :wk "Novel imenu")
  "n C-s" '((lambda () (interactive) (save-buffer) (org-babel-tangle) (refresh-my-agenda-files))
					:wk "Org save")
	"n b" '(org-roam-buffer-toggle :wk "Org Buffer whith refrences to nodes")
	"n f" '(org-roam-node-find :wk "Org Open node")
	"n c" '(my/org-roam-create-node-in-directory :wk "Org Create node in dir")
	"n L" '(org-roam-node-insert :wk "Org Link node")
	"n l" '(my/org-roam-insert-or-create-node :wk "Org Make and link node")
	"n g" '(org-roam-graph :wk "Org Open Graph")
	"n h" '(org-id-get-create :wk "Org Make a header/file into a node")
	"n a" '(org-roam-alias-add :wk "Org Make node alias")
	"n r" '(org-roam-alias-remove :wk "Org Remove node alias")
	"n u" '((lambda ()
							(interactive)
							(org-roam-ui-mode)
							(org-roam-ui-open))
						:wk "Org Enable and open org-R-ui")
)

(start/leader-keys
	"C-d" '(duplicate-dwim :wk "Duplicate line or region")
	"l T" '((lambda ()
						(interactive)
						(let ((width (read-number "Tab width: " 2)))
							(add-file-local-variable 'tab-width width)))
					:wk "Set org tab width")
  "I" '(iimage-mode :wk "Toggle inline images in buffer")
  "S" '(flyspell-correct-at-point :wk "Toggle spell checker options")
  "C-s" '(jinx-correct :wk "Toggle alternative-spell checker options")
  "M-s" '(jinx-mode :wk "Enable/disable spell checker")
	)

(start/leader-keys
  "c" '(:ignore t :wk "Complition/cape/company/etc")
	"c c" '(cape-prefix-map :wk "Custom cape completions")
  )

(start/leader-keys
  "C" '(:ignore t :wk "Calendar")
	"C c" '(cfw:open-org-calendar :wk "Open org+mails specific Calendar")
	"C q" '(cfw:org-clean-exit :wk "Quit Calendar properly")
	"C d" '(org-gcal-delete-at-point :wk "Delete at point {file-mail.org}")
	"C f" '(org-gcal-fetch :wk "Fetch changes in calendar")
	"C s" '(org-gcal-sync :wk "Sync calendar")
	"C t" '(cfw:open-calendar-buffer :wk "Open main Calendar")
	"C g" '((lambda () 
						(interactive)
						(cfw:open-ical-calendar "https://calendar.google.com/calendar/ical/user.idc004%40gmail.com/private-a9c27b6aeffca1f9755f2a3a12d2bb57/basic.ics")) 
					:wk "Open google Calendar (user.idc)")
  )

(start/leader-keys
  "C-c" '(:ignore t :wk "Cheatsheet")
	"C-c s" '(cheat-sh :wk "Search cheat.sh")
	"C-c r" '(cheat-sh-region :wk "Search region in cheat.sh")
	"C-c i" '(devdocs-install :wk "Install documantetion")
	"C-c D" '(devdocs-delete :wk "Delete documantetion")
	"C-c u" '(devdocs-update-all :wk "Update all documantetion")
	"C-c d" '(devdocs-lookup :wk "Search specific library documantetion")
	"C-c o" '(devdocs-peruse :wk "Open library documantetion")
  )

(start/global
	;; "C-<down>"  'evil-window-down   ;; Move to the window below
	;; "C-<up>"    'evil-window-up       ;; Move to the window above
	;; "C-<left>"  'evil-window-left   ;; Move to the window on the left
	;; "C-<right>" 'evil-window-right ;; Move to the window on the right
	"C-<down>"  (lambda () (interactive) (evil-window-down 1) (golden-ratio)) ;; Move to the window below
	"C-<up>"    (lambda () (interactive) (evil-window-up 1) (golden-ratio)) ;; Move to the window above
	"C-<left>"  (lambda () (interactive) (evil-window-left 1) (golden-ratio)) ;; Move to the window on the left
	"C-<right>" (lambda () (interactive) (evil-window-right 1) (golden-ratio)) ;; Move to the window on the right)
	;; "C-M-<down>" 'evil-window-move-very-bottom ;; Move to the bottom of the window"
	;; "C-M-<up>" 'evil-window-move-very-top    ;; Move to the top of the window
	;; "C-M-<left>" 'evil-window-move-far-left   ;; Move to the far left of the window
	;; "C-M-<right>" 'evil-window-move-far-right ;; Move to the far right of the window
	"C-M-<down>" 'buf-move-down ;; Move to the bottom of the window"
	"C-M-<up>" 'buf-move-up    ;; Move to the top of the window
	"C-M-<left>" 'buf-move-left   ;; Move to the far left of the window
	"C-M-<right>" 'buf-move-right ;; Move to the far right of the window
	"C-s" nil
	;; "C-s" (lambda () (interactive) (save-buffer))
	"C-s" 'save-buffer
	"C-h f" nil
	"C-h v" nil
	"C-h k" nil
	"C-h x" nil
	"C-h c" nil
	"C-h F" nil
	;; "C-h f" 'helpful-callable
	;; "C-h v" 'helpful-variable
	"C-h f" 'counsel-describe-function
	"C-h v" 'counsel-describe-variable
	"C-h k" 'helpful-key
	"C-h x" 'helpful-command
	"C-h c" 'helpful-at-point
	"C-h F" 'helpful-function
	)

(start/global-keys-no-insert
  "<escape>" 'keyboard-escape-quit
  ;; "C-<tab>" 'switch-to-next-buffer
  ;; "C-S-<iso-lefttab>" 'switch-to-prev-buffer
  "C-<tab>" 'centaur-tabs-forward
	"C-<iso-lefttab>" 'centaur-tabs-backward
	"C-SPC C-SPC" 'centaur-tabs-counsel-switch-group
  ;; "C-SPC p" 'projectile-command-map
	)

(start/Nvim-Keys
  "M-z" 'evil-emacs-state
  "C-z" 'evil-undo
  "C-r" 'evil-redo
  "M-a" (lambda () (interactive) (evil-goto-first-line) (evil-visual-line) (evil-goto-line) (move-end-of-line nil))
  "M-SPC" 'er/expand-region
	"M-<backspace>" 'er/contract-region
	)

;; Keybindings matching Neovim behavior
(start/Nvim-Keys-normal-and-visual
  "S-<down>" nil
  "S-<up>" nil
  "S-<up>"   'drag-stuff-up
	"S-<down>" 'drag-stuff-down
	)





(start/Nvim-Keys-normal-only
  "C-s" nil
  "C-s" (lambda () (interactive) (save-buffer) (org-babel-tangle))
	"C-g d" 'lsp-find-definition
  "C-g r" 'lsp-find-references
  "C-g i" 'lsp-find-implementation
  "C-g t" 'lsp-find-type-definition
  "C-g D" 'lsp-find-declaration
	"C-g p" 'lsp-ui-peek-find-definitions
  "C-g R" 'lsp-ui-peek-find-references
  "C-g I" 'lsp-ui-peek-find-implementation

;; s-l g g – lsp-find-definition
;; Jumps to the definition of the symbol under the cursor. This means the point where it's implemented or declared, like a function definition or variable declaration.

;; s-l g r – lsp-find-references
;; Lists all references to that symbol across your workspace—basically, every place it's being used.

;; s-l g i – lsp-find-implementation
;; Finds the implementation(s) of an interface, method, or virtual function. It's more specific than definition, focusing on concrete implementations, especially in object-oriented or interface-driven languages.

;; s-l g t – lsp-find-type-definition
;; Jumps to the type definition of the symbol—for example, if you're on a variable name, it navigates to its type or class definition.

;; s-l g d – lsp-find-declaration
;; Finds the declaration of a symbol (for example, a function prototype in C or a forward declaration). Similar to “definition,” but especially useful in languages where declarations and definitions are distinct.
	)

;; Remap < and > to the custom functions
(start/Nvim-Keys-visual-only
  "<" nil
  ">" nil
  "<" 'my/evil-shift-left-and-restore
  ">" 'my/evil-shift-right-and-restore
	)

(start/Nvim-Keys-insert-only
  "TAB" nil
  "S-TAB" nil
	"M-TAB" 'codeium-overlay-accept-suggested-completion
	;;   "TAB" 'tab-to-tab-stop
	;;   "S-TAB" 'corfu-next
	"C-v" nil
  "C-v" 'evil-paste-after
	"C-a" nil
  ;; "C-c" 'my-codeium-company-complete
  "C-a" 'my/codeium-only-completion
	"C-SPC SPC" 'company-complete
	"C-SPC C-SPC" 'company-complete
	)

(start/emacs-motion			
  "M-'" 'eval-expression				
)













(start/emacs-normal
  "C-S-<up>" nil
  "C-S-<down>" nil
  "C-S-<left>" nil
  "C-S-<right>" nil
  "C-S-<up>" 'enlarge-window
  "C-S-<down>" 'shrink-window
  "C-S-<left>" 'enlarge-window-horizontally
  "C-S-<right>" 'shrink-window-horizontally

  "C-c e" 'sudo-edit
  "C-c E" 'sudo-edit-find-file

	;; workspaces 
  "C-S-w" 'persp-switch
	"C-S-s" 'persp-switch-by-number
	"C-!" '(lambda () (interactive) (persp-switch-by-number 1))
  "C-@" '(lambda () (interactive) (persp-switch-by-number 2))
  "C-#" '(lambda () (interactive) (persp-switch-by-number 3))
	"C-$" '(lambda () (interactive) (persp-switch-by-number 4))
	"C-%" '(lambda () (interactive) (persp-switch-by-number 5))
	"C-^" '(lambda () (interactive) (persp-switch-by-number 6))
	"C-&" '(lambda () (interactive) (persp-switch-by-number 7))
	"C-*" '(lambda () (interactive) (persp-switch-by-number 8))
	"C-(" '(lambda () (interactive) (persp-switch-by-number 9))
	"C-)" '(lambda () (interactive) (persp-switch-by-number 0))
	)





(start/emacs-motion-normal
	"SPC q" '(kill-buffer-and-window :wk "Close window/buffer")
	)















)
(provide 'keymaps)
