(setq user-emacs-directory "~/.config/MainEmacs/")

;; (add-to-list 'load-path "~/.emacs.d/copilot")
;; ;; (require 'copilot)
;; (add-to-list 'load-path "~/.emacs.d/codeium.el")
;; (add-to-list 'load-path "~/.emacs.d/codeium-overlay.el")
(load (expand-file-name "packages.el" user-emacs-directory))
(load (expand-file-name "package_configs.el" user-emacs-directory))
(load (expand-file-name "configs.el" user-emacs-directory))
(load (expand-file-name "keymaps.el" user-emacs-directory))

(defun my/org-auto-tangle ()
  "Automatically tangle the org file if it contains a tangle header."
  (when (string-match "#\\+auto_tangle: t" (buffer-string))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-auto-tangle)
