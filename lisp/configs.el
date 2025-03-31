;;; configs.el --- Non-package Emacs configuration

;; Increase GC threshold during startup for performance.
(setq gc-cons-threshold (* 50 1000 1000))

;; Auto-tangle the Org configuration when saving.
(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it.
Credit to Emacs From Scratch."
  (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

;; === Good Defaults ===
;; Disable unnecessary UI elements.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Enable delete-selection-mode so typed text replaces selected text.
(delete-selection-mode 1)

;; Adjust automatic indenting and pairing.
(electric-indent-mode -1)
(electric-pair-mode 1)

;; Disable cursor blinking.
(blink-cursor-mode -1)

;; Auto-reload files when they change on disk.
(global-auto-revert-mode 1)

;; Use relative line numbers.
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Smooth scrolling and mouse wheel behavior.
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 10)

;; Set tab width.
(setq tab-width 4)

;; Disable creation of backup and auto-save files.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Load a separate custom file if it exists.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; === Appearance Adjustments ===
;; Set transparency for all new frames.
(add-to-list 'default-frame-alist '(alpha-background . 100))

;; Set default font and line spacing.
(set-face-attribute 'default nil :height 120 :weight 'medium)
(add-to-list 'initial-frame-alist '(font . "MonaspiceRn Nerd Font-16.5"))
(add-to-list 'default-frame-alist '(font . "MonaspiceRn Nerd Font-16.5"))
(setq-default line-spacing 0.12)

;; === Runtime Performance ===
;; Once Emacs finishes startup, dial the GC threshold back down and increase process output.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))
            (setq read-process-output-max (* 1024 1024))))  ;; 1MB

;;; configs.el ends here

(provide 'configs)
