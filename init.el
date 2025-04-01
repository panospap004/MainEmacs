;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.
;;
;; It is *recommeded* to configure it from the *config.org* file.
;;
;; You can delete this when you're done. It's your config now. :)

;; (load "~/.configs/emacs/lisp/emacs-settings.el")

(setq user-emacs-directory "~/.config/MainEmacs/")

;;(require 'example)
;;(require 'keymaps)
;;(require 'configs) 
;;(require 'packages) 
;;(require 'package_configs)  ;; Load package configurations 
(load (expand-file-name "example.el" user-emacs-directory))
(load (expand-file-name "keymaps.el" user-emacs-directory))
(load (expand-file-name "configs.el" user-emacs-directory))
(load (expand-file-name "packages.el" user-emacs-directory))
(load (expand-file-name "package_configs.el" user-emacs-directory))

;; (load "~/.configs/emacs/lisp/packages.el")

;; (start/hello)

(defun my/org-auto-tangle ()
  "Automatically tangle the org file if it contains a tangle header."
  (when (string-match "#\\+auto_tangle: t" (buffer-string))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-auto-tangle)
