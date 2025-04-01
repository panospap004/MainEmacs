;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.
;;
;; It is *recommeded* to configure it from the *config.org* file.
;;
;; You can delete this when you're done. It's your config now. :)

;; (load "~/.configs/emacs/lisp/emacs-settings.el")

(setq user-emacs-directory "~/.config/MainEmacs/")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'example)
(require 'keymaps)
(require 'configs) 
(require 'packages) 
(require 'package_configs)  ;; Load package configurations

;; (load "~/.configs/emacs/lisp/packages.el")

;; (start/hello)

(defun my/org-auto-tangle ()
  "Automatically tangle the org file if it contains a tangle header."
  (when (string-match "#\\+auto_tangle: t" (buffer-string))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-auto-tangle)
