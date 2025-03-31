;; Kickstart.emacs is *not* a distribution.
;; It's a template for your own configuration.
;;
;; Configure it from this config.org file.
;; Read every line, understand your configuration, and modify it as needed.
;;
;; Delete this once you're comfortable with your setup.

(require 'configs)

(setq user-emacs-directory "~/.config/MainEmacs/")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'example)
(require 'package_configs)  ;; Now loaded here instead of in config.org
(require 'keymaps)
(require 'packages)
;; The previous (require 'packages) has been replaced by loading package_configs.el below.

;; (start/hello)

(defun my/org-auto-tangle ()
  "Automatically tangle the org file if it contains a tangle header."
  (when (and (buffer-file-name)
             (string-suffix-p ".org" (buffer-file-name)))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'after-save-hook #'my/org-auto-tangle)
