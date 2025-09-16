;;; mu4e-marker-icons.el --- Display icons for mu4e markers -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "26.1") (nerd-icons "0.0.1"))
;; Package-Version: 20250228.218
;; Package-Revision: 13541181d514
;; Keywords: mail
;; homepage: https://repo.or.cz/mu4e-marker-icons.git

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.
;;
;; mu4e-marker-icons is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; mu4e-marker-icons is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage
;;
;; (mu4e-marker-icons-mode 1)

;;; Code:

(require 'mu4e-headers)
(require 'nerd-icons)

(defgroup mu4e-marker-icons nil
  "Display icons for mu4e markers."
  :group 'mu4e-marker-icons)

(defcustom mu4e-marker-icons-style 'nerd-icons
  "Prefer to use unicode icons over nerd-icons."
  :type '(choice
          :tag "An option to set mu4e-marker-icons style use Unicode or icon font."
          (const :tag "Unicode" unicode)
          (const :tag "nerd-icons" nerd-icons)
          (const :tag "all-the-icons" all-the-icons))
  :safe #'booleanp
  :group 'mu4e-marker-icons)

(defvar mu4e-marker-icons-marker-alist
  '((mu4e-headers-seen-mark . mu4e-marker-icons-saved-headers-seen-mark)
    (mu4e-headers-new-mark . mu4e-marker-icons-saved-headers-new-mark)
    (mu4e-headers-unread-mark . mu4e-marker-icons-saved-headers-unread-mark)
    (mu4e-headers-signed-mark . mu4e-marker-icons-saved-headers-signed-mark)
    (mu4e-headers-encrypted-mark . mu4e-marker-icons-saved-headers-encrypted-mark)
    (mu4e-headers-draft-mark . mu4e-marker-icons-saved-headers-draft-mark)
    (mu4e-headers-attach-mark . mu4e-marker-icons-saved-headers-attach-mark)
    (mu4e-headers-passed-mark . mu4e-marker-icons-saved-headers-passed-mark)
    (mu4e-headers-flagged-mark . mu4e-marker-icons-saved-headers-flagged-mark)
    (mu4e-headers-replied-mark . mu4e-marker-icons-saved-headers-replied-mark)
    (mu4e-headers-trashed-mark . mu4e-marker-icons-saved-headers-trashed-mark))
  "An alist of markers used in mu4e.")

(defun mu4e-marker-icons--store (l)
  "Store mu4e header markers value from L."
  (mapcar (lambda (x) (set (cdr x) (symbol-value (car x)))) l))

(defun mu4e-marker-icons--restore (l)
  "Restore mu4e header markers value from L."
  (let ((lrev (mapcar (lambda (x) (cons (cdr x) (car x))) l)))
    (mu4e-marker-icons--store lrev)))


(defun mu4e-marker-icons-enable ()
  "Enable mu4e-marker-icons."
  (mu4e-marker-icons--store mu4e-marker-icons-marker-alist)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-precise-alignment t)
  (pcase mu4e-marker-icons-style
    ;; The unicode icons is totally from http://xenodium.com/mu4e-icons, Thanks, Alvaro Ramirez.
    ('unicode
     (setq mu4e-headers-unread-mark    '("u" .  "📩")
           mu4e-headers-draft-mark     '("D" .  "🚧")
           mu4e-headers-flagged-mark   '("F" .  "🚩")
           mu4e-headers-new-mark       '("N" .  "✨")
           mu4e-headers-passed-mark    '("P" .  "🆗")
           mu4e-headers-replied-mark   '("R" .  "📧")
           mu4e-headers-seen-mark      '("S" .  " ")
           mu4e-headers-trashed-mark   '("T" .  "❎")
           mu4e-headers-attach-mark    '("a" .  "📎")
           mu4e-headers-encrypted-mark '("x" .  "🔐")
           mu4e-headers-signed-mark    '("s" .  "🔑")
           mu4e-headers-thread-duplicate-prefix '("Ⓓ" . "♊") ; ("=" . "≡ ")
           mu4e-headers-list-mark      '("s" . "📬")
           mu4e-headers-personal-mark  '("p" . "🙍")
           mu4e-headers-calendar-mark  '("c" . "📅"))
     (setq mu4e-modeline-all-clear '("C:" . "🌀")
           mu4e-modeline-all-read '("R:" . "✅")
           mu4e-modeline-unread-items '("U:" . "📫")
           mu4e-modeline-new-items '("N:" . "🔥")))
    ('nerd-icons
     (setq
      mu4e-headers-seen-mark `("S" . ,(nerd-icons-mdicon "nf-md-email_open_outline" :face 'nerd-icons-dsilver))
      mu4e-headers-new-mark `("N" . ,(nerd-icons-mdicon "nf-md-email_mark_as_unread" :face 'nerd-icons-lgreen))
      mu4e-headers-unread-mark `("u" . ,(nerd-icons-mdicon "nf-md-email_outline" :face 'nerd-icons-green))
      mu4e-headers-signed-mark `("s" . ,(nerd-icons-mdicon "nf-md-email_seal_outline" :face 'nerd-icons-blue)) ; "nf-md-email_check_outline"
      mu4e-headers-encrypted-mark `("x" . ,(nerd-icons-mdicon "nf-md-email_lock" :face 'nerd-icons-purple))
      mu4e-headers-draft-mark `("D" . ,(nerd-icons-mdicon "nf-md-email_edit_outline" :face 'nerd-icons-orange))
      mu4e-headers-attach-mark `("a" . ,(nerd-icons-mdicon "nf-md-email_plus_outline" :face 'nerd-icons-lorange))
      mu4e-headers-passed-mark `("P" . ,(nerd-icons-mdicon "nf-md-email_fast_outline" :face 'nerd-icons-lpink)) ; ❯ (I'm participated in thread) / Forward
      mu4e-headers-flagged-mark `("F" . ,(nerd-icons-mdicon "nf-md-email_alert_outline" :face 'nerd-icons-lred))
      mu4e-headers-replied-mark `("R" . ,(nerd-icons-mdicon "nf-md-reply" :face 'nerd-icons-silver))
      mu4e-headers-trashed-mark `("T" . ,(nerd-icons-mdicon "nf-md-trash_can_outline" :face 'nerd-icons-dsilver))
      mu4e-headers-thread-duplicate-prefix `("=" . ,(nerd-icons-mdicon "nf-md-content_duplicate" :face 'nerd-icons-dorange))
      mu4e-headers-list-mark `("s" . ,(nerd-icons-codicon "nf-cod-list_tree" :face 'nerd-icons-silver))
      mu4e-headers-personal-mark `("p" . ,(nerd-icons-codicon "nf-cod-person" :face 'nerd-icons-cyan-alt))
      mu4e-headers-calendar-mark `("c" . ,(nerd-icons-mdicon "nf-md-calendar_import" :face 'nerd-icons-lorange)))
     (setq mu4e-modeline-all-clear `("C:" . ,(nerd-icons-mdicon "nf-md-email_open_outline" :face 'nerd-icons-green))
           mu4e-modeline-all-read `("R:" . ,(nerd-icons-mdicon "nf-md-email_mark_as_unread" :face 'nerd-icons-blue))
           mu4e-modeline-unread-items `("U:" . ,(nerd-icons-mdicon "nf-md-email_plus_outline" :face 'nerd-icons-orange))
           mu4e-modeline-new-items `("N:" . ,(nerd-icons-mdicon "nf-md-email_alert_outline" :face 'nerd-icons-red))))
    ('all-the-icons
     ;; TODO
     )))

(defun mu4e-marker-icons-disable ()
  "Disable mu4e-marker-icons."
  (mu4e-marker-icons--restore mu4e-marker-icons-marker-alist))

;;;###autoload
(define-minor-mode mu4e-marker-icons-mode
  "Display icons for mu4e markers."
  :require 'mu4e-marker-icons-mode
  :init-value nil
  :global t
  (if mu4e-marker-icons-mode
      (mu4e-marker-icons-enable)
    (mu4e-marker-icons-disable)))



(provide 'mu4e-marker-icons)

;;; mu4e-marker-icons.el ends here
