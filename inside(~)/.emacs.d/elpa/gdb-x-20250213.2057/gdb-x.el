;;; gdb-x.el --- Improve GDB-MI user interface -*- lexical-binding: t -*-
;;
;; Copyright © 2023 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@outlook.es>
;; Package-Version: 20250213.2057
;; Package-Revision: 9db6e02cc101
;; URL: https://codeberg.org/pastor/gdb-x
;; Keywords: extensions
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Extra utilities for improving the user interface of gdb-mi.el.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'gud)
(require 'gdb-mi)
(require 'hl-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recenter buffer when stepping and ensure `hl-line' is updated if enabled. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gdb-x--gud-source-center (true-file line)
  "Recenter GUD source buffer around the program counter source line.
TRUE-FILE is the GUD file to recenter and LINE is the last source line
executed."
  (let ((bf (gud-find-file true-file)))
    (save-excursion
	  (with-selected-window (get-buffer-window bf)
	    (save-restriction
          (goto-char (point-min)) (forward-line (1- line))
	      (recenter))))))

;; Centre GUD source buffer window and enable `hl-line' if needed.
(defun gdb-x--disassembly-highlight-and-recenter ()
  "Make sure that `hl-line' gets updated after updating disassembly buffer.
Also ensure that the last executed line is centred."
  (when-let* ((window (get-buffer-window
                       (gdb-get-buffer 'gdb-disassembly-buffer)))
              (featurep 'hl-line))
	(with-selected-window window
      (when (marker-position gdb-disassembly-position)
        (cond
	     (global-hl-line-mode
	      (global-hl-line-highlight))
	     ((and hl-line-mode hl-line-sticky-flag)
	      (hl-line-highlight)))
	    (goto-char gdb-disassembly-position)
	    (recenter)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Many windows arrangement. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar gdb-x-hide-mode-line t
  "Whether to hide the mode-line for `gdb-x-many-windows-mode' buffers.")

(defun gdb-x--get-non-dedicated-windows ()
  "Get Emacs windows that are not dedicated windows."
  (seq-filter #'(lambda (win) (not (window-dedicated-p win))) (window-list)))

(defun gdb-x--fit-window-to-buffer (buffer &optional preserve horizontal)
  "Advice for fitting assembly window to BUFFER after every update.
If PRESERVE is t, mark window size as preserved.  For more info read
`window-preserve-size'.  If preserve is fixed the fix, then windows size.
If HORIZONTAL is t, allow resizing the buffer width-wise."
  (when-let ((fit-window-to-buffer-horizontally horizontal)
             (window-resize-pixelwise t)
             (window (get-buffer-window
                      (gdb-get-buffer buffer))))
    (fit-window-to-buffer window)
    (when preserve
      (window-preserve-size window horizontal t))))

(defun gdb-x--display-in-side-window (buf direction slot width)
  "Show buffer BUF, and make that window a side window.
Read `display-buffer' for more information on the meaning of DIRECTION, SLOT and
WIDTH."
  (let ((buf-mode (with-current-buffer buf
		            major-mode))
        (mode-line-fmt (when gdb-x-hide-mode-line
                         '(mode-line-format . none))))
    (display-buffer-in-side-window buf
				                   `((mode . ,buf-mode)
				                     (side . ,direction)
				                     (slot . ,slot)
				                     (window-width . ,width)
				                     (window-parameters
                                      .
					                  (,mode-line-fmt
                                       (no-delete-other-windows . t)))))))

(defun gdb-x--display-buffer-in-direction (buf direction width)
  "Show buffer BUF, and make that window a side window.
Read `display-buffer' for more information on the meaning of DIRECTION, SLOT and
WIDTH."
  (when-let ((buf-mode (with-current-buffer buf
		                 major-mode))
	         (parent (or (car gdb-source-window-list)
		                 (car (gdb-x--get-non-dedicated-windows))))
             (mode-line-fmt (when gdb-x-hide-mode-line
                              '(mode-line-format . none))))
    (display-buffer-in-direction buf
				                 `((mode . ,buf-mode)
                                   (dedicated . t)
				                   (direction . ,direction)
				                   (window . ,parent)
				                   (window-width . ,width)
                                   (window-parameters
                                    .
					                (,mode-line-fmt
                                     (no-delete-other-windows . t)))))))

(defun gdb-x--display-in-atom-window (buf direction slot width)
  "Show buffer BUF, and make that window a side window.
Read `display-buffer' for more information on the meaning of DIRECTION, SLOT and
WIDTH."
  (let ((buf-mode (with-current-buffer buf
		            major-mode))
	    (parent (or (car gdb-source-window-list)
		            (car (gdb-x--get-non-dedicated-windows))))
        (mode-line-fmt (when gdb-x-hide-mode-line
                         '(mode-line-format . none))))
    (display-buffer-in-atom-window buf
				                   `((mode . ,buf-mode)
                                     (dedicated . t)
				                     (side . ,direction)
				                     (slot . ,slot)
				                     (window . ,parent)
                                     (preserve-size . (t . t))
				                     (window-width . ,width)
                                     (window-parameters
                                      .
					                  (,mode-line-fmt
                                       (no-delete-other-windows . t)))))))

(defun gdb-x-display-locals-buffer (&optional thread)
  "Display the local variables of current GDB stack.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (when-let ((buffer (gdb-get-buffer-create 'gdb-locals-buffer thread)))
    (gdb-x--display-in-side-window buffer
			                       'left
			                       0
			                       0.15)))

(defun gdb-x-display-breakpoints-buffer (&optional thread)
  "Display GDB breakpoints.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (when-let ((buffer (gdb-get-buffer-create 'gdb-breakpoints-buffer thread)))
    (gdb-x--display-in-side-window buffer
			                       'left
			                       1
			                       0.15)))

(defun gdb-x-display-stack-buffer (&optional thread)
  "Display GDB backtrace for current stack.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (when-let ((buffer (gdb-get-buffer-create 'gdb-stack-buffer thread)))
    (gdb-x--display-in-side-window buffer
			                       'left
			                       2
			                       0.15)))

(defun gdb-x-display-gdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (gdb-x--display-in-side-window gud-comint-buffer
			                     'bottom
			                     0
			                     0.5))

(defun gdb-x-display-io-buffer ()
  "Display IO of debugged program in a separate window."
  (interactive)
  (gdb-x--display-in-side-window (gdb-get-buffer-create 'gdb-inferior-io)
			                     'bottom
			                     1
			                     0.5))

(defun gdb-x--fit-window-to-disas-buffer (&rest _)
  "Fit `gdb-disassembly-buffer' to window."
  (gdb-x--fit-window-to-buffer 'gdb-disassembly-buffer t t))

(defun gdb-x-display-disassembly-buffer (&optional thread)
  "Display GDB disassembly information.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (when-let ((buffer (gdb-get-buffer-create 'gdb-disassembly-buffer thread)))
    (advice-add #'gdb-disassembly-handler-custom
                :after
                #'gdb-x--fit-window-to-disas-buffer)
    (gdb-x--display-in-side-window buffer
			                       'right
			                       0
			                       80)))

(defun gdb-x--fit-window-to-reg-buffer (&rest _)
  "Fit `gdb-registers-buffer' to window."
  (gdb-x--fit-window-to-buffer 'gdb-registers-buffer t t))

(defun gdb-x-display-registers-buffer (&optional thread)
  "Display GDB disassembly information.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (when-let ((buffer (gdb-get-buffer-create 'gdb-registers-buffer thread)))
    (advice-add #'gdb-disassembly-handler-custom
                :after
                #'gdb-x--fit-window-to-reg-buffer)
    (gdb-x--display-buffer-in-direction buffer
			                            'right
			                            17)))

(defun gdb-x--many-windows-remove-advice ()
  "Remove advice added by `gdb-x'."
  (advice-remove #'gdb-disassembly-handler-custom #'gdb-x--fit-window-to-disas-buffer)
  (advice-remove #'gdb-disassembly-handler-custom #'gdb-x--fit-window-to-reg-buffer))

;;;###autoload
(define-minor-mode gdb-x-many-windows-mode
  "Minor mode to toggle the display of all relevant GUD side windows."
  :global t
  :group 'gdb-x
  :lighter " gdb-x-many"
  (if gdb-x-many-windows-mode
      (progn
        (let ((gdb-src-buf (gdb-get-source-buffer))
              (ignore-window-parameters t))
          (display-buffer-full-frame (or gdb-src-buf
                                         (list-buffers-noselect))
                                     nil)
          (setq gdb-source-window-list (list (selected-window))))
        (gdb-x-display-breakpoints-buffer)
        (gdb-x-display-disassembly-buffer)
        (gdb-x-display-registers-buffer)
        (gdb-x-display-locals-buffer)
        (gdb-x-display-stack-buffer)
        (gdb-x-display-io-buffer)
        (select-window (gdb-x-display-gdb-buffer)))
    ;; Delete buffers and windows displayed by `gdb-x-many-windows-mode'.
    (dolist (gdb-x-display-fn '(gdb-x-display-breakpoints-buffer
                                gdb-x-display-disassembly-buffer
                                gdb-x-display-registers-buffer
                                gdb-x-display-locals-buffer
                                gdb-x-display-stack-buffer
                                gdb-x-display-io-buffer))
      (when-let ((gdb-buf-win (funcall gdb-x-display-fn)))
        (delete-window gdb-buf-win)))
    ;; Delete and redisplay `gud-comint-buffer' so it is not forced in a side
    ;; window.
    (when (buffer-live-p gud-comint-buffer)
      (delete-window (get-buffer-window gud-comint-buffer))
      (select-window (display-buffer-in-direction gud-comint-buffer
                                                  '((direction . leftmost)))))
    (gdb-x--many-windows-remove-advice)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restore window configuration on exit. Taken from             ;;
;; 'https://www.doof.me.uk/2019/06/09/making-emacs-gud-usable'. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gdb-x-gud-quit ()
  "Exit GUD."
  (interactive)
  (gud-basic-call "quit"))

(defun gdb-x--gud-sentinel-cleanup (orig-fun &rest args)
  "GUD sentinel cleanup wrapper function.
This is meant to be used as an arround advice to `gud-sentinel'.
ORIG-FUN is the adviced function and ARGS are its arguments."
  (when (and (memq (process-status (car args)) '(signal exit))
             gdb-x-many-windows-mode)
    (gdb-x-many-windows-mode -1))
  (apply orig-fun args)
  (when-let* ((buf gud-comint-buffer)
              (win (prog1
                       (get-buffer-window buf)
                     (kill-buffer buf))))
    (delete-window win)))

;;;###autoload
(define-minor-mode gdb-x-mode
  "Minor mode to advice GDB functions to:
- Recenter current line after each step, including assemlby buffer line.
- Highlight assembly buffer line if `hl-line-mode' is enabled."
  :global t
  :group 'gdb-x
  :lighter " gdb-x"
  (if gdb-x-mode
      (progn
        (advice-add #'gdb-disassembly-handler-custom :after #'gdb-x--disassembly-highlight-and-recenter)
        (advice-add #'gud-display-line :after #'gdb-x--gud-source-center)
        (advice-add #'gud-sentinel :around #'gdb-x--gud-sentinel-cleanup))
    (advice-remove #'gdb-disassembly-handler-custom #'gdb-x--disassembly-highlight-and-recenter)
    (advice-remove #'gud-display-line #'gdb-x--gud-source-center)
    (advice-remove #'gud-sentinel #'gdb-x--gud-sentinel-cleanup)))

(defun gdb-x-unload-function ()
  "Disable `gdb-x' library.
Called by `unload-feature'."
  (when gdb-x-many-windows-mode
    (gdb-x-many-windows-mode -1))
  (when gdb-x-mode
    (gdb-x-mode -1)))


(provide 'gdb-x)
;;; gdb-x.el ends here.
