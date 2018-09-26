;;; moe-flycheck-mode-line.el --- Set moe-theme mode line color with Flycheck status -*- lexical-binding: t -*-

;; Copyright (c) 2013 Matías Croce <mati@nelumboweb.com.ar>
;;
;; Author: Matías Croce <mati@nelumboweb.com.ar>
;; Keywords: convenience language tools
;; Version: 0.1
;; Package-Requires: ((flycheck) (moe-theme))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Set moe-theme mode line color with Flycheck status
;;
;; This package provides the `moe-flycheck-mode-line-mode' minor mode which sets
;; the moe-theme color of the mode line according to the status of Flycheck
;; syntax checking.
;;
;; To enable this mode in Flycheck, add it to `flycheck-mode-hook':
;;
;; (add-hook 'flycheck-mode-hook 'moe-flycheck-mode-line-mode)
;;
;; Thanks go to:
;; - Sylvain Benner (syl20bnr) for the idea and code, which I shamelessly stole
;; from his flycheck-color-mode-line mode
;; - hiroko (kuanyui) for moe-theme, the best emacs theme ever!

;;; Code:

(require 'flycheck)
(require 'moe-theme)

;; Customization
(defun moe-flycheck-mode-line-customize ()
  "Open the customization group for the `moe-flycheck-mode-line' package."
  (interactive)
  (customize-group 'moe-flycheck-mode-line t))

;; Default values
(defcustom moe-flycheck-mode-line-error 'red
  "Mode line color theme for flycheck errors."
  :group 'moe-flycheck-mode-line)
(defcustom moe-flycheck-mode-line-warning 'orange
  "Mode line color theme for flycheck warnings."
  :group 'moe-flycheck-mode-line)
(defcustom moe-flycheck-mode-line-info 'blue
  "Mode line color theme for flycheck info."
  :group 'moe-flycheck-mode-line)
(defcustom moe-flycheck-mode-line-default 'green
  "Mode line color theme for flycheck default/no-error."
  :group 'moe-flycheck-mode-line)
(defcustom moe-flycheck-mode-line-check-failed 'purple
  "Mode line color theme for flycheck failed checks."
  :group 'moe-flycheck-mode-line)

(defun moe-flycheck-mode-line-reset ()
  "Reset the mode line theme."
  (moe-theme-set-color moe-flycheck-mode-line-default))

(defun moe-flycheck-mode-line-check-failed ()
  "Change mode line theme when flycheck checks fails."
  (moe-theme-set-color moe-flycheck-mode-line-check-failed))

(defun moe-flycheck-mode-line-update ()
  "Update the mode line theme according to the Flycheck status."
  (moe-flycheck-mode-line-reset)
  (-when-let (moe-theme-color (cond ((flycheck-has-current-errors-p 'error)
				     moe-flycheck-mode-line-error)
				    ((flycheck-has-current-errors-p 'warning)
				     moe-flycheck-mode-line-warning)
				    ((flycheck-has-current-errors-p 'info)
				     moe-flycheck-mode-line-info)))
    (moe-theme-set-color moe-theme-color)))

;;;###autoload
(define-minor-mode moe-flycheck-mode-line-mode
  "Minor mode to change moe-theme mode line with the Flycheck status.

When called interactively, toggle
`moe-flycheck-mode-line-mode'.  With prefix ARG, enable
`moe-flycheck-mode-line-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `moe-flycheck-mode-line-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `moe-flycheck-mode-line-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter nil
  :group 'flycheck
  :require 'moe-flycheck-mode-line
  (cond
   (moe-flycheck-mode-line-mode
    (add-hook 'flycheck-after-syntax-check-hook
	      #'moe-flycheck-mode-line-update nil t)
    (add-hook 'flycheck-syntax-check-failed-hook
	      #'moe-flycheck-mode-line-check-failed nil t)

    (moe-flycheck-mode-line-update))
   (:else
    (remove-hook 'flycheck-after-syntax-check-hook
		 #'moe-flycheck-mode-line-update t)
    (remove-hook 'flycheck-syntax-check-failed-hook
		 #'moe-flycheck-mode-line-reset t)

    (moe-flycheck-mode-line-reset))))

;;;###autoload
(custom-add-frequent-value 'flycheck-mode-hook 'mode-flycheck-mode-line-mode)

(provide 'moe-flycheck-mode-line)

;;; moe-flycheck-mode-line.el ends here
