;;; emacs.el --- Custom config
;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.emacs.d/")
(require 'custom-elpa)
(require 'custom-git)
(require 'custom-js)
(require 'custom-look)
(require 'custom-python)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-w") 'backward-delete-word)
(global-set-key (kbd "C-q") 'kill-region)
(global-set-key (kbd "M-q") 'copy-region-as-kill)
(global-set-key (kbd "M-e") 'fill-paragraph)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-v") 'hippie-expand)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes (quote ("b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" default)))
 '(global-font-lock-mode t nil (font-lock))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(indent-tabs-mode nil)
 '(require-final-newline t)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(setq inhibit-splash-screen t)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

(setq nxml-slash-auto-complete-flag t)
(setq skeleton-pair nil)
(autopair-global-mode)
(require 'uniquify)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)

(provide 'emacs)

;;; emacs.el ends here
