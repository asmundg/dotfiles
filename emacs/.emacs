;;; emacs.el --- Custom config
;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(global-font-lock-mode t nil (font-lock))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(indent-tabs-mode nil)
 '(minimap-mode t)
 '(minimap-window-location (quote right))
 '(require-final-newline t)
 '(select-enable-clipboard t)
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
 '(minimap-active-region-background ((t (:background "dark slate gray")))))

(add-to-list 'load-path "~/.emacs.d/local/")
(add-to-list 'load-path "~/.emacs.d/config/")
(require 'custom-elpa)
(require 'config-csharp)
(require 'custom-git)
(require 'custom-haskell)
(require 'custom-js)
(require 'config-appearance)
(require 'custom-python)

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
(require 'uniquify)

(provide 'emacs)

;;; emacs.el ends here
(put 'upcase-region 'disabled nil)
