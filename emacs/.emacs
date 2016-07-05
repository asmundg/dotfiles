;;; emacs.el --- Custom config
;;; Commentary:

;;; Code:
;; Bootstrap use-package
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)

;;; Disable keyboard fumble of death
(global-unset-key "\C-x\C-c")

(use-package aggressive-indent
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'csharp-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package cider
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package clojure-mode)

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook '(lambda () (c-set-offset 'arglist-intro '+))))

(use-package company
  :config
  (global-company-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :config
  (global-flycheck-mode 1)
  (setq flycheck-display-errors-delay 0.1))

(use-package flycheck-pos-tip
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package fsharp-mode)

(use-package helm
  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x))
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq
   helm-quick-update                     t ; do not display invisible candidates
   helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
   helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
   helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
   helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
   helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
   helm-ff-file-name-history-use-recentf t))

(use-package helm-ls-git
  :bind ("C-x C-g" . helm-browse-project))

(use-package helm-git-grep
  :bind (("C-c g" . helm-git-grep)
         ("C-x g" . helm-git-grep-at-point))
  :config
  (if (eq system-type 'windows-nt)
      (defun helm-git-submodule-grep-process ())))

(use-package magit
  :bind (("C-x v s" . magit-status)
         ("C-x v b" . magit-blame))
  :config
  (setq
   magit-last-seen-setup-instructions "1.4.0"
   magit-push-always-verify nil))

(use-package nxml
  :config
  (setq nxml-slash-auto-complete-flag t))

(use-package rainbow-delimiters
  :config
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'python-mode-hook #'rainbow-identifiers-mode)
  (add-hook 'csharp-mode-hook #'rainbow-identifiers-mode)
  (add-hook 'clojure-mode-hook #'rainbow-identifiers-mode))

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package smartparens
  :bind (("C-M-)" . sp-forward-slurp-sexp)
         ("C-M-(" . sp-forward-barf-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark))

; Suppress error "directory
; ~/.emacs.d/server is unsafe"
; on windows.)
(if (eq system-type 'windows-nt)
    (progn
      (set-frame-font "-outline-Consolas-normal-r-normal-normal-11-97-96-96-c-*-iso8859-1")
      (when (>= emacs-major-version 23) ; Stupid hack for running emacs as admin
        (defun server-ensure-safe-dir (dir) "Noop" t)))
  (set-frame-font "-outline-Terminus-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

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
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(global-font-lock-mode t nil (font-lock))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(indent-tabs-mode nil)
 '(require-final-newline t)
 '(select-enable-clipboard t)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(smartparens-global-mode t)
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq inhibit-splash-screen t)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

(require 'uniquify)

(put 'upcase-region 'disabled nil)

(provide 'emacs)
;;; .emacs ends here
