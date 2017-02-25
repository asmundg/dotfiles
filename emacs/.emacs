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
(setq visible-bell 1)

;;; Disable keyboard fumble of death
(global-unset-key "\C-x\C-c")

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
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (auto-virtualenv restclient-helm restclient restclient-mode flycheck ts-comint default-text-scale tide helm-smex helm-config yaml-mode web-mode virtualenvwrapper use-package solarized-theme smartparens smart-mode-line slime rainbow-identifiers rainbow-delimiters python-mode powershell php-mode password-store omnisharp nvm mo-git-blame markdown-mode magit-find-file magit-filenotify lua-mode less-css-mode json-mode js2-mode jinja2-mode jedi iy-go-to-char helm-ls-git helm-git-grep helm-dash graphviz-dot-mode gnuplot-mode fsharp-mode flymake-haskell-multi flycheck-pos-tip flycheck-clojure find-file-in-project expand-region exec-path-from-shell csv-mode color-theme coffee-mode clojure-mode-extra-font-locking autopair aggressive-indent ac-haskell-process ac-cider)))
 '(require-final-newline t)
 '(select-enable-clipboard t)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; (use-package aggressive-indent
;;   :config
;;   (add-to-list
;;    'aggressive-indent-dont-indent-if
;;    '(and (derived-mode-p 'csharp-mode)
;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                              (thing-at-point 'line)))))
;;   (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package auto-virtualenv
  :init
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

(use-package cider
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package clojure-mode)

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook '(lambda ()
                                 (c-set-offset 'arglist-intro '+)
                                 (set-fill-column 120))))

(use-package company
  :config
  (global-company-mode 1))

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :bind (("M-." . jedi:goto-definition)
         ("M-," . jedi:goto-definition-pop-marker))
  :config
  (setq jedi:complete-on-dot t))

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package fill-column-indicator)

(defun use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name (if (eq system-type 'windows-nt)
                                            "node_modules/.bin/tslint.cmd"
                                          "node_modules/.bin/tslint")
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

(use-package flycheck
  :init
  (add-hook 'flycheck-mode-hook #'use-tslint-from-node-modules)
  :config
  (global-flycheck-mode 1)
  (setq flycheck-display-errors-delay 0.1
        flycheck-pos-tip-timeout 0))

(use-package flycheck-pos-tip
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package fsharp-mode
  :config
  (if (eq system-type 'windows-nt)
      (setq inferior-fsharp-program "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\4.0\\Framework\\v4.0\\fsi.exe\"")
    (setq fsharp-compile-command "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\4.0\\Framework\\v4.0\\fsc.exe\"")))

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

(use-package helm
  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files))
  :config
  (use-package helm-config :ensure f)
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
      (defun helm-git-grep-submodule-grep-process ())))

(use-package helm-smex
  :bind (("M-x" . helm-smex)))

(use-package helm-swoop
  :bind (("C-x f" . helm-swoop)))

(use-package json-mode
  :config
  (setq js-indent-level 2))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("M-n" . forward-paragraph)
              ("M-p" . backward-paragraph)))

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

(use-package powershell)

(use-package rainbow-delimiters
  :config
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'tide-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'python-mode-hook #'rainbow-identifiers-mode)
  (add-hook 'csharp-mode-hook #'rainbow-identifiers-mode)
  (add-hook 'tide-mode-hook #'rainbow-identifiers-mode)
  (add-hook 'clojure-mode-hook #'rainbow-identifiers-mode))

(use-package restclient)
(use-package restclient-helm)

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package smartparens
  :bind (("C-M-)" . sp-forward-slurp-sexp)
         ("C-M-(" . sp-forward-barf-sexp))
  :init
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (smartparens-global-mode 1)
  :config
  (require 'smartparens-config))

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark))

(defun setup-tide-mode ()
  "Only enable tide if we have a file buffer.

tide-setup will crash otherwise."
  (if (not (eq buffer-file-name nil))
      (tide-setup)))

;; This requires node
(use-package tide
  :init
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  :config
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))

(use-package ts-comint)

(use-package web-mode
  :mode "\\.tsx\\'"
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Don't die horribly in magit ediff, where buffer-file-name is nil
              (when (and buffer-file-name
                         (string-equal "tsx" (file-name-extension buffer-file-name)))
                (setup-tide-mode)))))

(use-package yaml-mode)

(add-to-list 'load-path "~/.emacs.d/local")

;; Quick and dirty font selection scheme
(defun fontify-frame (frame)
  (let ((size
         ;; Large Resolution
         (if (> (x-display-pixel-width) 2000)
             (if (> (cadr (assoc 'mm-size (car (display-monitor-attributes-list)))) 310)
                 ;; Large Display
                 13
               ;; Small Display
               18)
           ;; Low resolution
           12))
        (font (if (eq system-type 'windows-nt)
                  ;; Windows
                  "Consolas"
                ;; Proper OS
                "Terminus")))
    (set-frame-font (format "-outline-%s-normal-r-normal-normal-%d-97-96-96-c-*-iso8859-1" font size))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
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

(setq inhibit-splash-screen t)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode -1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

(require 'uniquify)

(put 'upcase-region 'disabled nil)

(provide 'emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
