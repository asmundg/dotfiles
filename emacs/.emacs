;;; emacs.el --- Custom config
;;; Commentary:

;;; Code:
;; Bootstrap use-package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)
(setq visible-bell 1)


(setq server-socket-dir "~/.emacs.d/server")
(server-start)

;; Assuming we switched option and command keys in OSX
(setq mac-option-modifier 'meta)
(setq-default indent-tabs-mode nil)

(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.node/bin" ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")

;; Python
(when (eq system-type 'darwin)
  (setq python-environment-virtualenv (list (expand-file-name "~/Library/Python/2.7/bin/virtualenv") "--system-site-packages" "--quiet")))

;;; Disable keyboard fumbles of death
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")

;; Put autosave and backups out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable lockfiles on Windows, as they create all sorts of weird
;; file system effects
(when (eq system-type 'windows-nt)
  (setq create-lockfiles nil))

;; Don't pop up extra windows in ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" default)))
 '(package-selected-packages
   (quote
    (ob-http org clang-format prettier-js csv-mode flycheck-swift swift-mode powerline git-gutter-fringe git-timemachine yarn-mode web-mode which-key ts-comint tide smartparens smart-mode-line restclient rainbow-identifiers rainbow-delimiters powershell request-deferred omnisharp npm-mode multiple-cursors magit markdown-mode json-mode indium helm-git-grep helm-ls-git fsharp-mode flycheck-pos-tip expand-region default-text-scale jedi ivy-pass intero flx flow-minor-mode editorconfig dockerfile-mode docker-compose-mode counsel-projectile counsel company csharp-mode cider avy auto-virtualenv aggressive-indent use-package))))

(use-package aggressive-indent
  :config
  ;;   (add-to-list
  ;;    'aggressive-indent-dont-indent-if
  ;;    '(and (derived-mode-p 'csharp-mode)
  ;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
  ;;                              (thing-at-point 'line)))))
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'csharp-mode-hook 'aggressive-indent-mode))

(winner-mode 1)

(use-package auto-virtualenv
  :init
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

(use-package avy)

(use-package cider
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package clang-format
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (add-hook 'before-save-hook 'clang-format-buffer nil 'local))))

(use-package clojure-mode)

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook '(lambda ()
                                 (c-set-offset 'arglist-intro '+)
                                 (set-fill-column 140)
                                 (setq-local show-trailing-whitespace t)
                                 (setq-local company-backends '(company-omnisharp company-dabbrev-code company-keywords))
                                 (company-mode)
                                 ;; Don't die horribly in magit ediff, where buffer-file-name is nil
                                 (when (and buffer-file-name
                                            (string-equal "tsx" (file-name-extension buffer-file-name)))
                                   (omnisharp-mode)))))

(use-package csv-mode)

(use-package company
  :config
  (global-company-mode 1))

(use-package counsel
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c g" . counsel-rg)
         ("C-x C-g" . counsel-projectile-find-file)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-l" . counsel-esh-history)
         ("M-x" . counsel-M-x)
         ("C-c C-r" . ivy-resume)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers 1)
  (setq ivy-count-format "(%d/%d)")
  (setq ivy-wrap 1)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 30)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word))

(use-package counsel-projectile)

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package flow-minor-mode)

(use-package flx)

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package git-timemachine)

;; Haskell devenv
(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package ivy-pass)

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :bind (
         :map jedi-mode-map
              ("M-." . jedi:goto-definition)
              ("M-," . jedi:goto-definition-pop-marker))
  :config
  (setq jedi:complete-on-dot t))

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun find-executable-from-node-modules (name)
  "Check for executable NAME in project root node_modules, then from the current directory and up."
  (find-from-node-modules (concat
                           (file-name-as-directory ".bin")
                           name
                           (if (eq system-type 'windows-nt) ".cmd" ""))))

(defun find-from-node-modules (name)
  "Check for executable NAME in project root node_modules, then from the current directory and up."
  (let* ((relative-executable-path (concat
                                    (file-name-as-directory "node_modules")
                                    name))
         (toplevel (expand-file-name
                    relative-executable-path
                    (projectile-project-root)))
         (closest-parent (expand-file-name
                          relative-executable-path
                          (locate-dominating-file default-directory "node_modules"))))
    (seq-find 'file-exists-p (list closest-parent toplevel))))

(defun use-prettier-from-node-modules ()
  (when-let ((executable (find-executable-from-node-modules "prettier")))
    (setq-local prettier-js-command executable)))

(defun use-tsserver-from-node-modules ()
  (when-let ((executable (find-executable-from-node-modules "tsserver")))
    (setq-local tide-tsserver-executable executable)))

(defun use-tslint-from-node-modules ()
  (when-let ((tslint (find-executable-from-node-modules "tslint")))
    (setq-local flycheck-typescript-tslint-executable tslint)))

(defun use-tsun-from-node-modules ()
  (when-let ((tsun (find-executable-from-node-modules "tsun")))
    (setq-local ts-comint-program-command tsun)))

(use-package flycheck
  :config
  (global-flycheck-mode 1)
  (setq flycheck-display-errors-delay 0.1
        flycheck-pos-tip-timeout 0))

(use-package flycheck-pos-tip
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flycheck-swift
  :config
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
  ;; ↑ Select the appropriate SDK version you use
  (setq flycheck-swift-target "arm64-apple-ios10")
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package fsharp-mode
  :config
  (if (eq system-type 'windows-nt)
      (setq inferior-fsharp-program "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\4.0\\Framework\\v4.0\\fsi.exe\"")
    (setq fsharp-compile-command "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\4.0\\Framework\\v4.0\\fsc.exe\"")))

(use-package haskell-mode)

(use-package helm-git-grep
  :config
  (when (eq system-type 'windows-nt)
    (defun helm-git-grep-submodule-grep-process ())))

(use-package indium)

(use-package json-mode
  :config
  (setq js-indent-level 2)
  (add-hook 'json-mode-hook #'prettier-js-mode))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("M-n" . forward-paragraph)
              ("M-p" . backward-paragraph))
  :init
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  :config
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-code-lang-modes
              '(("ocaml" . tuareg-mode)
                ("elisp" . emacs-lisp-mode)
                ("ditaa" . artist-mode)
                ("asymptote" . asy-mode)
                ("dot" . fundamental-mode)
                ("sqlite" . sql-mode)
                ("calc" . fundamental-mode)
                ("C" . c-mode)
                ("cpp" . c++-mode)
                ("C++" . c++-mode)
                ("screen" . shell-script-mode)
                ("shell" . sh-mode)
                ("bash" . sh-mode)
                ("TypeScript" . typescript-mode))))

(use-package magit
  :bind (("C-x v s" . magit-status)
         ("C-x v b" . magit-blame))
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-local-branches 'magit-insert-stashes)
  (setq
   magit-last-seen-setup-instructions "1.4.0"
   magit-push-always-verify nil
   ;; Always on linux, never on Windows, due to slooow
   magit-diff-refine-hunk (if (eq system-type 'windows-nt) nil 'all)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

(use-package moe-theme
  :config
  (load-theme 'moe-dark)
  (eval-after-load 'powerline '(powerline-moe-theme)))

(use-package npm-mode
  :config
  (setq npm-global-mode t))

(use-package ob-http)

(use-package omnisharp
  :bind (
         :map omnisharp-mode-map
              ("M-." . omnisharp-go-to-definition)
              ("M-," . pop-tag-mark))
  :config
  (when (eq system-type 'windows-nt)
    (setq omnisharp-use-http t) ; Fake it, we need to launch manually on windows
    (setq omnisharp--server-info t)))

;; Org config
(use-package org
  :config
  (setq org-directory "~")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-src-fontify-natively t)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (http . t)
     (sh . t)
     (python . t)
     ))
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "http")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))

(use-package powerline)

(use-package prettier-js
  :config
  (add-hook 'tide-mode-hook #'prettier-js-mode)
  (add-hook 'tide-mode-hook #'configure-prettier)
  (add-hook 'tide-mode-hook #'use-prettier-from-node-modules)
  (add-hook 'js-mode-hook #'prettier-js-mode)
  (add-hook 'js-mode-hook #'configure-prettier)
  (add-hook 'js-mode-hook #'use-prettier-from-node-modules))

(defun configure-prettier ()
  "Find the appropriate prettierrc to use."
  (let ((rc (expand-file-name ".prettierrc" (projectile-project-root))))
    (when (file-exists-p rc)
      (setq-local prettier-js-args `(,(concat "--config " rc) "--write")))))

(use-package projectile
  :config
  (setq projectile-require-project-root nil))

(use-package request-deferred)

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
  (show-smartparens-global-mode)
  :config
  (require 'smartparens-config))

(defun setup-tide-mode ()
  "Only enable tide if we have a file buffer.

tide-setup will crash otherwise."
  (when (not (eq buffer-file-name nil))
    (tide-setup)))

(use-package swift-mode)

;; This requires node
(use-package tide
  :bind (
         :map tide-mode-map
              ("C-M-." . tide-references))
  :init
  (add-hook 'tide-mode-hook #'use-tslint-from-node-modules)
  (add-hook 'tide-mode-hook #'use-tsserver-from-node-modules)
  (add-hook 'tide-mode-hook #'use-tsun-from-node-modules)
  (add-hook 'tide-mode-hook #'flyspell-prog-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  :config
  (setq tide-always-show-documentation t)
  (flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package ts-comint)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package web-mode
  :mode "\\.tsx\\'"
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Don't die horribly in magit ediff, where buffer-file-name is nil
              (when (and buffer-file-name
                         (string-equal "tsx" (file-name-extension buffer-file-name)))
                (setup-tide-mode))))
  :config
  (setq web-mode-enable-auto-quoting nil))

(use-package yaml-mode)

(use-package yarn-mode)

;; Quick and dirty font selection scheme
(defun fontify-frame (frame)
  (when (display-graphic-p)
    (let ((size
           ;; Large Resolution
           (if (> (x-display-pixel-width) 2000)
               (if (> (cadr (assoc 'mm-size (car (display-monitor-attributes-list)))) 310)
                   ;; Large Display
                   13
                 ;; Small Display
                 18)
             ;; Low resolution
             12)))
      (when (eq system-type 'windows-nt) (set-frame-font (format "-outline-Consolas-normal-r-normal-normal-%d-97-96-96-c-*-iso8859-1" size)))
      (when (eq system-type 'gnu/linux) (set-frame-font (format "-outline-Inconsolata-normal-r-normal-normal-%d-97-96-96-c-*-iso8859-1" size)))
      )))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

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
(delete-selection-mode 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dired-recursive-deletes 'always)

(require 'uniquify)

(put 'upcase-region 'disabled nil)

;; Custom defun magic

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun tide-eldoc-at-point ()
  "print Typescript's idea of the type at the current point"
  (interactive)
  (if (tide-method-call-p) 
      (tide-command:signatureHelp #'message) 
    (when (looking-at "\\s_\\|\\sw") 
      (tide-command:quickinfo 
       (tide-on-response-success-callback response t 
         (message (tide-doc-text (plist-get response :body)))))))) 

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-w") 'backward-delete-word)
(global-set-key (kbd "C-q") 'kill-region)
(global-set-key (kbd "M-e") 'fill-paragraph)
(global-set-key (kbd "M-q") 'unfill-paragraph)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-v") 'hippie-expand)

(provide 'emacs)
;;; .emacs ends here
(if (eq system-type 'windows-nt)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(ivy-current-match ((t :background "dark slate gray")))
     '(ivy-minibuffer-match-face-2 ((t :foreground "#002b36" :background "green" :weight bold)))
     '(swiper-current-match ((t :background "dark slate gray")))
     '(swiper-line-face ((t :background "dark slate gray")))
     '(swiper-match-face-2 ((t :foreground "#002b36" :background "green" :weight bold)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
