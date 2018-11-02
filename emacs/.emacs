;;; emacs.el --- Custom config
;;; Commentary:

;;; Code:
;; Bootstrap use-package
(require 'package)
(package-initialize)

(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(setq package-pinned-packages
      '((jedi-core . "MELPA")
        (moe-theme . "MELPA")
        (flycheck . "MELPA")
        (use-package . "MELPA")))

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
(add-to-list 'exec-path (concat (getenv "HOME") "/.pyenv/shims"))

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
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" default))))

(use-package aggressive-indent
  :delight
  :config
  ;;   (add-to-list
  ;;    'aggressive-indent-dont-indent-if
  ;;    '(and (derived-mode-p 'csharp-mode)
  ;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
  ;;                              (thing-at-point 'line)))))
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'csharp-mode-hook 'aggressive-indent-mode))

(winner-mode 1)

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

(use-package company-jedi
  :config
  (add-to-list 'company-backends 'company-jedi)
  :bind (
         :map python-mode-map
         ("M-." . jedi:goto-definition)
         ("M-," . jedi:goto-definition-pop-marker)))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook '(lambda ()
                                 (c-set-offset 'arglist-intro '+)
                                 (set-fill-column 140)
                                 (setq-local company-backends '(company-omnisharp company-dabbrev-code company-keywords))
                                 (company-mode)
                                 ;; Don't die horribly in magit ediff, where buffer-file-name is nil
                                 (when (and buffer-file-name
                                            (string-equal "tsx" (file-name-extension buffer-file-name)))
                                   (omnisharp-mode)))))

(use-package csv-mode)

(use-package company
  :delight
  :config
  (global-company-mode 1))

(use-package counsel
  :delight ivy-mode
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c s" . counsel-rg)
         ("C-c f" . counsel-projectile-find-file)
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
  (setq ivy-height 20
        counsel-rg-base-command "rg -i --hidden --no-heading --line-number --color never %s .")
  
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word))

(use-package counsel-dash
  :config
  (setq counsel-dash-common-docsets '("Javascript" "HTML" "React")))

(use-package counsel-projectile)

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package delight)

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1)
  (add-to-list 'editorconfig-indentation-alist '(swift-mode swift-mode:basic-offset)))

(use-package flow-minor-mode)

(use-package flx)

(use-package git-gutter-fringe
  :delight git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package git-timemachine)

(use-package goto-addr
  :init
  (add-hook 'compilation-mode-hook 'goto-address-mode)
  (add-hook 'prog-mode-hook 'goto-address-mode)
  (add-hook 'eshell-mode-hook 'goto-address-mode)
  (add-hook 'shell-mode-hook 'goto-address-mode)
  :bind (:map goto-address-highlight-keymap
              ("M-<RET>" . goto-address-at-point))
  :commands (goto-address-prog-mode
             goto-address-mode))

;; Haskell devenv
(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package ivy-pass)

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

(defun find-from-node-modules (path)
  "Check for path in project root node_modules, then from the current directory and up."
  (file-truename
   (let ((search-path (concat (file-name-as-directory "node_modules") path)))
     (concat (locate-dominating-file
              default-directory (lambda (d) (file-exists-p (concat d search-path))))
             search-path))))

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
        flycheck-pos-tip-timeout 600))

(use-package flycheck-objc-clang
  :hook (flycheck-mode . flycheck-objc-clang-setup))

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

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

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
  :after powerline
  :config
  (moe-dark)
  (powerline-moe-theme))

(eval-and-compile
  (defun elisp-load-path ()
    (concat (file-name-directory (file-truename "~/.emacs")) "elisp")))

(use-package moe-flycheck-mode-line
  :load-path (lambda () (elisp-load-path))
  :after flycheck
  :hook (flycheck-mode . moe-flycheck-mode-line-mode))

(use-package npm-mode
  :config
  (setq npm-global-mode t))

(use-package mustache-mode)

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
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (setq org-directory "~")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-src-fontify-natively t
        org-startup-truncated nil)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (dot . t)
     (http . t)
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
  (add-hook 'js-mode-hook #'use-prettier-from-node-modules)
  (add-hook 'markdown-mode-hook #'prettier-js-mode))

(defun configure-prettier ()
  "Find the appropriate prettierrc to use."
  (let ((rc (expand-file-name ".prettierrc" (projectile-project-root))))
    (when (file-exists-p rc)
      (setq-local prettier-js-args `(,(concat "--config " rc) "--write")))))

(use-package projectile
  :init
  (projectile-global-mode)
  :config
  (setq projectile-require-project-root nil))

(use-package request-deferred)

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "PIPENV_MAX_DEPTH" "10"))

(use-package powershell)

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package elpy)

(setq python-shell-interpreter "python3")

(use-package rainbow-delimiters
  :hook ((python-mode csharp-mode tide-mode clojure-mode objc-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :hook ((python-mode csharp-mode tide-mode clojure-mode objc-mode) . rainbow-identifiers-mode))

(use-package restclient)

(use-package shell-switcher
  :init
  (setq shell-switcher-mode t))

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package smartparens
  :delight
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
  :delight
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

(use-package wgrep)

(use-package ws-butler
  :init
  (ws-butler-global-mode))

(use-package yaml-mode)

(use-package yarn-mode)

(use-package xterm-color
  :init
  (setq xterm-color-preserve-properties t
        eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter))

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

;; Custom defun magic

(defun url-decode-region (start end)
  "Replace a region between start and end in buffer, with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun tide-eldoc-at-point ()
  "print Typescript's idea of the type at the current point."
  (interactive)
  (if (tide-method-call-p) 
      (tide-command:signatureHelp #'message) 
    (when (looking-at "\\s_\\|\\sw") 
      (tide-command:quickinfo 
       (tide-on-response-success-callback response t 
         (message (tide-doc-text (plist-get response :body)))))))) 

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text."
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
(delight 'auto-revert-mode)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dired-recursive-deletes 'always)

(add-hook 'js-mode-hook #'flyspell-prog-mode)

(require 'uniquify)

(put 'upcase-region 'disabled nil)

;; Blink modeline on bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

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
