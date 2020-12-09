;;; emacs.el --- Custom config
;;; Commentary:

;;; Code:

;; Interface reduction
(setq inhibit-splash-screen t)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode -1)

;; Remove the built-in version of Org from the load-path
(require 'cl-seq)
(setq load-path
      (cl-remove-if
       (lambda (x)
         (string-match-p "org$" x))
       load-path))

;; straight.el setup
(defvar bootstrap-version)
;;(setq straight-vc-git-default-protocol 'ssh)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load org early to avoid the old version bundled with emacs getting required

;; Add straight support for use-package
(straight-use-package 'use-package)

(defun locate-dominating-file-concat (file name)
  (concat (locate-dominating-file file name) name))

;; Org config
(use-package org
  :straight t
  :hook ((org-babel-after-execute . org-redisplay-inline-images)
         (org-mode . org-indent-mode))
  :bind (:map org-mode-map ("M-e" . org-fill-paragraph))
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
     (gnuplot . t)
     (http . t)
     (python . t)
     (shell . t)
     ))
  (defun my-org-confirm-babel-evaluate (lang body)
    (and (not (string= lang "http"))
         (not (string= lang "dot"))
         (not (string= lang "gnuplot"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
        org-src-lang-modes '(("http" . "ob-http")
                             ("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("ditaa" . artist)
                             ("asymptote" . asy)
                             ("dot" . graphviz-dot)
                             ("sqlite" . sql)
                             ("calc" . fundamental)
                             ("C" . c)
                             ("cpp" . c++)
                             ("C++" . c++)
                             ("screen" . shell-script)
                             ("shell" . sh)
                             ("bash" . sh))))

(use-package ob-http
  :straight t)

(require 'org-tempo)
(require 'uniquify)

(setq server-socket-dir "~/.emacs.d/server")
(server-start)

;; System paths
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.node/bin" ":/usr/local/bin"))
(setenv "NODE_OPTIONS" "--max-old-space-size=8192")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (concat (getenv "HOME") "/.pyenv/shims"))

;; Assuming we switched option and command keys in OSX
(setq mac-option-modifier 'meta)

;; Don't indent using tabs
(setq-default indent-tabs-mode nil)
(setq system-time-locale "C")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Disable keyboard fumbles of death
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")

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

;; Put autosave and backups out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable lockfiles, to avoid triggering automatic dev tools all the time
(setq create-lockfiles nil)

;; Don't pop up extra windows in ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(delete-selection-mode 1)
(winner-mode 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dired-recursive-deletes 'always)

;; Blink modeline on bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

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
      (when (eq system-type 'darwin) (set-frame-font (format "-*-Cascadia Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" size)))
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

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun find-from-node-modules (path)
  "Check for PATH in project root node_modules, then from the current directory and up."
  (file-truename
   (let ((search-path (concat (file-name-as-directory "node_modules") path)))
     (concat (locate-dominating-file
              default-directory (lambda (d) (file-exists-p (concat d search-path))))
             search-path))))

(defun find-executable-from-node-modules (name)
  "Check for executable NAME in project root node_modules, then from the current directory and up."
  (find-from-node-modules (concat
                           (file-name-as-directory ".bin")
                           name
                           (if (eq system-type 'windows-nt) ".cmd" ""))))

(defun use-prettier-from-node-modules ()
  (when-let ((executable (find-executable-from-node-modules "prettier")))
    (setq-local prettier-js-command executable)))

(defun use-tslint-from-node-modules ()
  (when-let ((tslint (find-executable-from-node-modules "tslint")))
    (setq-local flycheck-typescript-tslint-original-source-executable tslint)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" default))))

(use-package delight
  :straight t
  :config (delight 'auto-revert-mode))

(use-package aggressive-indent
  :straight t
  :delight
  :hook ((emacs-lisp-mode
          csharp-mode
          graphviz-dot-mode
          swift-mode) . aggressive-indent-mode))

;; (use-package avy)

;; Blink line when switching to buffer
(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

;; (use-package cider
;;     :config
;;     (add-hook 'clojure-mode-hook 'cider-mode))

;; (use-package clojure-mode)

;; (use-package company-jedi
;;     :config
;;     (add-to-list 'company-backends 'company-jedi)
;;     :bind (
;;               :map python-mode-map
;;               ("M-." . jedi:goto-definition)
;;               ("M-," . jedi:goto-definition-pop-marker)))

(use-package csharp-mode
  :straight t
  :config
  (setq-local company-backends '(company-omnisharp company-dabbrev-code company-keywords)))

(use-package csv-mode
  :straight t)

(use-package company
  :straight t
  :delight
  :config
  (global-company-mode 1))

(use-package counsel
  :straight t
  :after (counsel-projectile)
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
        '((swiper . ivy--regex-ignore-order)
          (t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 20)

  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word))

(use-package counsel-dash
  :straight t
  :config
  (setq counsel-dash-common-docsets '("Javascript" "HTML" "React")))

(use-package counsel-projectile
  :straight t)

;; (use-package docker-compose-mode)

;; (use-package dockerfile-mode)

(use-package editorconfig
  :straight t
  :delight
  :config
  (editorconfig-mode 1)
  (add-to-list 'editorconfig-indentation-alist '(swift-mode swift-mode:basic-offset)))

(use-package elpy
  :straight t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; (use-package flow-minor-mode)

;; (use-package flx)

;; Auto format
(use-package format-all
  :straight (format-all :type git :host github :repo "lassik/emacs-format-all-the-code"
                        :fork (:host github :repo "asmundg/emacs-format-all-the-code" :branch "asmundg/expose-formatter-definition"))
  :hook ((clang-mode
          elisp-mode
          emacs-lisp-mode
          graphql-mode
          js-mode
          json-mode
          nix-mode
          markdown-mode
          nix-mode
          objc-mode
          python-mode
          swift-mode
          typescript-mode
          web-mode) . format-all-mode)
  :config
  (define-format-all-formatter swiftformat
    (:executable "swiftformat")
    (:install (macos "brew install swiftformat"))
    (:languages "Swift")
    (:format (format-all--buffer-easy executable "--quiet" "--config" (locate-dominating-file-concat default-directory ".swiftformat")))))

;; Show git line status in buffer gutter
(use-package git-gutter-fringe
  :straight t
  :delight git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package graphql-mode
  :straight t)

;; (use-package git-timemachine)

;; (use-package goto-addr
;;     :init
;;     (add-hook 'compilation-mode-hook 'goto-address-mode)
;;     (add-hook 'prog-mode-hook 'goto-address-mode)
;;     (add-hook 'eshell-mode-hook 'goto-address-mode)
;;     (add-hook 'shell-mode-hook 'goto-address-mode)
;;     :bind (:map goto-address-highlight-keymap
;;               ("M-<RET>" . goto-address-at-point))
;;     :commands (goto-address-prog-mode
;;                   goto-address-mode))

;; (use-package grip-mode)

;; ;; Haskell devenv
;; (use-package intero
;;     :config
;;     (add-hook 'haskell-mode-hook 'intero-mode))

;; (use-package ivy-pass)

(use-package default-text-scale
  :straight t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode 1)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message) line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))

  (flycheck-define-checker typescript-tslint-original-source
    "TypeScript style checker using TSLint.

Note that this syntax checker is not used if
`flycheck-typescript-tslint-config' is nil or refers to a
non-existing file.

See URL `https://github.com/palantir/tslint'."
    :command ("tslint" "--format" "json"
              (config-file "--config" flycheck-typescript-tslint-config)
              (option "--rules-dir" flycheck-typescript-tslint-rulesdir)
              (eval flycheck-tslint-args)
              source-original)
    :error-parser flycheck-parse-tslint
    :modes (typescript-mode))

  (setq flycheck-display-errors-delay 0.1
        flycheck-pos-tip-timeout 600)

  (add-to-list 'flycheck-checkers 'proselint)
  (add-to-list 'flycheck-checkers 'typescript-tslint-original-source))

(use-package flycheck-pos-tip
  :straight t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flycheck-swiftlint
  :straight t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-swiftlint-setup)))

;; (use-package flycheck-swift
;;     :config
;;     (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
;;     ;; ↑ Select the appropriate SDK version you use
;;     (setq flycheck-swift-target "arm64-apple-ios10")
;;     (eval-after-load 'flycheck '(flycheck-swift-setup)))

;; (use-package fsharp-mode
;;     :config
;;     (if (eq system-type 'windows-nt)
;;         (setq inferior-fsharp-program "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\4.0\\Framework\\v4.0\\fsi.exe\"")
;;         (setq fsharp-compile-command "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\4.0\\Framework\\v4.0\\fsc.exe\"")))

(use-package gnuplot
  :straight t)

(use-package graphviz-dot-mode
  :straight t)

(use-package lsp-haskell
  :straight t
  :config)

(use-package haskell-mode
  :straight t)

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

;; JS Debugging
(use-package indium
  :straight t)

(use-package json-mode
  :straight t
  :config
  (setq js-indent-level 2)
  (add-hook 'json-mode-hook #'prettier-js-mode))

(use-package ledger-mode
  :straight t)

(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (flycheck-add-next-checker 'lsp '(warning . typescript-tslint-original-source))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :after (flycheck which-key)
  :hook ((typescript-mode . lsp-deferred)
         (haskell-mode . lsp)
         (pytnon-mode . lsp)
         (lsp-mode . lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook lsp-mode
  :config
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-java
  :straight t
  :hook (java-mode . lsp))

;; (use-package markdown-mode
;;     :bind (:map markdown-mode-map
;;               ("M-n" . forward-paragraph)
;;               ("M-p" . backward-paragraph))
;;     :init
;;     (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
;;     (add-hook 'markdown-mode-hook #'flyspell-mode)
;;     :config
;;     (setq-local markdown-fontify-code-blocks-natively t)
;;     (setq-local markdown-code-lang-modes
;;         '(("ocaml" . tuareg-mode)
;;              ("elisp" . emacs-lisp-mode)
;;              ("ditaa" . artist-mode)
;;              ("asymptote" . asy-mode)
;;              ("dot" . fundamental-mode)
;;              ("sqlite" . sql-mode)
;;              ("calc" . fundamental-mode)
;;              ("C" . c-mode)
;;              ("cpp" . c++-mode)
;;              ("C++" . c++-mode)
;;              ("screen" . shell-script-mode)
;;              ("shell" . sh-mode)
;;              ("bash" . sh-mode)
;;              ("TypeScript" . typescript-mode))))

(use-package magit
  :straight t
  :hook
  (git-commit-mode . (lambda () (setq fill-column 72)))
  :bind (("C-x v s" . magit-status)
         ("C-x v b" . magit-blame-addition))
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-local-branches 'magit-insert-stashes)
  (setq
   magit-git-executable (if (eq system-type 'darwin) "/usr/local/bin/git" "git")
   magit-last-seen-setup-instructions "1.4.0"
   magit-push-always-verify nil
   ;; Always on linux, never on Windows, due to slooow
   magit-diff-refine-hunk (if (eq system-type 'windows-nt) nil 'all)))

;; (use-package multiple-cursors
;;     :bind (("C-S-c C-S-c" . mc/edit-lines)))

(use-package powerline
  :straight t)

(use-package moe-theme
  :straight t
  :after (powerline)
  :config
  (moe-dark)
  (powerline-moe-theme))

;; (eval-and-compile
;;     (defun elisp-load-path ()
;;         (concat (file-name-directory (file-truename "~/.emacs")) "elisp")))

(use-package moe-flycheck-mode-line
  :straight (moe-flycheck-mode-line :type git :host github :repo "choma/moe-flycheck-mode-line")
  :after (flycheck)
  :hook (flycheck-mode . moe-flycheck-mode-line-mode))

;; (use-package npm-mode
;;     :config
;;     (setq npm-global-mode t))

(use-package nix-mode
  :straight t)

(use-package mustache-mode
  :straight t)

(use-package org-re-reveal
  :straight t)

;; (use-package omnisharp
;;     :bind (
;;               :map omnisharp-mode-map
;;               ("M-." . omnisharp-go-to-definition)
;;               ("M-," . pop-tag-mark))
;;     :config
;;     (when (eq system-type 'windows-nt)
;;         (setq omnisharp-use-http t) ; Fake it, we need to launch manually on windows
;;         (setq omnisharp--server-info t)))

(use-package openapi-yaml-mode
  :after (yaml-mode)
  :straight (openapi-yaml-mode :type git :host github :repo "magoyette/openapi-yaml-mode"))

(use-package org-bullets
  :straight t
  :hook (org-mode) . (lambda () (org-bullets-mode 1)))

(use-package org-present
  :straight t)

(use-package org-tanglesync
  :straight t
  :hook ((org-mode . org-tanglesync-mode)
         ;; enable watch-mode globally:
         ((prog-mode text-mode) . org-tanglesync-watch-mode))
  )

;; (use-package prettier-js
;;     :hook ((tide-mode js-mode markdown-mode yaml-mode) . prettier-js-mode))

;; (defun configure-prettier ()
;;     "Find the appropriate prettierrc to use."
;;     (let* ((rc (expand-file-name ".prettierrc" (projectile-project-root)))
;;               (rc-config (if (file-exists-p rc) (list "--config" rc) '()))
;;               (ignore (expand-file-name ".prettierignore" (projectile-project-root)))
;;               (ignored-config (if (file-exists-p ignore) (list "--ignore-path" ignore) '())))
;;         (setq-local prettier-js-args (append rc-config ignored-config '("--write")))))

(use-package projectile
  :straight t
  :init
  (projectile-global-mode)
  :config
  (setq
   projectile-require-project-root nil
   projectile-enable-caching t))

;; (use-package request-deferred)

;; (use-package pipenv
;;     :hook (python-mode . pipenv-mode)
;;     :config
;;     (setenv "PIPENV_MAX_DEPTH" "10"))

;; (use-package powershell)

;; (use-package py-autopep8
;;     :hook (python-mode . py-autopep8-enable-on-save))

;; (use-package elpy)

;; (setq python-shell-interpreter "python3")

(use-package rainbow-delimiters
  :straight t
  :hook ((python-mode csharp-mode typescript-mode clojure-mode objc-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :straight t
  :hook ((python-mode csharp-mode typescript-mode clojure-mode objc-mode) . rainbow-identifiers-mode))

;; (use-package restclient)

(use-package shell-switcher
  :straight t
  :init
  (setq shell-switcher-mode t))

;; (use-package smart-mode-line
;;     :config
;;     (setq sml/theme 'respectful)
;;     (sml/setup))

(use-package smartparens
  :straight t
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

;; (defun setup-tide-mode ()
;;     "Only enable tide if we have a file buffer.

;; tide-setup will crash otherwise."
;;     (when (not (eq buffer-file-name nil))
;;         (tide-setup)))

(use-package swift-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :after flycheck
  :hook ((typescript-mode . use-tslint-from-node-modules)
         (typescript-mode . use-prettier-from-node-modules)
         (typescript-mode . flyspell-prog-mode))
  :mode "\\.tsx\\'")

;; ;; This requires node
;; (use-package tide
;;   :straight t
;;   :bind (
;;          :map tide-mode-map
;;          ("C-M-." . tide-references))
;;   :init
;;   (add-hook 'tide-mode-hook #'use-tslint-from-node-modules)
;;   (add-hook 'tide-mode-hook #'use-prettier-from-node-modules)
;;   (add-hook 'tide-mode-hook #'flyspell-prog-mode)
;;   (add-hook 'typescript-mode-hook #'tide-setup)
;;   :config
;;   (setq tide-always-show-documentation t
;;         company-tooltip-align-annotations t)
;;   (flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append))

;; (use-package ts-comint)

;; (use-package uuidgen)

;; Show available chord completions
(use-package which-key
  :straight t
  :delight
  :init
  (which-key-mode))

(use-package web-mode
  :straight t
  :config
  (setq web-mode-enable-auto-quoting nil))

;; Use for editing in ivy-occur
(use-package wgrep
  :straight t)

;; (use-package ws-butler
;;     :init
;;     (ws-butler-global-mode))

(use-package yaml-mode
  :straight t)

;; (use-package yarn-mode)

;; (use-package xterm-color
;;     :init
;;     (setq xterm-color-preserve-properties t
;;         eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;;     (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter))

;; (add-hook 'js-mode-hook #'flyspell-prog-mode)

(provide 'emacs)
