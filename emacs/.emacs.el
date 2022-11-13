;; Disable internal package manager
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
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

;; This installs and integrates straight with use-package, making
;; use-package use straight as long as we provide a :straight t
;; keyword.
(straight-use-package 'use-package)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(global-linum-mode -1)
(line-number-mode 1)
(column-number-mode 1)

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
			       (lambda (fg) (set-face-foreground 'mode-line fg))
			       orig-fg))))

(set-face-attribute 'default nil :font "Fira Code" :height 160)

(setq-default indent-tabs-mode nil)

(setq create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Assuming we switched option and command keys in OSX
(setq mac-option-modifier 'meta)

(global-set-key (kbd "C-q") 'kill-region)
(global-set-key (kbd "M-e") 'fill-paragraph)
(global-set-key (kbd "M-q") 'unfill-paragraph)
(global-set-key (kbd "C-l") 'goto-line)

(delete-selection-mode 1)

(winner-mode 1)

(global-so-long-mode)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(setq global-auto-revert-non-file-buffers t)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-w") 'backward-delete-word)

;; No quick exit emacs
(global-unset-key "\C-x\C-c")

;; No suspend
(global-unset-key "\C-z")

(use-package default-text-scale
  :straight t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(setq show-trailing-whitespace t)

(setenv "TERM" "screen-256color")
(let ((path-from-shell (shell-command-to-string "fish -l -c \"echo -n \\$PATH[1]; for val in \\$PATH[2..-1];echo -n \\\":\\$val\\\";end\"")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell ":")))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind (("C-<tab>" . copilot-accept-completion)
         ("C-S-<tab>" . copilot-accept-completion-by-line))
  :init
  (global-copilot-mode)
  (copilot-diagnose))

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

(use-package counsel-projectile
  :init
  (projectile-global-mode)
  :config
  (setq projectile-enable-caching t)
  :straight t)

(use-package wgrep
  :straight t)

(use-package company
  :straight t
  :delight
  :config
  (global-company-mode 1))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

(use-package format-all
  :straight (format-all :type git :host github :repo "lassik/emacs-format-all-the-code"
                        :fork (:host github :repo "asmundg/emacs-format-all-the-code" :branch "asmundg/expose-formatter-definition"))
  :hook ((c-mode-common
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
          sh-mode
          swift-mode
          typescript-mode
          yaml-mode
          web-mode) . format-all-mode)
  :config
  (define-format-all-formatter swiftformat-with-config
    (:executable "swiftformat")
    (:install (macos "brew install swiftformat"))
    (:languages "Swift")
    (:format (format-all--buffer-easy executable "--quiet" "--config" (concat (locate-dominating-file default-directory ".swiftformat") ".swiftformat"))))
  (define-format-all-formatter shfmt-with-options
    (:executable "shfmt")
    (:install
     (macos "brew install shfmt")
     (windows "scoop install shfmt"))
    (:languages "Shell")
    (:format
     (format-all--buffer-easy
      executable
      (if (buffer-file-name)
          (list "-filename" (buffer-file-name))
        (list "-ln" (cl-case (and (eql major-mode 'sh-mode)
                                  (boundp 'sh-shell)
                                  (symbol-value 'sh-shell))
                      (bash "bash")
                      (mksh "mksh")
                      (t "posix"))))
      (list "-i" "4" "-bn"))))

  (add-hook 'c-mode-common-hook (lambda () (setq-local format-all-formatters '(("C" clang-format) ("Objective-C" clang-format)))))
  (add-hook 'graphql-mode-hook (lambda () (setq-local format-all-formatters '(("GraphQL" prettier)))))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local format-all-formatters '(("Emacs Lisp" emacs-lisp)))))
  (add-hook 'js-mode-hook (lambda () (setq-local format-all-formatters '(("JavaScript" prettier)))))
  (add-hook 'json-mode-hook (lambda () (setq-local format-all-formatters '(("JSON" prettier)))))
  (add-hook 'markdown-mode-hook (lambda () (setq-local format-all-formatters '(("Markdown" prettier)))))
  (add-hook 'python-mode-hook (lambda () (setq-local format-all-formatters '(("Python" black)))))
  (add-hook 'swift-mode-hook (lambda () (setq-local format-all-formatters '(("Swift" swiftformat-with-config)))))
  (add-hook 'typescript-mode-hook (lambda () (setq-local format-all-formatters '(("TypeScript" prettier)))))
  (add-hook 'sh-mode-hook (lambda () (setq-local format-all-formatters '(("Shell" shfmt-with-options)))))
  (add-hook 'yaml-mode-hook (lambda () (setq-local format-all-formatters '(("YAML" prettier))))))

(define-advice org-edit-src-exit (:before (&rest _args) format-buffer)
  "Format source blocks before exit"
  (when 'format-all-formatters
    (format-all-buffer)))

(use-package editorconfig
  :straight t
  :delight
  :config
  (editorconfig-mode 1)
  (add-to-list 'editorconfig-indentation-alist '(swift-mode swift-mode:basic-offset)))

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

(use-package lsp-mode
  :straight t
  :after (flycheck which-key)
  :hook ((js-mode . lsp)
         (typescript-mode . lsp)
         (haskell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :init
  (setq lsp-keymap-prefix "s-l")
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tcp-connection (lambda (port) `("graphql-lsp" "server" "-m" "socket" "-p" ,(number-to-string port))))
                    :major-modes '(graphql-mode)
                    :initialization-options (lambda () `())
                    :server-id 'graphql))
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql")))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-lines 10)
  (setq lsp-ui-doc-position 'bottom)
  :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package magit
  :straight t
  :hook (git-commit-mode . (lambda () (setq fill-column 72)))
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

(use-package delight
  :straight t
  ;; Hide auto-revert-mode
  :config (delight 'auto-revert-mode))

(use-package org
  :straight t
  :after (ob-http ob-mermaid)
  :hook (
         ;; Refresh any images after running org-babel, in case the
         ;; command generated one.
         (org-babel-after-execute . org-redisplay-inline-images)
         (org-mode . org-indent-mode)
         (org-mode . flyspell-mode))
  ;; org has a custom fill-paragraph, which performs extra magic for
  ;; tables etc.
  :bind (:map org-mode-map ("M-e" . org-fill-paragraph))
  :config
  (setq
   org-directory "~/Sync"

   org-default-notes-file (concat org-directory "/notes.org")

   ;; Add syntax highlighting in src blocks
   org-src-fontify-natively t
   ;; Start org files with all trees collapsed
   org-startup-truncated nil

   org-agenda-breadcrumbs-separator "/"

   org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t% s %b")
                              (todo . "%i %-12:c %b")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (add-to-list 'org-agenda-files (concat org-directory "/agenda.org"))

  ;; org-babel allows execution of src blocks containing the following
  ;; languages.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (dot . t)
     (gnuplot . t)
     (http . t)
     (python . t)
     (shell . t)
     (mermaid . t)
     ))

  ;; Skip confirmation for src block execution for the following
  ;; languages.
  (defun my-org-confirm-babel-evaluate (lang body)
    (and (not (string= lang "http"))
         (not (string= lang "dot"))
         (not (string= lang "gnuplot"))
         (not (string= lang "mermaid"))))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; Configure executors for the given languages
  (setq org-src-lang-modes '(("C" . c)
                             ("C++" . c++)
                             ("asymptote" . asy)
                             ("bash" . sh)
                             ("calc" . fundamental)
                             ("cpp" . c++)
                             ("ditaa" . artist)
                             ("dot" . graphviz-dot)
                             ("elisp" . emacs-lisp)
                             ("http" . "ob-http")
                             ("mermaid" . mermaid)
                             ("ocaml" . tuareg)
                             ("screen" . shell-script)
                             ("shell" . sh)
                             ("sqlite" . sql))))

(require 'org-tempo)

(use-package org-download
  :straight t
  :after (org)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot))

(use-package ob-http
  :straight t)

(use-package ob-mermaid
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook ((python-mode csharp-mode typescript-mode clojure-mode javascript-mode objc-mode swift-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :straight t
  :hook ((python-mode csharp-mode typescript-mode clojure-mode javascript-mode objc-mode swift-mode) . rainbow-identifiers-mode))

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

(use-package powerline
  :straight t)

(use-package moe-theme
  :straight t
  :after (powerline)
  :config
  (moe-dark)
  (powerline-moe-theme))

(use-package moe-flycheck-mode-line
  :straight (moe-flycheck-mode-line :type git :host github :repo "asmundg/moe-flycheck-mode-line" :branch "asmundg/support-new-mode-syntax")
  :after (flycheck)
  :hook (flycheck-mode . moe-flycheck-mode-line-mode))

(use-package git-gutter-fringe
  :straight t
  :delight git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package shell-switcher
  :straight t
  :init
  (setq shell-switcher-mode t))

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :config (ivy-prescient-mode))

(use-package csharp-mode
  :straight t
  :config
  (setq-local company-backends '(company-dabbrev-code company-keywords)))

(use-package csv-mode
  :straight t)

(use-package graphviz-dot-mode
  :straight t)

(use-package gnuplot
  :straight t)

(use-package graphql-mode
  :straight t)

(use-package groovy-mode
  :straight t)

(use-package kotlin-mode
  :straight t)

(defun java-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'java-indent-setup)

;;(use-package indium
;;  :straight t)

(use-package json-mode
  :straight t
  :config
  (setq js-indent-level 2))

(use-package mermaid-mode
  :straight t)

(use-package mustache-mode
  :straight t)

(use-package nix-mode
  :straight t)

(use-package swift-mode
  :straight t)

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

(defun use-eslint-from-node-modules ()
  (when-let ((eslint (find-executable-from-node-modules "eslint")))
    (setq-local flycheck-javascript-eslint-executable eslint)))

(defun use-tslint-from-node-modules ()
  (when-let ((tslint (find-executable-from-node-modules "tslint")))
    (setq-local flycheck-typescript-tslint-original-source-executable tslint)))

(defun ts-lsp-flycheck ()
  (flycheck-add-next-checker 'lsp '(warning . typescript-tslint-original-source)))

(use-package typescript-mode
  :straight t
  :after flycheck
  :hook ((typescript-mode . ts-lsp-flycheck)
         (typescript-mode . use-tslint-from-node-modules)
         (typescript-mode . use-eslint-from-node-modules)
         (typescript-mode . flyspell-prog-mode))
  :mode "\\.tsx\\'")

(use-package yaml-mode
  :straight t)

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

(use-package which-key
  :straight t
  :delight
  :init
  (which-key-mode))
