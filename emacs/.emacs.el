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

;; Emacs ships these. Straight skips them when another package
;; depends on them (org-roam on org, copilot on editorconfig).
(setq straight-built-in-pseudo-packages
      (append '(org use-package bind-key which-key editorconfig csharp-mode)
              straight-built-in-pseudo-packages))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(tool-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
				(lambda (fg) (set-face-foreground 'mode-line fg))
				orig-fg))))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Iosevka" :height 160))

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

(global-so-long-mode)

(use-package exec-path-from-shell
  :straight t
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(setenv "TERM" "screen-256color")

(setenv "NPM_AUTH_TOKEN" "")

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Redirect customizations outside the main config, to avoid spurious diffs
  (setq custom-file "~/.emacs.d/custom.el")
  (when (file-exists-p custom-file)
    (load custom-file))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package org
  :after (ob-http ob-mermaid)
  :hook (
         ;; Refresh any images after running org-babel, in case the
         ;; command generated one.
         (org-babel-after-execute . org-redisplay-inline-images)
         (org-mode . org-indent-mode)
         (org-mode . flyspell-mode))
  ;; org has a custom fill-paragraph, which performs extra magic for
  ;; tables etc.
  :bind (:map org-mode-map ("M-e" . org-fill-paragraph)
              ("C-c C-." . org-time-stamp-inactive))
  :config
  (defun my/org-agenda-prefix-with-roam-title ()
    "Get org-roam title for agenda prefix."
    (let ((file (buffer-file-name (org-base-buffer (current-buffer)))))
      (if (and file (org-roam-file-p file))
          (let* ((title (or (caar (org-roam-db-query
                                   [:select title :from nodes
                                            :where (= file $s1)] file))
                            ""))
                 (formatted (format "%-20.20s" title)))
            (message "Title: '%s' Length: %d" formatted (length formatted))
            formatted)
        (format "%-20s" ""))))

  (setq
   org-directory "~/Sync"

   org-default-notes-file (concat org-directory "/notes.org")

   ;; Add syntax highlighting in src blocks
   org-src-fontify-natively t
   ;; Start org files with all trees collapsed
   org-startup-truncated nil

   org-agenda-breadcrumbs-separator "/"

   org-agenda-prefix-format '((agenda . "%i %(my/org-agenda-prefix-with-roam-title) %t %s")
                              (todo . "%i %(my/org-agenda-prefix-with-roam-title) %b")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c"))

   org-priority-lowest 9
   org-priority-highest 1
   org-priority-default 2

   org-agenda-custom-commands
   '(("c" "Simple agenda view"

      (
       (tags "PRIORITY=1"
             ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)))
              (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda "")
       (alltodo ""
                ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority 1)
                                                (org-agenda-skip-if nil '(scheduled deadline))))))))))

  (add-to-list 'org-agenda-files (concat org-directory "/agenda.org"))
  (add-to-list 'org-agenda-files (concat org-directory "/roam/"))
  (add-to-list 'org-modules 'org-agenda t)

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

  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

          PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

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
                             ("powershell" . powershell)
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
  (org-download-screenshot-method "pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot))

(use-package ob-http
  :straight t)

(use-package ob-mermaid
  :straight t)

(use-package ox-gfm
  :straight t)

(use-package org-roam
    :straight t
    :after (org)
    :hook (after-init . org-roam-mode)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n g" . org-roam-graph)
           ("C-c n c" . org-roam-capture))
    :custom
    (org-roam-directory (file-truename "~/Sync/roam"))
    (org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :if-new (file+head "%<%Y-%m-%d-%H_%M_%S>-${slug}.org"
                           ":PROPERTIES:
:CATEGORY: roam
:END:
#+title: ${title}\n#+date: %U\n")
        :unnarrowed t)))
    :config
    (make-directory "~/Sync/roam" t)
    (org-roam-db-autosync-mode))

(use-package org-tidy
  :straight t
  :hook
  (org-mode . org-tidy-mode))

(use-package org-habit-plus
  :after (org)
  :straight (org-habit-plus :type git :host github :repo "myshevchuk/org-habit-plus")
  :config
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-habit-plus t))

(use-package org-pandoc-import
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))

;; install required inheritenv dependency:
(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

;; for eat terminal backend:
(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

;; for vterm terminal backend:
(use-package vterm :straight t)

(defun my-claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; optional IDE integration with Monet
  ;;(add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;;(monet-mode 1)
  (setq claude-code-notification-function #'my-claude-notify)
  (setq use-default-font-for-symbols nil)
  (set-fontset-font t 'unicode (font-spec :family "JuliaMono"))
  (claude-code-mode))

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
          (counsel-rg . ivy--regex-plus)
          (t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 20)

  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-kill-word))

(use-package counsel-projectile
  :init
  (projectile-global-mode)
  :config
  (setq projectile-enable-caching t
        ;; Improve perf in large repos
        counsel-projectile-find-file-matcher 'ivy--re-filter)
  :straight t)

(use-package wgrep
  :straight t)

(use-package company
  :straight t
  :delight
  :config
  (global-company-mode 1))

(eval-after-load "dired" '(require 'dired-x))
;; Use system trash instead of rm
(setq delete-by-moving-to-trash t
;; Suggest other buffer as target when two direds are open
      dired-dwim-target t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode 1)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" "check" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " :")))) ": "
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))

  (setq flycheck-display-errors-delay 0.1
        flycheck-pos-tip-timeout 600)

  (add-to-list 'flycheck-checkers 'proselint)

  ;; Supports scenario-specific chaining. Specifically, we use this to
  ;; set up eslint to run after LSP when we're in typescript-mode.
  (advice-add 'flycheck-checker-get :around
              (lambda (fn checker property)
                (or (alist-get property (alist-get checker flycheck-checker-local-override))
                    (funcall fn checker property))))

  ;; Monkeypatch flycheck to support overriding CLI args when checking
  ;; that eslint can be enabled. For some reason, the default
  ;; implementation ignores flycheck-eslint-args when checking that
  ;; eslint can run, meaning it won't find plugins in monorepos with
  ;; shared config packages (since the config package contains plugin
  ;; dependencies and not the packages consuming the config).
  (advice-add
   'flycheck-eslint-config-exists-p
   :override
   (lambda ()
     (eql 0
          (apply #'flycheck-call-checker-process
                 (append (list 'javascript-eslint nil nil nil)
                         flycheck-eslint-args
                         (list "--print-config" (or buffer-file-name "index.js")))))))
  )

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

(use-package flycheck-color-mode-line
  :straight t
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :custom
  (flycheck-color-mode-line-face-to-color 'mode-line-active)
  (flycheck-mode-line-color nil)
  :config (custom-set-faces
           '(flycheck-color-mode-line-success-face ((t (:background "dark green" :foreground "white"))))
           '(flycheck-color-mode-line-info-face ((t (:inherit flycheck-color-mode-line-success-face))))
           '(flycheck-color-mode-line-error-face ((t (:background "dark red" :foreground "white"))))))

(defun my/moody-flycheck-face (args)
  (if-let* ((face (cdr-safe (bound-and-true-p flycheck-color-mode-line-cookie)))
            ((face-background face nil t)))
      (let ((args (append args (make-list (- 6 (length args)) nil))))
        (setf (nth 4 args) (or (nth 4 args) face))
        args)
    args))

(with-eval-after-load 'moody
  (advice-add 'moody-wrap :filter-args #'my/moody-flycheck-face))

(use-package format-all
  :straight (format-all :type git :host github :repo "lassik/emacs-format-all-the-code")
  :config
  ;; (define-format-all-formatter swiftformat-with-config
  ;;   (:executable "swiftformat")
  ;;   (:install (macos "brew install swiftformat"))
  ;;   (:languages "Swift")
  ;;   (:format (format-all--buffer-easy executable "--quiet" "--config" (concat (locate-dominating-file default-directory ".swiftformat") ".swiftformat"))))
  ;; (define-format-all-formatter shfmt-with-options
  ;;   (:executable "shfmt")
  ;;   (:install
  ;;    (macos "brew install shfmt")
  ;;    (windows "scoop install shfmt"))
  ;;   (:languages "Shell")
  ;;   (:format
  ;;    (format-all--buffer-easy
  ;;     executable
  ;;     (if (buffer-file-name)
  ;;         (list "-filename" (buffer-file-name))
  ;;       (list "-ln" (cl-case (and (eql major-mode 'sh-mode)
  ;;                                 (boundp 'sh-shell)
  ;;                                 (symbol-value 'sh-shell))
  ;;                     (bash "bash")
  ;;                     (mksh "mksh")
  ;;                     (t "posix"))))
  ;;     (list "-i" "4" "-bn"))))
  (add-hook 'c-mode-common-hook (lambda () (setq-local format-all-formatters '(("C" clang-format) ("Objective-C" clang-format)))))
  (add-hook 'graphql-mode-hook (lambda () (setq-local format-all-formatters '(("GraphQL" prettier)))))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local format-all-formatters '(("Emacs Lisp" emacs-lisp)))))
  (add-hook 'js-mode-hook (lambda () (setq-local format-all-formatters '(("JavaScript" prettier)))))
  (add-hook 'json-mode-hook (lambda () (setq-local format-all-formatters '(("JSON" prettier)))))
  (add-hook 'markdown-mode-hook (lambda () (setq-local format-all-formatters '(("Markdown" prettier)))))
  (add-hook 'swift-mode-hook (lambda () (setq-local format-all-formatters '(("Swift" swiftformat-with-config)))))
  (add-hook 'typescript-mode-hook (lambda () (setq-local format-all-formatters '(("TypeScript" prettier)))))
  (add-hook 'sh-mode-hook (lambda () (setq-local format-all-formatters '(("Shell" shfmt-with-options)))))
  (add-hook 'yaml-mode-hook (lambda () (setq-local format-all-formatters '(("YAML" prettier))))))

(define-advice org-edit-src-exit (:before (&rest _args) format-buffer)
  "Format source blocks before exit"
  (when (bound-and-true-p format-all-formatters)
    (format-all-buffer)))

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1)
  (add-to-list 'editorconfig-indentation-alist '(swift-mode swift-mode:basic-offset)))

(use-package apheleia
  :straight t
  :init
  (apheleia-global-mode +1)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'swiftformat apheleia-formatters)
        '("swiftformat"
          ;; Look for .swiftformat in parent directories
          "--config" (concat (locate-dominating-file default-directory ".swiftformat") ".swiftformat")
          "--stdinpath" filepath
          "stdin"))
  (setf (alist-get 'swift-mode apheleia-mode-alist)
        '(swiftformat))
  )

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

(use-package ledger-mode
  :straight t)

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
  (setq lsp-ui-doc-show-with-cursor t)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package magit
  :straight t
  :hook (git-commit-mode . (lambda () (setq fill-column 72)))
  :bind (("C-x v s" . magit-status)
         ("C-x v b" . magit-blame-addition))
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-local-branches 'magit-insert-stashes)
  (setq
   magit-last-seen-setup-instructions "1.4.0"
   magit-push-always-verify nil
   ;; Always on linux, never on Windows, due to slooow
   magit-diff-refine-hunk (if (eq system-type 'windows-nt) nil 'all)))

(use-package magit-delta
  :straight t
  :hook (magit-mode . magit-delta-mode))

(use-package markdown-indent-mode
  :straight (markdown-indent-mode :type git :host github :repo "whhone/markdown-indent-mode")
  :hook (markdown-mode . markdown-indent-mode))

(use-package delight
  :straight t
  ;; Hide auto-revert-mode
  :config (delight 'auto-revert-mode))

(use-package lsp-pyright
  :straight t
  ;; basedpyright is a pyright fork bundling stdlib/builtin docstrings, so
  ;; hover and completion show real docs. Set in :init because the client
  ;; registers at load time and reads this to pick the executable.
  ;; Multi-root shares one server across every project in the lsp session, so
  ;; one repo's config errors and deleted folders leak into all the others.
  ;; lsp-pyright sends typeCheckingMode "standard", which reports unused
  ;; names only as hints (flycheck info). Raise those to warnings.
  :init (setq lsp-pyright-langserver-command "basedpyright"
              lsp-pyright-multi-root nil
              lsp-pyright-diagnostic-severity-overrides
              '(("reportUnusedVariable" . "warning")
                ("reportUnusedImport" . "warning")))
  :hook (python-mode . my/python-lsp))

(defun my/python-lsp ()
  (require 'lsp-pyright)
  (direnv-update-environment)
  (when-let* ((dir (locate-dominating-file default-directory "pyproject.toml"))
              (root (lsp-f-canonical dir))
              ((not (member root (lsp-session-folders (lsp-session))))))
    (lsp-workspace-folders-add root))
  (lsp))

(use-package python-pytest
  :straight t
  :config
  ;; A project with a pyproject.toml per subdirectory (adventofcode has one per
  ;; year) needs pytest to run in that subdirectory, so rootdir and conftest
  ;; discovery resolve there. Projectile's root stays at the repo, which keeps
  ;; cross-directory search working.
  (define-advice python-pytest--project-root
      (:around (orig) asg/nearest-pyproject)
    (or (locate-dominating-file default-directory "pyproject.toml")
        (funcall orig)))

  ;; projectile-find-matching-test guesses a test path by swapping "src" for
  ;; "test" and never checks that it exists, so dwim runs pytest on a missing
  ;; file. Fall back to the visited file. The guess also needs re-rooting,
  ;; since projectile returns it relative to its own root.
  (define-advice python-pytest--sensible-test-file
      (:around (orig file) asg/only-existing)
    (let ((guess (and (not (python-pytest--test-file-p file))
                      (ignore-errors
                        (expand-file-name (funcall orig file)
                                          (projectile-project-root))))))
      (python-pytest--relative-file-name
       (if (and guess (file-exists-p guess)) guess file)))))

(defun asg/pytest-target ()
  "Absolute path of the tests covering the current buffer, or nil.
Either the buffer itself when it defines tests, or the test file
projectile associates with it."
  (let ((file (buffer-file-name)))
    (cond
     ((null file) nil)
     ((save-excursion (goto-char (point-min))
                      (re-search-forward "^[ \t]*def test" nil t))
      file)
     (t (let ((guess (ignore-errors
                       (expand-file-name (python-pytest--sensible-test-file file)
                                         (python-pytest--project-root)))))
          (and guess (file-exists-p guess) (not (equal guess file)) guess))))))

(with-eval-after-load 'flycheck
  (flycheck-define-checker pytest
    "Run the tests covering the current file with pytest."
    :command ("pytest" "--tb=line" "-q" "--no-header" "-p" "no:cacheprovider"
              (eval (asg/pytest-target)))
    :error-patterns
    ((error line-start (file-name) ":" line ": " (message) line-end)
     (error line-start "E" (one-or-more " ") "File \"" (file-name)
            "\", line " line line-end)
     (error line-start "FAILED " (one-or-more (not (any " "))) "::"
            (id (minimal-match (one-or-more not-newline))) " - " (message)
            line-end))
    ;; A traceback line points at where the exception was raised, which is
    ;; often not the test. pytest lists failures and its summary in the same
    ;; order, so the node ids from the summary zip onto the locations. Errors
    ;; belonging to another file are moved to line 1, since flycheck discards
    ;; them otherwise: a failing assertion in tests/ would leave src/ green.
    :error-filter
    (lambda (errors)
      (let* ((this (buffer-file-name))
             (named (seq-filter #'flycheck-error-id errors))
             (sites (seq-remove #'flycheck-error-id errors))
             (paired (= (length named) (length sites))))
        (when paired
          (cl-loop for site in sites for name in named do
                   (setf (flycheck-error-message site)
                         (format "%s: %s" (flycheck-error-id name)
                                 (flycheck-error-message site)))))
        (dolist (err sites)
          (unless (flycheck-error-message err)
            (setf (flycheck-error-message err)
                  "pytest could not import this file"))
          (unless (equal (flycheck-error-filename err) this)
            (setf (flycheck-error-message err)
                  (format "%s (%s:%s)"
                          (flycheck-error-message err)
                          (file-name-nondirectory
                           (or (flycheck-error-filename err) "?"))
                          (flycheck-error-line err))
                  (flycheck-error-filename err) this
                  (flycheck-error-line err) 1)))
        (flycheck-sanitize-errors
         (flycheck-fill-empty-line-numbers (if paired sites errors)))))
    ;; Only on a saved buffer: pytest reads the file from disk. Requiring a
    ;; target also keeps pytest from exiting 5 on a file with no tests, which
    ;; flycheck would report as a broken checker.
    :predicate (lambda () (and (not (buffer-modified-p)) (asg/pytest-target)))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'pytest t))

;; lsp-mode claims flycheck-checker in python buffers, so the checker only runs
;; when chained. The lsp checker is defined lazily, hence the explicit call.
(with-eval-after-load 'lsp-diagnostics
  (lsp-diagnostics-lsp-checker-if-needed)
  (flycheck-add-next-checker 'lsp '(t . pytest)))

(use-package rainbow-delimiters
  :straight t
  :hook ((python-mode python-ts-mode csharp-mode typescript-mode clojure-mode javascript-mode objc-mode swift-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :straight t
  :hook ((python-mode python-ts-mode csharp-mode typescript-mode clojure-mode javascript-mode objc-mode swift-mode) . rainbow-identifiers-mode))

(use-package sdcv-mode
  :straight (:host github :repo "gucong/emacs-sdcv" :files ("*.el"))
  :hook (sdcv-mode . (outline-show-all))
  :bind (("C-c i" . sdcv-search)))

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

(use-package moody
  :straight t
  :after (modus-themes)
  :config
  (defun my/moody-unbox (&rest _)
    (set-face-attribute 'mode-line-active nil :box 'unspecified)
    (set-face-attribute 'mode-line-inactive nil :box 'unspecified))
  ;; This block runs when modus-themes is required, before load-theme
  ;; applies the theme's faces, so the boxes have to go after that.
  (add-hook 'enable-theme-functions #'my/moody-unbox)
  (my/moody-unbox)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package modus-themes
  :straight t
  :init (load-theme 'modus-vivendi))

(use-package diff-hl
  :straight t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode))

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

(use-package copilot
  :straight (copilot
             :type git :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook ((prog-mode . copilot-mode))
  :bind (("C-<tab>" . copilot-accept-completion)
         ("C-S-<tab>" . copilot-accept-completion-by-line)))

(use-package treesit-auto
  :straight t
  :config
  ;; (global-treesit-auto-mode) Disable for now - too many perf issues
  (setq treesit-auto-install t))

(use-package csharp-mode
  :hook (csharp-mode . lsp-deferred)
  :config
  (setq-local company-backends '(company-dabbrev-code company-keywords)))

(setq lsp-roslyn-package-version "5.4.0-2.26179.14"
      lsp-roslyn-dotnet-executable (expand-file-name "~/.dotnet/dotnet")
      ;; Information level logs a line to stdout ahead of the pipe-name JSON,
      ;; and lsp-roslyn fails to parse it.
      lsp-roslyn-server-log-level "Warning")
(setenv "DOTNET_ROOT" (expand-file-name "~/.dotnet"))
;; Sydney's native macOS Bond compiler, so design-time builds generate Bond types.
(when (file-executable-p (expand-file-name "~/.local/bin/gbc"))
  (setenv "BOND_COMPILER_PATH" (expand-file-name "~/.local/bin")))

(with-eval-after-load 'lsp-roslyn
  ;; lsp-roslyn only looks for .sln files, so it never offers .slnx solutions.
  (defun my/lsp-roslyn-find-solution-file ()
    (let ((solutions (lsp-roslyn--find-files-in-parent-directories
                      (file-name-directory (buffer-file-name))
                      (rx ".sln" (? "x") eos))))
      (if (cdr solutions)
          (lsp-roslyn--pick-solution-file-interactively solutions)
        (car solutions))))
  (advice-add 'lsp-roslyn--find-solution-file :override #'my/lsp-roslyn-find-solution-file)
  ;; lsp-mode runs this hook from a response callback in whatever buffer is
  ;; current, so the solution lookup and the solution/open notify need the
  ;; workspace's own buffer.
  (defun my/lsp-roslyn-on-initialized (workspace)
    (lsp-with-current-buffer (car (lsp--workspace-buffers workspace))
      (with-lsp-workspace workspace
        (lsp-roslyn-open-solution-file))))
  (advice-add 'lsp-roslyn--on-initialized :override #'my/lsp-roslyn-on-initialized))

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

;(use-package indium
;  :straight t)

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

(use-package lsp-sourcekit
  :straight t
  :after lsp-mode
  :hook (swift-mode . (lambda () (lsp)))
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

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

;; Monorepo hack. This is not nice, but eslint needs to be told on the
;; command line when plugins are provided by an external
;; package. Which tends to be the case on monorepos with a shared
;; linter config.
(defun sverrejoh-configure-eslint ()
  (when-let* ((root (projectile-project-root))
              (eslint-resolve-from (concat root (getenv "ESLINT_CONFIG_PKG"))))
    (setq-local flycheck-eslint-args `("--resolve-plugins-relative-to" ,eslint-resolve-from))))

;; Chain eslint checker to LSP checker _when we're in
;; typescript-mode_. This assumes that we're monkeypatching flycheck
;; to read this variable back at the appropriate time.
(defvar-local flycheck-checker-local-override nil)
(defun set-flycheck-checker-to-lsp-typescript ()
  (when (derived-mode-p 'typescript-mode)
    (setq flycheck-checker-local-override '((lsp . ((next-checkers . (javascript-eslint))))))))

(use-package typescript-mode
  :straight t
  :after flycheck
  :hook ((typescript-mode . sverrejoh-configure-eslint)
         (typescript-mode . use-eslint-from-node-modules)
         (typescript-mode . flyspell-prog-mode)
         (lsp-managed-mode . set-flycheck-checker-to-lsp-typescript))
  :config
  ;; Ensure V8 has enough memory to load big projects into tsserver
  (setq lsp-clients-typescript-max-ts-server-memory 16384
        lsp-clients-typescript-prefer-use-project-ts-server t)
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

(defun my/copy-image-to-clipboard ()
  "Copy the current image to the macOS clipboard."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (derived-mode-p 'image-mode))
        (progn
          (shell-command (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as TIFF picture)'" file))
          (message "Image copied to clipboard"))
      (message "Not visiting an image file"))))

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "C-c C-v") #'my/copy-image-to-clipboard))

(use-package which-key
  :delight
  :init
  (which-key-mode))

(custom-set-variables
 '(custom-safe-themes
   '("d067a9ec4b417a71fbbe6c7017d5b7c8b961f4b1fc495cd9fbb14b6f01cca584" default)))
