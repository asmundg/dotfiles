;;; custom-elpa.el -- ELPA packages (and some configuration)
;;; Commentary:

;;; Code:
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(let ((packages '(ac-cider
                  auto-complete
                  cider
                  clojure-mode
                  coffee-mode
                  color-theme
                  company
                  csharp-mode
                  exec-path-from-shell
                  find-file-in-project
                  flycheck
                  flycheck-clojure
                  flycheck-pos-tip
                  helm
                  helm-dash
                  helm-ls-git
                  helm-git-grep
                  iy-go-to-char
                  jedi
                  jinja2-mode
                  js2-mode
                  json-mode
                  less-css-mode
                  lua-mode
                  magit
                  magit-filenotify
                  magit-find-file
                  markdown-mode
                  minimap
                  mo-git-blame
                  nvm
                  omnisharp
                  password-store
                  powershell
                  python-mode
                  rainbow-delimiters
                  rainbow-identifiers
                  slime
                  smart-mode-line
                  smartparens
                  solarized-theme
                  virtualenvwrapper
                  web-mode
                  yaml-mode)))
     (dolist (pkg packages)
       (when (not (package-installed-p pkg))
         (package-refresh-contents)
         (package-install pkg))))


(require 'helm-config)
(require 'helm-ls-git)
(helm-mode 1)

(setq
  helm-quick-update                     t ; do not display invisible candidates
  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
  helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
  helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-x g") 'helm-git-grep-at-point)
(global-set-key (kbd "C-x C-g") 'helm-browse-project)

(defvar help-at-pt-timer-delay 0.1)
(defvar help-at-pt-display-when-idle '(flymake-overlay))

(require 'auto-complete)
(global-auto-complete-mode)

(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-to-char)

; Clojure
;; require or autoload smartparens
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-identifiers-mode)

(add-hook 'after-init-hook 'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (flycheck-clojure-setup)
  (setq flycheck-display-errors-delay 0.1))

; Smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(global-set-key (kbd "M-(") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
(global-set-key (kbd "C-M-)") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-M-(") 'sp-forward-barf-sexp)

(provide 'custom-elpa)
;;; custom-elpa.el ends here
