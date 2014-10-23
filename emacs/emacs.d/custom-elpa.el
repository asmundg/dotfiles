;;; custom-elpa.el -- ELPA packages (and some configuration)
;;; Commentary:

;;; Code:
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(let ((packages '(auto-complete
                  autopair
                  coffee-mode
                  color-theme
                  find-file-in-project
                  flycheck
                  helm
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
                  mo-git-blame
                  slime
                  virtualenvwrapper
                  yaml-mode
                  hc-zenburn-theme)))
     (dolist (pkg packages)
       (when (not (package-installed-p pkg))
         (package-refresh-contents)
         (package-install pkg))))


(require 'helm-config)
(helm-mode 1)

(setq
  helm-quick-update                     t ; do not display invisible candidates
  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
  helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
  helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-x g") 'helm-git-grep-at-point)

(defvar help-at-pt-timer-delay 0.1)
(defvar help-at-pt-display-when-idle '(flymake-overlay))

(require 'auto-complete)
(global-auto-complete-mode)

(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-to-char)

(provide 'custom-elpa)
;;; custom-elpa.el ends here
