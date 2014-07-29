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
                  ido-ubiquitous
;;                  jedi
                  js2-mode
                  magit
                  magit-commit-training-wheels
                  magit-filenotify
                  magit-find-file
                  markdown-mode
                  mo-git-blame
                  slime
                  smex
                  virtualenvwrapper
                  yaml-mode)))
     (dolist (pkg packages)
       (when (not (package-installed-p pkg))
         (package-refresh-contents)
         (package-install pkg))))


(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(global-set-key (kbd "M-x") 'smex)

(setq help-at-pt-timer-delay 0.1)
(setq help-at-pt-display-when-idle '(flymake-overlay))

(require 'auto-complete)
(global-auto-complete-mode)

(provide 'custom-elpa)
;;; custom-elpa.el ends here
