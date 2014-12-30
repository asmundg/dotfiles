;;; custom-js.el -- Javascript config
;;; Commentary:

;;; Code:
(require 'nvm)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(defun load-nvm ()
  "Initialize nvm."
  (nvm-use "0.11")
  (exec-path-from-shell-copy-env "PATH"))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode))
(setq coffee-mode-hook nil)
(add-hook 'coffee-mode-hook 'auto-complete-mode)
(add-hook 'coffee-mode-hook 'load-nvm)
(add-hook 'coffee-mode-hook 'flycheck-mode)

(provide 'custom-js)
;;; custom-js.el ends here
