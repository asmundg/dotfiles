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
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defun web-mode-config ()
  "Webhook mode identation."
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))

(add-hook 'web-mode-hook 'web-mode-config)

(add-hook 'coffee-mode-hook 'auto-complete-mode)
(add-hook 'coffee-mode-hook 'load-nvm)

(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(provide 'custom-js)
;;; custom-js.el ends here
