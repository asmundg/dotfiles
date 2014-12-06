;;; custom-js.el -- Javascript config
;;; Commentary:

;; (when (load "flymake" t)
;;   (defun flymake-gjslint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace)))
;;       (list "gjslint" (list temp-file "--nosummary --jslint_error all --strict"))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '(".+\\.js$"
;;                  flymake-gjslint-init
;;                  flymake-simple-cleanup
;;                  flymake-get-real-file-name))

;;   (add-to-list 'flymake-err-line-patterns
;;                '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: "
;;                  nil 1 nil)))

;;; Code:
(require 'nvm)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
;;(add-hook 'javascript-mode-hook 'flymake-mode-on)

(defun load-nvm ()
  "Initialize nvm."
  (nvm-use "0.11")
  (exec-path-from-shell-copy-env "PATH"))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(setq coffee-mode-hook nil)
(add-hook 'coffee-mode-hook 'auto-complete-mode)
(add-hook 'coffee-mode-hook 'load-nvm)
(add-hook 'coffee-mode-hook 'flycheck-mode)

(provide 'custom-js)
;;; custom-js.el ends here
