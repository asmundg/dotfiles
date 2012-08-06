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

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
;;(add-hook 'javascript-mode-hook 'flymake-mode-on)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;;(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(provide 'custom-js)
