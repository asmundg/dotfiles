;;; custom-python.el --- Python config
;;; Commentary:

;;; Code:
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location (expand-file-name "~/.virtualenvs"))
(setq gud-pdb-command-name "python -m pdb")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(provide 'custom-python)
;;; custom-python.el ends here
