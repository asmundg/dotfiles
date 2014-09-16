;;; custom-python.el --- Python config
;;; Commentary:

;;; Code:
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(defun project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))

(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (require 'virtualenvwrapper)
  (let ((project-name (project-name buffer-file-name)))
    (when project-name (venv-workon project-name))))

(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location (expand-file-name "~/.virtualenvs"))
(setq gud-pdb-command-name "python -m pdb")

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi-setup-venv)
(setq jedi:complete-on-dot t)

(provide 'custom-python)
;;; custom-python.el ends here
