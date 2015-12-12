(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(let ((packages '(flymake-haskell-multi
                  haskell-mode)))
     (dolist (pkg packages)
       (when (not (package-installed-p pkg))
         (package-refresh-contents)
         (package-install pkg))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(provide 'custom-haskell)
