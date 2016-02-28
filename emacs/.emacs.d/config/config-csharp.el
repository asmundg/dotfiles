;;; config-csharp.el -- C# config
;;; Commentary:

;;; Code:
(defvar company-backends)

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(provide 'config-csharp)
;;; config-csharp.el ends here
