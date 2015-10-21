;;; custom-git.el -- Git config
;;; Commentary:

;;; Code:

(global-set-key (kbd "C-x v s") 'magit-status)
;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (defun magit-highlight-section ())
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))


(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(global-set-key (kbd "C-x v b") 'mo-git-blame-current)

(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'custom-git)
;;; custom-git.el ends here
