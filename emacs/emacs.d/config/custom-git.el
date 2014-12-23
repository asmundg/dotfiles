(global-set-key (kbd "C-x v s") 'magit-status)
;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-background 'magit-diff-add "black")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-diff-del "black")
     (defun magit-highlight-section ())
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))


(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(global-set-key (kbd "C-x v b") 'mo-git-blame-current)

(provide 'custom-git)
