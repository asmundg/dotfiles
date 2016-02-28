;;; config-appearance.el -- Color and font configuration

;;; Commentary:

;;; Code:

(set-face-attribute 'default nil :height 80)

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1)
  )

(global-set-key (kbd "M-0")
                '(lambda () (interactive)
                   (global-text-scale-adjust (- text-scale-mode-amount))
                   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
                '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
                '(lambda () (interactive) (global-text-scale-adjust -1)))


(load-theme 'solarized-dark)
(setq sml/theme 'respectful)
(sml/setup)


(set-frame-font
 (if (eq system-type 'windows-nt)
     "-outline-Consolas-normal-r-normal-normal-11-97-96-96-c-*-iso8859-1"
   "-outline-Terminus-normal-r-normal-normal-11-97-96-96-c-*-iso8859-1"))

(provide 'config-appearance)
;;; config-appearance.el ends here
