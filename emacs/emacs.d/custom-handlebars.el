(require 'handlebars-sgml-mode)
(add-to-list 'auto-mode-alist '("\\.hbs$" . sgml-mode))
(handlebars-use-mode 'global)

(provide 'custom-handlebars)
