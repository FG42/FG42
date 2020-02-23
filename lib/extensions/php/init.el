;; Functions ----------------------------------

(defun extensions/php-initialize ()
  "PHP extensions initialization function"
  (add-hook 'php-mode-hook (lambda () (lsp)))
  )

(provide 'extensions/php/init)
